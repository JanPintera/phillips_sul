library(readr)
library(dplyr)
library(eurostat)
library(spdep)
library(rgdal)
library(giscoR)
library(foreach)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(arrow)

source("functions.R")
source("helper.R")

#################################### Get the Eurostat Data ##############################
#### REMARK: these files are downloaded in order to have access to their historical versions,
#### for the real analysis I will use the get_eurostat function below.
data <- read_csv("nama_10r_2gdp/nama_10r_2gdp_1_Data.csv")
# TODO: we don't have UK anymore, and at the same time we have other

## NUTS3 level - possibly for comparison NUTS 3 level
data_nuts3 <- read_csv("nama_10r_3gdp/nama_10r_3gdp_1_Data.csv")

## Get old data - we have missing french regions in the old file
data_old <- read_csv("nama_10r_2gdp_old/nama_10r_2gdp_1_Data.csv")

## The UK data
uk_nuts <- gisco_get_nuts(year = "2016", epsg = "4326", cache = TRUE, update_cache = FALSE,
                          cache_dir = NULL, verbose = FALSE, resolution = "60", spatialtype = "RG", country = NULL, nuts_id = NULL, nuts_level = "2") %>%
                          filter(substr(NUTS_ID, start = 1, stop = 2) == "UK")

data_uk <- read_csv("uk_regions/ons_uk_regions_gdp_current_prices_per_cap.csv") %>% 
                    filter(ITL == "ITL2") %>%
                    left_join(uk_nuts, c('Region name' = 'NAME_LATN')) %>%
                    rename(itl_code = "ITL code", 'region_name' = 'Region name', "2019" = "2019\n[note 3]") %>%
                    mutate(NUTS_ID = replace(NUTS_ID, itl_code == "TLK1", "UKK1")) %>% # the names of Gloucestershire are somewhat different in both sources
                    select("NUTS_ID", "itl_code", "region_name", starts_with("20"))

        

# PPS are derived by dividing any economic aggregate of a country in national currency by its respective purchasing power parities.
# PPPs can be interpreted as the exchange rates of countries' national currencies against the PPS. They express the number of currency units per PPS.
ppp <- get_eurostat("prc_ppp_ind", time_format = "num", filters = list(geo = "UK", 
                                                                       ppp_cat = "GDP",
                                                                       na_item = "PPP_EU27_2020")) 



uk_pps <- data_uk %>% pivot_longer(cols = starts_with("20"), names_to="time", values_to="values") %>%
                 mutate(time = as.numeric(time)) %>%
                 inner_join(ppp, c("time"), suffix = c("_gbp", "_ppp")) %>%
                 mutate(value_pps = values_gbp / values_ppp) # For claritity a call it what it is and rename it later - the pps is just "Value" in the Eurostat dfs

## Let's use my original naming. It's now trivial but I expect some merging of the dataset above in the future
## in order to have the complete dataset
orig_data <- data %>% rename(time = TIME,
                             code = GEO,
                             value = Value) %>% 
                      mutate(value = as.numeric(gsub(",", "", value))) %>%
                      filter(nchar(code) == 4 & substr(code, start = 1, stop = 2) %in% eu_countries)


## Getting it directly from Eurostat - the GDP is given only in relative form - I have to use the "orig_data", but it still provides nice mapping between NUTS13 and NUTS16
tmp <- get_eurostat_toc()
data_online <- get_eurostat("tgs00005", time_format = "num", cache = FALSE) %>% # (see tmp above) = Regional gross domestic product (PPS per inhabitant) by NUTS 2 regions
               harmonize_geo_code() # This harmonizes different versions of NUTS labels and also add names 

## Joining the "offline" and "online" stuff, for some stupid reason "tgs00005" is downloaded only in the relative version
pps_nuts2 = data %>% select("TIME", "GEO", "Value") %>% inner_join(data_online, c('GEO' = 'code16', 'TIME' = 'time'))
  
## Check NUTS2013 and NUTS2016 compatibility
# Result - apart from the french regions all the regional gdps seem to be calculated back to the past in their NUTS2016 definitions
changed_nuts <- pps_nuts2 %>% filter(change != 'unchanged' | GEO == "FR10") %>% rename(code16 = GEO) %>% select(code16, code13, name, resolution) %>% unique()

# Adding the French regions from the old files:
old_french <- data_old %>% left_join(changed_nuts, c('GEO' = 'code13'))  %>%
                           filter(!is.na(code16) & substr(GEO, start = 1, stop = 2) == "FR") %>%
                           rename(value_old = Value) %>%
                           select(code16, TIME, value_old)

## FINAL JOINS happens here!
complete_data <- orig_data %>% left_join(old_french, c("code" = "code16", 'time' = 'TIME')) %>%
             filter(!(code %in% c("FRY3", "FRY4", "FRY5", "PT20"))) %>% # Small territories outside of continental Europe for which we get NaNs for the Getis-Ord spatial operation below
             bind_rows((uk_pps %>% rename(value = value_pps, # append the uk data
                                          code = NUTS_ID,
                                          GEO_LABEL = region_name)) %>% select(time, code, GEO_LABEL, value)) %>%
            mutate(value_complete = coalesce(value, as.numeric(gsub(",", "", value_old))))

################################### Getis- Ord statistics ###################################
EU_NUTS <- gisco_get_nuts(year = "2016", epsg = "4326", cache = TRUE, update_cache = FALSE,
              cache_dir = NULL, verbose = FALSE, resolution = "60", spatialtype = "RG", country = NULL, nuts_id = NULL, nuts_level = "2") %>%
              filter(substr(NUTS_ID, start = 1, stop = 2) %in% eu_countries) %>%
              #filter(substr(NUTS_ID, start = 1, stop = 2) != "UK") %>% #TODO - add UK and delete!
              filter(!(NUTS_ID %in% c("FRY3", "FRY4", "FRY5", "PT20"))) # Small territories outside of continental Europe for which we get NaNs for the Getis-Ord spatial operation below
  

row.names(EU_NUTS) <- EU_NUTS$NUTS_ID
# want SpatialPolygonDataFrame z toho
Geo_nuts2 <- as(EU_NUTS, "Spatial")

com<-intersect(unique(Geo_nuts2@data$NUTS_ID), unique(complete_data$code)) # This helps us to get rid of the "Extra regions"
# setdiff(unique(Geo_nuts2@data$NUTS_ID), unique(complete_data$code)) # TODO: note we are missing UK, add it!

#### Getis- Ord statistics:

## Moran test  - FINISH!
geo_nb<-poly2nb(Geo_nuts2)
nb<-nb2listw(geo_nb, zero.policy=TRUE, style="B")
moran.test(dat$values, nb, zero.policy=TRUE, randomisation = FALSE)


# Using previously defined function (functions.R) for all possible years, for years 2003-2019, the others have missing in them

logy_star <- list(0)
for(i in 1:(length(unique(complete_data$time))-3)){
    dat <- complete_data %>% filter(time == 2002+i & code %in% com) %>%
           mutate(values = log(value_complete))
    row.names(dat) <- dat$code
    logy_star[[i]] <- data.frame(Year=2002+i, GEO=com, y_star=NA)
    logy_star[[i]][,3] <- y_star(dat)
}
income_data <- logy_star[[1]]
for(i in 2:17){
  income_data<- rbind(income_data, logy_star[[i]])  
}

#################################### Phillips and Sul algorithm ###################################################
log_y_test <- income_data %>% rename(TIME = Year, Value = y_star)

#### Ordering according to last period income
log_y_test$Last<- NA
for(i in unique(log_y_test$GEO))
{
  log_y_test[which(log_y_test$GEO == i), "Last"] <- log_y_test[which(log_y_test$GEO == i & log_y_test$TIME == 2019), "Value"] #  2014 for the non-country data!!!
}
log_y_test <- log_y_test[order(log_y_test$Last, decreasing = T),]
log_y_test <- log_y_test[order(log_y_test$TIME),]
# the last year observation order
ordering <- log_y_test[which(log_y_test$TIME == 2019), "GEO"] 

#### Core group formation
h_t<-list(0)
for(i in 2:(length(ordering)-1))
{
  names <- log_y_test[seq(from=1,to=i,by=1),2]
  h_t[[i]]<- 
    foreach(b=1:i, .combine = 'cbind')%:%
    foreach(a=unique(log_y_test$TIME), .combine='c') %do% {
      h(a, b, names)
    }
}

#### doing the initial t-test regression
t_stat <- log_t_test(ordering)

k_star = core(t_stat) # get "Drop the highest and repeat" (t-test for 1st two is -35.89) -> I drop UKI3

outlier_gr1 <- ordering[1]
remaining <- ordering[-1]
t_stat <- log_t_test(remaining) # I reuse this var name across the core group formation in order not to overwhelm the namespace
k_star = core(t_stat)

# k_star = 7, which means that core group is formed by six regions these are:
remaining[1:k_star] # "LU00" "IE05" "CZ01" "BE10" "DE60" "IE06" "RO32"
# this group is clearly converging with t value of the t-test 0.9389539 and coefficient 0.2135305

##################### Adding one country at a time to the core group: ######################
# The procedure here is - I add one country each iteration to the k_* members of the core group, then add all the units
# that qualifies using the c_* criteria and run log t test on this whole group again testing whether t_stat > -1.65
# Problem is a choice of c_*, Phillips and Sul do not suggest anything specific, only values from -1.65 to 0
# they remark that c_*=0 is extremely conservative and leads to higher number of clubs than is the true number
# therefore they suggest procedure of merging at the very end the resulting clubs together

tc_stat <- log_t_test_c(k_star, remaining)
#### Group 1:
gr1_ind <- c(1:k_star, which(tc_stat>0))
names_gr1<- remaining[gr1_ind]

print(log_t_test_core(names_gr1[1:10]))
# As the group defined by the `log_t_test_c` procedure does not convergence with t value of the test is -4.5643058 and coefficient -0.2965755 suggesting conditional
# I try to keep only the core as Group 1, plus 4 regions that seem to be diverging from the group 2 when I try to include them:
print(log_t_test_core(names_gr1[1:10])) # and this core group converges: t value of the test: 0.11267325, coeff.: 0.01935022
#gr1_ind = 1:k_star # possibility to take only the code
gr1_ind = 1:10 # this is chosen somewhat arbitrarily, 
names_gr1<- remaining[gr1_ind]

######################## Second convergence group formation: ##########################
# First of all I perform a test of overall convergence in this remaining group:
remaining2 <- remaining[-gr1_ind]
print(log_t_test_core(remaining2)) 
# We receive t value of -15.5208671 and coefficient -0.6815267 -> the null of overall convergence in this remaining group

# Forming second core group:
t_stat2 <- log_t_test(remaining2)
k_star2 <- core(t_stat2) # "The core group has k^* equal to  12"

# Adding one region a time...
tc_stat2 <- log_t_test_c(k_star2, remaining2)

gr2_ind <- c(1:k_star2, which(tc_stat2>0))
names_gr2<- remaining2[gr2_ind]

print(log_t_test_core(names_gr2)) # Group 2 converges with t-stat 0.9303793 and coeff. 0.1460817

################################# Third convergence group formation #####################################
remaining3<- remaining2[-gr2_ind]

print(log_t_test_core(remaining3))
# The whole remaining group is found non-convergent t-stat -11.0929885, coeff: -0.5549737

# Forming a third core group:
t_stat3 <- log_t_test(remaining3)
k_star3 <- core(t_stat3) # I get "Drop the highest and repeat"

# I have a new outlier
outlier_gr3 <- remaining3[1]
remaining3 <- remaining3[-1]

t_stat3 <- log_t_test(remaining3)
k_star3 <- core(t_stat3) # "The core group has k^* equal to  3"

tc_stat3 <- log_t_test_c(k_star3, remaining3)

gr3_ind <- c(1:k_star3, which(tc_stat3>0))
names_gr3<- remaining3[gr3_ind]

print(log_t_test_core(names_gr3)) # Group 3 converges with t-stat 1.9505306, coeff.: 0.1678917

################################# Forth convergence group formation #####################################
remaining4 <- remaining3[-gr3_ind]

print(log_t_test_core(remaining4))
# The whole remaining group is found non-convergent t-stat -8.579337 -0.472681

# Forming a forth core group:
t_stat4 <- log_t_test(remaining4)
k_star4 <- core(t_stat4) # I get "Drop the highest and repeat"

tc_stat4 <- log_t_test_c(k_star4, remaining4)

gr4_ind <- c(1:k_star4, which(tc_stat4>0))
names_gr4<- remaining4[gr4_ind]

print(log_t_test_core(names_gr4)) # Group 4 converges with t-stat 0.9868163, coeff.: 0.1012206



################################# Fifth convergence group formation #####################################
remaining5 <- remaining4[-gr4_ind]

print(log_t_test_core(remaining5))
# The whole remaining group is found non-convergent -6.9056644 -0.4239206

t_stat5 <- log_t_test(remaining5)
k_star5 <- core(t_stat5) # "The core group has k^* equal to  17"

tc_stat5 <- log_t_test_c(k_star5, remaining5)

gr5_ind <- c(1:k_star5, which(tc_stat5>0))
names_gr5<- remaining5[gr5_ind]

print(log_t_test_core(names_gr5)) # Group 5 converges with t-stat 0.7458122 0.0919395

################################# Sixth convergence group formation #####################################
remaining6 <- remaining5[-gr5_ind]

print(log_t_test_core(remaining6))
# The whole remaining group is found non-convergent -3.4922516, coeff: -0.2529671

t_stat6 <- log_t_test(remaining6)
k_star6 <- core(t_stat6)

tc_stat6 <- log_t_test_c(k_star6, remaining6)

gr6_ind <- c(1:k_star6, which(tc_stat6>0))
names_gr6<- remaining6[gr6_ind]

print(log_t_test_core(names_gr6)) # Group 6 converges with t-stat 1.5301293, coeff: 0.1628613

################################# Seventh convergence group formation #####################################
remaining7 <- remaining6[-gr6_ind]

print(log_t_test_core(remaining7)) # The whole remaining group is found convergent -0.96806120 -0.09627829

names_gr7 <- remaining7

################################# Checking and Merging ##############################################
# Overall, we have 7 convergence groups and two outliers: UKI3 and BE21

names_list<- list(names_gr1, names_gr2, names_gr3, names_gr4, names_gr5, names_gr6, names_gr7)
anyDuplicated(unlist(names_list))

# "BE21" - is an outlier from the second group -> I attempt merging it with the second convergence club
names_gr2_plus <- c(names_gr2, outlier_gr3)

log_t_test_core(names_gr2_plus) # the group converges with t-stat 0.7101582 and coefficient 0.1084816

upd_names_gr2 <- c(names_gr2, outlier_gr3)

# Basic result check:
names_list<- list(names_gr1, upd_names_gr2, names_gr3, names_gr4, names_gr5, names_gr6, names_gr7)
print(length(unlist(names_list))) # = 276, i.e. the original 
anyDuplicated(unlist(names_list))

#### Checking mergers of the Convergence clubs:

m_stat<- c(0)
for (i in 1:(length(names_list)-1)){
  m_stat[i] <- log_t_test_core(unlist(c(names_list[i], names_list[i+1])))[1]
}

print(m_stat) # Based on the m-stat we cannot merge the first two clubs, but we can merge 3, 4, 5, 6
# -> merge 3 and 4 and continue
#log_t_test_core(c(names_gr3, names_gr4))

final_names_gr3 <- c(names_gr3, names_gr4)

# Adding group 5:
log_t_test_core(c(final_names_gr3, names_gr5)) # t-stat -1.4047701 and coefficient: -0.1233501
# This is really tight, but we add group 5 there

final_names_gr3 <- c(final_names_gr3, names_gr5)

# Adding group 6:
log_t_test_core(c(final_names_gr3, names_gr6)) # no convergence: t-stat: -3.8173834


# Do last two groups form a club?
log_t_test_core(c(names_gr6, names_gr7)) # No, t-stat: -3.4922516 

## Final results are thus:
# UKI3 as an outlier, "BE21" is merged with the second group and groups 3, 4 and 5 are merged into one:
convergence_clubs<- list(outlier_gr1, names_gr1, upd_names_gr2, final_names_gr3, names_gr6, names_gr7)

# Check if all regions are included:
length(unlist(convergence_clubs)) == length(ordering)

############################ Transition Paths #################################################
# Enabling eventual visualization of the transition paths, by first calculation the h_t for each convergence club
merged_ht <- get_transition_path(convergence_clubs)

overall_ht<- foreach(b=c(1:length(ordering)), .combine = 'cbind')%:% 
  foreach(a=unique(log_y_test$TIME), .combine='c') %do% {
    h(a, b, ordering)
}

clubweight <- as_tibble(do.call(cbind, merged_ht))
#NOTE: the tail is there to get rid of the outlier
colnames(clubweight) <- tail(unlist(convergence_clubs), n=length(unlist(convergence_clubs))-1) #TODO: double check the ordering, clubweight df should keep ordering the convergence_clubs

############################ Plotting the clubs ###############################################
Geo_plot<- Geo_nuts2[!Geo_nuts2$NUTS_ID %in% c("FRY1","FRY2","FRY3","ES70","PT20","PT30"),]
# TODO: investigate the projection string:
Geo_plot <- spTransform(Geo_plot, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
Geo_club5<- tibble(Geo_plot@data) %>% add_column(Club = NA) %>% mutate(Club = as.integer(Club))
for(i in 1:6){
  Geo_club5[Geo_club5$NUTS_ID %in% convergence_clubs[[i]], ]$Club <- i-1
}

## ggplot https://rpubs.com/m_dev/Intro-to-Spatial-Data-and-ggplot2
Geo_f<-fortify(Geo_plot, region="NUTS_ID")
#geo_club5_melt<- melt(Geo_club5[, c(1,5)], id=c("NUTS_ID"))
Geo_ggplot<- merge(Geo_f, Geo_club5, by.x="id", by.y="NUTS_ID")
#Geo_ggplot <- Geo_ggplot[order(Geo_ggplot$order),] 


ggplot(data=Geo_ggplot, aes(long, lat, fill= Club, group = group
))+geom_polygon()+coord_equal()+  theme(line = element_blank(), 
                                        axis.text=element_blank(),
                                        axis.title=element_blank(),
                                        panel.background = element_blank())+
  scale_fill_gradientn("Club:",colours= rainbow(6,start=.40, end=.63),guide = "legend")


###################### Write-outs ##########################
write_parquet(income_data, "data-processed/income_data.parquet")
write_parquet(income_data, "data-processed/convergence_clubs.parquet")

