library(readr)
library(dplyr)
library(eurostat)
library(spdep)
library(rgdal)
library(giscoR)
library(foreach)
library(tidyr)

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



uk_pps <- data_uk %>% pivot_longer(cols = starts_with("20"), names_to="year", values_to="values") %>%
                 mutate(year = as.numeric(year)) %>%
                 inner_join(ppp, c("year" = "time"), suffix = c("_gbp", "_ppp")) %>%
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
             mutate(value_complete = coalesce(value, as.numeric(gsub(",", "", value_old)))) %>%
             filter(!(code %in% c("FRY3", "FRY4", "FRY5", "PT20"))) # Small territories outside of continental Europe for which we get NaNs for the Getis-Ord spatial operation below

################################### Getis- Ord statistics ###################################
EU_NUTS <- gisco_get_nuts(year = "2016", epsg = "4326", cache = TRUE, update_cache = FALSE,
              cache_dir = NULL, verbose = FALSE, resolution = "60", spatialtype = "RG", country = NULL, nuts_id = NULL, nuts_level = "2") %>%
              filter(substr(NUTS_ID, start = 1, stop = 2) %in% eu_countries) %>%
              filter(substr(NUTS_ID, start = 1, stop = 2) != "UK") %>% #TODO - add UK and delete!
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
      h(a,b)
    }
}

#### doing the initional t-test regression
t_stat <- log_t_test(ordering)

k_star = core(t_stat) # "The core group has k^* equal to  2"
ordering[1:k_star] # "LU00" "IE05"


##################### Adding one country at a time to the core group: ######################
# The procedure here is - I add one country each iteration to the k_* members of the core group, then add all the units
# that qualifies using the c_* criteria and run log t test on this whole group again testing whether t_stat > -1.65
# Problem is a choice of c_*, Phillips and Sul do not suggest anything specific, only values from -1.65 to 0
# they remark that c_*=0 is extremely conservative and leads to higher number of clubs than is the true number
# therefore they suggest procedure of merging at the very end the resulting clubs together

tc_stat <- log_t_test_c(k_star, ordering)
#### Group 1:
gr1_ind <- c(1:k_star, which(tc_stat>0))
names_gr1<- ordering[gr1_ind]

print(log_t_test_core(names_gr1))
# As the whole group we see convergence t value of the test is -0.059 and coefficient -0.0293 suggesting conditional


######################## Second convergence group formation: ##########################
# First of all I perform a test of overall convergence in this remaining group:
remaining<- ordering[-gr1_ind]
print(log_t_test_core(remaining)) 
# We receive t value of -23.5141287 and coefficient -0.8625014 -> the null of overall convergence in this remaining group

# Forming second core group:
t_stat2 <- log_t_test(remaining)
k_star2 <- core(t_stat2) # "The core group has k^* equal to  11"

# Adding one region a time...
tc_stat2 <- log_t_test_c(k_star2, remaining)

gr2_ind <- c(1:k_star2, which(tc_stat2>0))
names_gr2<- remaining[gr2_ind]

print(log_t_test_core(names_gr2))

################################# Third convergence group formation #####################################
remaining2<- remaining[-gr2_ind]

print(log_t_test_core(remaining2))
# The whole remaining group is found convergent t-stat 0.22880938, coeff: 0.02212447
