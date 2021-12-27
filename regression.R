library(MASS)
library(readr)
library(dplyr)
library(tidyverse)
library(eurostat)
library(arrow)
library(erer)
library(readxl)
library(missMDA)
library(stargazer)


source("functions.R")
source("helper.R")
################################ Base file ########################################################
#exo_data <- income_data %>% select(Year, GEO)
exo_data <- read_parquet("data-processed/income_data.parquet") %>% select(Year, GEO) %>%
              rename(geo=GEO, time = Year) %>% arrange(geo)

nuts2_regions <- unique(exo_data$geo)
nuts1_regions <- substr(nuts2_regions, 1, nchar(nuts2_regions) - 1) %>% unique()
nuts_mapping <- tibble(nuts2_regions) %>% 
                  mutate(nuts1_regions = substr(nuts2_regions, 1, nchar(nuts2_regions) - 1))

exo_data <- exo_data %>% add_column(Club = NA) 
for(i in 1:length(convergence_clubs)){
  exo_data[exo_data$geo %in% convergence_clubs[[i]], ]$Club <- i-1
}


################################ Creating the features #############################################
## Do we have GVA data maybe?
## DO: K = Financial and insurance activities,
##     J = Information and communication, A = Agriculture, forestry and fishing, B-E = Industry (except construction)
specialisation_emp <- get_eurostat("lfst_r_lfe2en2", time_format = "num", type = "code",
                                                     filters=list(nace_r2 = c("A", "B-E", "K", "J"), 
                                                                  age="Y15-64", sex='T',
                                                                  time=c(2008:2019)))
                                                                                                   
specialisation_total <- get_eurostat("lfst_r_lfe2en2", time_format = "num", type = "code",
                                                      filters=list(nace_r2 = "TOTAL", 
                                                                   age="Y15-64", 
                                                                   time=c(2008:2019), sex='T'))


specialisation <- specialisation_emp %>% 
                  inner_join(specialisation_total, by = c("geo", "time"), 
                             suffix = c("", "_total")) %>%
                  mutate(spec = values/values_total) %>%
                  filter(geo %in% nuts2_regions) %>% select(geo, time, nace_r2, spec) %>%
                  group_split(nace_r2)

for( i in seq_along(specialisation))
{
  
  nace <- specialisation[[i]]  %>% distinct(nace_r2) %>% pull()
  print(nace)
  spec_select <- specialisation[[i]] %>% select(-nace_r2)
  exo_data <- exo_data %>%
                left_join(spec_select, by= c("geo", "time"),
                          suffix = c("", paste('_', nace, sep="")))
}

### BS
bs <- get_eurostat("sbs_r_nuts06_r2", time_format = "num", type = "code", 
                   filters = list(indic_sb="V16110",nace_r2=c("J58","J62", "J63","M69", "M70","M71","M73","N78"), 
                   time = c(2008:2019))) %>% filter(geo %in% nuts2_regions) %>%
                   group_by(geo, time) %>% summarise(bs_emp = sum(values)/10^3) %>%
                   inner_join(specialisation_total, by = c("geo", "time"), 
                   suffix = c("", "_total")) %>%
                   mutate(spec_bs = bs_emp/values) %>%
                   group_by(geo) %>% summarise(BS = mean(spec_bs, na.rm = TRUE))


pop_growth <- get_eurostat("demo_r_gind3", time_format = "num", 
                           type = "code", filters = list(indic_de="GROWRT", time=c(2008:2019))) %>%
                          filter(geo %in% nuts2_regions) %>%
                          group_by(geo) %>% summarise(pop_growth = gm_mean(values))


y_o <- get_eurostat("demo_r_pjanaggr3", time_format = "num", type = "code", filters = list(age = c("Y_LT15", "Y_GE65"),
                                        time=c(2008:2019), sex="T")) %>% filter(geo %in% nuts2_regions) %>%
                                        pivot_wider(names_from = age, values_from = values) %>%
                                        mutate(y_o_ratio = Y_LT15/Y_GE65) %>%
                                        group_by(geo) %>% summarise(y_o = mean(y_o_ratio))


scientists<- get_eurostat("rd_p_persreg", time_format = "num", 
                          type = "code", filters = list(prof_pos="RSE", sectperf = "BES", sex="T",
                                                       time=c(2008:2019), unit="PC_ACT_FTE")) %>%
                         filter(geo %in% nuts2_regions) %>%
                         group_by(geo) %>% summarise(scientists_share = mean(values, na.rm = TRUE))


metro_region_typology <- read_excel("Metro-regions-NUTS-2016.xlsx", 
                                     sheet = "List of 2016 metroregions") %>%
                          rename(nuts3 = NUTS_ID, reg_name = `Name of the metro region`) %>%
                          mutate(nuts2 = substr(nuts3, 1, nchar(nuts3)-1))
                          #distinct(nuts2) # this is slightly pointless, 223 nuts2 regions have a metro areas, almost everyone

capitals <- metro_region_typology %>% filter(reg_name %in% capital_cities) %>% distinct(nuts2) %>% pull

# European Quality of Government Index (EQI): https://www.gu.se/en/quality-government/qog-data/data-downloads/data-archive
gov_quality <- read_csv("qog_eqi_agg_2017.csv") %>% 
               filter((nuts %in% nuts2_regions) | (nuts %in% nuts1_regions)) %>%
               left_join(nuts_mapping, by = c("nuts"="nuts1_regions")) %>%
               mutate(geo = coalesce(nuts2_regions, nuts)) %>%
               filter(year == 2017) %>% select(geo, eqi_score)


################################# The estimation itself - merged clubs: ###############################################
regression_data <- exo_data %>% filter(time == 2008 & Club > 0) %>% # Taking year 2008 - the crisis year, as an initial point for  the soecialization variables
                   mutate(Club = as.factor(Club)) %>% 
                   rename(spec_A = spec, spec_BE = `spec_B-E`) %>%
                   mutate(capital = ifelse(geo %in% capitals, 1, 0)) %>% # Capital cities dummy
                   mutate(old_members = ifelse(substr(geo, 1, 2) %in% EU15, 1, 0)) %>%
                   left_join(pop_growth) %>% # will be joined by geo
                   left_join(scientists) %>%
                   left_join(gov_quality) %>%
                   left_join(bs) %>%
                   left_join(y_o)
  
################################# Imputation #########################################################

famd <- imputeFAMD(regression_data %>% select(-any_of(c("time", "Club", "geo"))),
                   ncp=3, scale=TRUE)$completeObs
famd[, c("time", "Club", "geo")] <- regression_data[, c("time", "Club", "geo")]


################################# Regression ##############################################
## Results: significant are young and old ratio (y\_o), spec\_BE = Industry (except construction),
## BS, scientist_share, somewhat also eqi_score
## Most interesting seems probably the result for industry (compare to the Cutrini's industrial core stuff)
## The emphasis on scientists seem to be confirmed

#model<-polr(Club~growth+density+y_o+hum_cap+scientist+BS+spec_agr+spec_con+spec_fin+spec_ret+spec_ind+spec_pub+infra+wage_difference,
#            data=exo_data, method=c("logistic"), Hess = TRUE)
model<-polr(Club ~ pop_growth + y_o + spec_A  + spec_J + spec_K + spec_BE + BS + scientists_share + capital + eqi_score + old_members, 
            data = regression_data, method=c("logistic"), Hess = TRUE)
summary(model)
stargazer(model)
marginal <- ocME(model)

### Imputed:
model_famd <- polr(Club ~ pop_growth + y_o + spec_A  + spec_J + spec_K + spec_BE + BS + scientists_share + capital + eqi_score + old_members, 
     data = famd, method=c("logistic"), Hess = TRUE)
stargazer(model_famd)
famd_marginal <- ocME(model_famd)

