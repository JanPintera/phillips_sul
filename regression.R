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
library(DMwR)
library(oglmx)
library(corrplot)
library(sandwich)
library(pscl)

source("functions.R")
source("helper.R")
source("regression_functions.R")
# NOTE: source(new_phillips_sul.R) I need convergence_clubs, clubweight etc.
################################ Base file ########################################################
#exo_data <- income_data %>% select(Year, GEO)
convergence_clubs <- readRDS(file = "data-processed/convergence_clubs.rds")
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
## per capita GDP in 
## Do we have GVA data maybe?
gdp_per_cap <- complete_data %>% filter(time == 2008) %>% rename(geo = code,
                                                                 init_gdp = value_complete) %>%
                                                          mutate(log_init_gdp = log(init_gdp)) %>%
                                                          select(geo, log_init_gdp)

last_gdp_per_cap <- complete_data %>% filter(time == 2019) %>% rename(geo = code, final_gdp = value_complete) %>%
                                                               mutate(log_final_gdp = log(final_gdp)) %>%
                                                               select(geo, log_final_gdp)

delta_gdp <- gdp_per_cap %>% inner_join(last_gdp_per_cap) %>% mutate(delta_gdp = log_final_gdp - log_init_gdp) %>% select(geo, delta_gdp, log_final_gdp)

#avg_gdp <- complete_data %>% rename(geo = code, gdp = value_complete) %>% mutate(log_gdp = log(gdp)) %>% group_by(geo) %>% summarise(avg_gdp = mean(log_gdp))

#### Specialization
#### for nace rev.1.1 see lfst_r_lfe2en1
## DO: K = Financial and insurance activities,
##     J = Information and communication (largely contained in the BS below),
##     A = Agriculture, forestry and fishing, B-E = Industry (except construction)
##     G-I = Wholesale and retail trade, transport, accommodation and food service activities
##     R-U = Arts, entertainment and recreation; other service activities; activities of household and extra-territorial organizations and bodies
##     L = Real estate activities

specialisation_emp <- get_eurostat("lfst_r_lfe2en2", time_format = "num", type = "code",
                                                     filters=list(nace_r2 = c("A", "B-E", "K", "G-I", "R-U", "L"), 
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
  spec_select <- specialisation[[i]] %>% select(-nace_r2) %>% rename_with(.fn = ~paste('spec', nace, sep="_"), .cols=spec)
  exo_data <- exo_data %>%
                left_join(spec_select, by= c("geo", "time"))
}

# Getting diffs of the spec vars to be later joined with the exo_data
spec_deltas <- exo_data %>% filter(time >= 2008) %>% # TODO: got "`funs()` was deprecated in dplyr 0.8.0", check if results are correct.
               group_by(geo) %>% summarise_at(vars(starts_with('spec')),
                                                   funs(last(na.omit(.), order_by = time) - 
                                                          first(na.omit(.), order_by = time))) %>%
                                 rename(spec_BE = `spec_B-E`, spec_GI = `spec_G-I`, spec_RU = `spec_R-U`)
#### Business Services
# NOTE: sbs_r_nuts06_r2 provide different totals than in `specialisation_total`, therefore I use the 
specialisation_total_sbs <- get_eurostat("sbs_r_nuts06_r2", time_format = "num", type = "code", 
                                         filters = list(indic_sb="V16110", nace_r2=c("B", "C", "D", "E", "F", "G", "H", "I", "J", "L", "M", "N", "S95"), 
                                                        time = c(2008:2019))) %>% filter(geo %in% nuts2_regions) %>%
                            group_by(geo, time) %>% summarise(values = sum(values, na.rm = TRUE))

bs <- get_eurostat("sbs_r_nuts06_r2", time_format = "num", type = "code", 
                   filters = list(indic_sb="V16110", nace_r2=c("J58","J62", "J63","M69", "M70","M71","M73","N78"), 
                   time = c(2008:2019))) %>% filter(geo %in% nuts2_regions) %>%
                   group_by(geo, time) %>% summarise(bs_emp = sum(values, na.rm = TRUE)) %>%
                   inner_join(specialisation_total_sbs, by = c("geo", "time"), 
                   suffix = c("", "_total")) %>%
                   mutate(spec_bs = bs_emp/values) %>%
                   group_by(geo) %>% summarise(BS_avg = mean(spec_bs, na.rm = TRUE),
                                               BS = first(na.omit(spec_bs), order_by = time), #keep naming convention of the other spec vars - they are by default 2008 values
                                               delta_BS = last(na.omit(spec_bs), order_by = time) - first(na.omit(spec_bs), order_by = time))

pop_growth <- get_eurostat("demo_r_gind3", time_format = "num", 
                           type = "code", filters = list(indic_de="GROWRT", time=c(2008:2019))) %>%
                          filter(geo %in% nuts2_regions) %>%
                          group_by(geo) %>% summarise(pop_growth = gm_mean(values),
                                                      init_pop_growth = first(values, order_by = time),
                                                      delta_pop_growth = last(values, order_by = time) - first(values, order_by = time))


net_migration <- get_eurostat("demo_r_gind3", time_format = "num", 
                              type = "code", filters = list(indic_de="CNMIGRATRT", time=c(2008:2019))) %>%
                              filter(geo %in% nuts2_regions) %>% group_by(geo) %>%
                              summarise(migration = gm_mean(values),
                                        init_migration = first(values, order_by = time))


y_o <- get_eurostat("demo_r_pjanaggr3", time_format = "num", type = "code", filters = list(age = c("Y_LT15", "Y_GE65"),
                                        time=c(2008:2019), sex="T")) %>% filter(geo %in% nuts2_regions) %>%
                                        pivot_wider(names_from = age, values_from = values) %>%
                                        mutate(y_o_ratio = Y_LT15/Y_GE65) %>%
                                        group_by(geo) %>% summarise(y_o = mean(y_o_ratio),
                                                                    init_y_o = first(y_o_ratio, order_by = time),
                                                                    delta_y_o = last(y_o_ratio, order_by = time) - first(y_o_ratio, order_by = time))


scientists<- get_eurostat("rd_p_persreg", time_format = "num", 
                          type = "code", filters = list(prof_pos="RSE", sectperf = "TOTAL", sex="T",
                                                       time=c(2008:2019), unit="PC_ACT_FTE")) %>%
                         filter(geo %in% nuts2_regions) %>% mutate(values = values / 100) %>%
                         group_by(geo) %>% summarise(scientists_share = mean(values, na.rm = TRUE),
                                                     init_scientists_share = first(na.omit(values), order_by = time), #NOTE: we have quite a few missing here, I take first non-null value
                                                     delta_scientist_share = last(na.omit(values), order_by = time) - first(na.omit(values), order_by = time))

tertiary_educated <- get_eurostat("edat_lfse_04", time_format = "num", 
                        type = "code",filters = list(isced11="ED5-8", time=c(2008:2019), sex="T")) %>%
                     group_by(geo) %>% mutate(values = values / 100) %>% summarize(ter_edu = mean(values, na.rm = TRUE),
                                                 init_ter_edu = first(na.omit(values), order_by = time),
                                                 delta_ter_edu = last(values, order_by = time) - first(values, order_by = time))


patents <- get_eurostat("pat_ep_rtot", time_format = "num", type = "code", filters=list(unit="P_MHAB",
                                                                                     time=c(2008:2012))) %>%
           filter(geo %in% nuts2_regions) %>% group_by(geo) %>% summarise(patents = mean(values, na.rm = TRUE),
                                                                          init_patents = first(values, order_by = time),
                                                                          delta_patents = last(values, order_by = time) - first(values, order_by = time))

# European and Regional Innovation Scoreboards 2021
inno_index <- read_excel("EIS_Data.xlsx") %>% rename(nuts = Region, time = Year, values = Value) %>%
                                               filter((nuts %in% nuts2_regions | nuts %in% nuts1_regions) & time <= 2019 & Indicator == "0 Summary Innovation Index") %>%
                                               left_join(nuts_mapping, by = c("nuts"="nuts1_regions")) %>%
                                               mutate(geo = coalesce(nuts2_regions, nuts)) %>%
                                               mutate(values = values / 100) %>% #TODO: this is an ad-hoc rescalling, consider using full standartization (z-score, see below)
                                               group_by(geo) %>% summarise(inno = mean(values, na.rm = TRUE),
                                                                          init_inno = first(values, order_by = time),
                                                                          delta_inno = last(values, order_by = time) - first(values, order_by = time))

# Gross fixed capital formation
gfcf <- get_eurostat('nama_10r_2gfcf', time_format = "num", type = "code",
                     filters = list(currency = "MIO_EUR", nace_r2 = "TOTAL", time=c(2008:2019))) %>%
                     filter(geo %in% nuts2_regions) %>% mutate(log_values = log(values)) %>%
                     group_by(geo) %>% summarise(gfcf = mean(log_values, na.rm = TRUE),
                                                 init_gfcf = first(log_values),
                                                 delta_gfcf = last(log_values, order_by = time) - first(log_values, order_by = time)
                                                )


metro_region_typology <- read_excel("Metro-regions-NUTS-2016.xlsx", 
                                     sheet = "List of 2016 metroregions") %>%
                          rename(nuts3 = NUTS_ID, reg_name = `Name of the metro region`) %>%
                          mutate(nuts2 = substr(nuts3, 1, nchar(nuts3)-1))
                          #distinct(nuts2) # this is slightly pointless, 223 nuts2 regions have a metro areas, almost everyone

capitals <- metro_region_typology %>% filter(reg_name %in% capital_cities) %>% distinct(nuts2) %>% pull
metro_regions <- metro_region_typology %>% distinct(nuts2) %>% pull

# Alternative: CATG_URBRU_REGIO - using three levels urban (1), intermediate (2) and rural (3)
urban_rural <- read_excel("Urban-rural typology of NUTS3 regions (NUTS v. 2016).xlsx",
                          sheet = "Urban-rural typology 2016") %>% 
                          rename(nuts3 = NUTS_ID, urb_rurl_class = URB_RURAL_CLASS) %>%
                          mutate(nuts2 = substr(nuts3, 1, nchar(nuts3)-1)) %>% filter(nuts2 %in% nuts2_regions) %>%
                          group_by(nuts2) %>% summarise(agg_urb_class = min(urb_rurl_class)) # i.e., e.g. if at least one urban nuts3 region (=1) present the whole nuts2 is urban.

urban_regions <- urban_rural %>% filter(agg_urb_class == 1) %>% distinct(nuts2) %>% pull
rural_regions <- urban_rural %>% filter(agg_urb_class == 3) %>% distinct(nuts2) %>% pull

# European Quality of Government Index (EQI): https://www.gu.se/en/quality-government/qog-data/data-downloads/data-archive
gov_quality <- read_csv("qog_eqi_agg_2017.csv") %>% 
               filter((nuts %in% nuts2_regions) | (nuts %in% nuts1_regions)) %>%
               left_join(nuts_mapping, by = c("nuts"="nuts1_regions")) %>%
               mutate(geo = coalesce(nuts2_regions, nuts)) %>%
               group_by(geo) %>% summarise(
                 eqi = mean(eqi_score, na.rm = TRUE),
                 init_eqi = first(eqi_score, order_by = year),
                 delta_eqi = last(eqi_score, order_by = year) - first(eqi_score, order_by = year)
               )

## Finding the richest neighbor - original data
neigh_list <- read_parquet("data-processed/neigh_income.parquet") # last_gdp_per_cap
no_land_neigh_regions <- neigh_list %>% filter(lengths(neighs) == 0) %>% select(geo) %>% as.list() # getting regions with no neighbours
# fixing mosthly GR regions - lot of islands close to mainland, neigh_list effectively maps land borders, region wo border are exclusively island, this mostl make sense (e.g. Mallorca) but especially GR it is too strict - 
# manual_neighs uses discretion to remap the regions 
manual_neighs <- tibble(geo = c("EL30", "EL30", "EL30", "EL41", "EL41", "EL41", "EL42", "EL42", "EL42", "EL42", "EL43", "EL62", "EL62", "EL51", "EL51", "EL51", "EL51","EL54", "EL54", "EL54", "EL54", "EL63", "EL63", "EL63", "EL63", "EL63", "EL64", "EL64", "EL64", "EL64", "EL64", "EL64", "EL65", "EL65", "EL65"),
                     neighs = c("EL64", "EL65", "EL42", "EL42", "EL51", "EL64", "EL41", "EL64", "EL43", "EL30", "EL42", "EL63", "EL54", "BG41", "BG42", "EL52", "EL41","EL53", "EL61", "EL63", "EL62", "EL65", "EL54", "EL61", "EL64", "EL62", "EL30", "EL61", "EL63", "EL65", "EL41", "EL42", "EL30", "EL63", "EL65"))
manual_neighs <- bind_rows(manual_neighs, tibble(geo=c("ITG1", "ITF5", "ITF5", "ITG2", "FRM0"), neighs=c("ITF6", "ITF5", "ITG1", "FRM0", "ITG2"))) # fixing insular Italy + Corsica
manual_neighs <- bind_rows(manual_neighs, tibble(geo=c("FI20", "FI1C", "FI1C", "FI1C", "FI1C"), neighs=c("FI1C", "FI19", "FI1B", "FI1D", "FI20"))) # Aaland
manual_neigh_income <- manual_neighs %>% left_join(gdp_per_cap, by=c('neighs'='geo')) %>% left_join(last_gdp_per_cap, by=c('neighs'='geo')) %>% group_by(geo) %>%
                                   summarise(neigh_highest_income = max(log_init_gdp), delta_highest_income = max(log_final_gdp) - max(log_init_gdp),
                                             neigh_avg_income = mean(log_init_gdp), delta_neigh_avg_income = mean(log_final_gdp) - mean(log_init_gdp))


neigh_income <- neigh_list %>% unnest(neighs, keep_empty=TRUE) %>% # keep_empty=FALSE could be used for robustness
                left_join(gdp_per_cap, by=c('neighs'='geo')) %>% left_join(last_gdp_per_cap, by=c('neighs'='geo')) %>% group_by(geo) %>%
                summarise(neigh_highest_income = max(log_init_gdp),
                          delta_highest_income = max(log_final_gdp) - max(log_init_gdp),
                          neigh_avg_income = mean(log_init_gdp), 
                          delta_neigh_avg_income = mean(log_final_gdp) - mean(log_init_gdp)) %>%
                replace_na(list(neigh_highest_income=0, delta_highest_income=0, neigh_avg_income=0, delta_neigh_avg_income=0)) #NOTE: change replace_na for different missingness treatment

# updating data with manually matched neighbours
neigh_income <- neigh_income %>% rows_update(manual_neigh_income, by="geo")
no_neigh_regions <- setdiff(no_land_neigh_regions$geo, manual_neighs$geo)

############################### ECOSYS-Robustness ##############################################
# formula_spec_plus
# pre-2008 NACE Rev.1.1 specializations
# NOTE: apart from specialization we just need to redownload the data, there is little bit of code repetition but I prefer not to touch the original
gdp_per_cap_ecosys <- complete_data %>% filter(time == 2003) %>% rename(geo = code, init_gdp = value_complete) %>% 
                                        mutate(log_init_gdp = log(init_gdp)) %>% select(geo, log_init_gdp)

specialisation_emp_rev11 <- get_eurostat("lfst_r_lfe2en1", time_format = "num", type = "code") %>% 
                                         filter(nace_r1 %in% c("A_B", "C-E", "J_K", "G-I")
                                                & between(time, 2003, 2008) # 2008 is the last year for NACE Rev 1.1
                                                & age == "Y15-64"
                                                & sex == 'T')

specialisation_emp_rev11_total <- get_eurostat("lfst_r_lfe2en1", time_format = "num", type = "code") %>% filter(nace_r1 == 'TOTAL' & between(time, 2003, 2008) & age == "Y15-64" & sex == 'T')

specialisation_rev11 <- specialisation_emp_rev11 %>% 
                        inner_join(specialisation_emp_rev11_total, by = c("geo", "time"), 
                                  suffix = c("", "_total")) %>%
                        mutate(spec = values/values_total) %>%
                        filter(geo %in% nuts2_regions) %>% select(geo, time, nace_r1, spec) %>%
                        pivot_wider(names_from = nace_r1, values_from = spec) %>% plyr::rename(c("A_B" = "spec_AB", "C-E" = "spec_CE", "G-I" = "spec_GI", "J_K" = "spec_JK")) %>%  #I concentrate on inits here due to little compatibility between the two NACE revisions
                        group_by(geo) %>% summarise_at(vars(starts_with('spec')),
                                                       funs(first(., order_by = time)))

#bs_ecosys <- get_eurostat("sbs_r_nuts03", time_format = "num", type = "code") NOTE: I don't see easy way to map the NACE Rev.2 categories to Rev.11

gfcf_ecosys <- get_eurostat('nama_10r_2gfcf', time_format = "num", type = "code") %>% filter(currency == "MIO_EUR" & nace_r2 == "TOTAL" & between(time, 2003, 2019) & geo %in% nuts2_regions) %>% 
                            mutate(log_values = log(values)) %>% group_by(geo) %>%
                            summarise(gfcf = mean(log_values, na.rm = TRUE),
                                      init_gfcf = first(log_values, order_by = time),
                                      delta_gfcf = last(log_values, order_by = time) - first(log_values, order_by = time))

scientists_ecosys <- get_eurostat("rd_p_persreg", time_format = "num", type = "code") %>%
                                 filter(prof_pos=="RSE" & sectperf=="TOTAL" & sex=="T" & between(time, 2003, 2019) & unit=="PC_ACT_FTE" & geo %in% nuts2_regions) %>% mutate(values = values / 100) %>%
                                  group_by(geo) %>% summarise(scientists_share = mean(values, na.rm = TRUE),
                                                            init_scientists_share = first(values, order_by = time),
                                                            delta_scientist_share = last(values, order_by = time) - first(values, order_by = time)
                                                            )

tertiary_educated_ecosys <- get_eurostat("edat_lfse_04", time_format = "num", type = "code") %>% filter(isced11=="ED5-8" & between(time, 2003, 2019) & sex=="T") %>%
                                        filter(geo %in% nuts2_regions) %>% mutate(values = values / 100) %>% group_by(geo) %>% summarize(ter_edu = mean(values, na.rm = TRUE),
                                                                                                                                         init_ter_edu = first(values, order_by = time),
                                                                                                                                         delta_ter_edu = last(values, order_by = time) - first(values, order_by = time))

# Finding the richest neighbor
neigh_income_ecosys <- neigh_list %>% unnest(neighs, keep_empty = TRUE) %>% left_join(gdp_per_cap_ecosys, by=c('neighs'='geo')) %>% group_by(geo) %>%
                                      summarise(neigh_highest_income = max(log_init_gdp), neigh_avg_income = mean(log_init_gdp))%>% replace_na(list(neigh_highest_income=0, neigh_avg_income=0)) #NOTE: change replace_na for different missingness treatment


manual_neigh_income_ecosys <- manual_neighs %>% left_join(gdp_per_cap_ecosys, by=c('neighs'='geo')) %>% group_by(geo) %>%
                                          summarise(neigh_highest_income = max(log_init_gdp), neigh_avg_income = mean(log_init_gdp))

neigh_income_ecosys <- neigh_income_ecosys %>% rows_update(manual_neigh_income_ecosys, by="geo")
################################ Regression weights ############################################
clubweight <- clubweight %>% select(order(colnames(clubweight))) # NOTE! this has to have the same order as regression_data below, TODO: check!
wreg <- unlist(apply(clubweight, 2, function(x) mean(abs(x-1))^(-1)))
wreg_df <- enframe(wreg, name = "geo", value = "wreg")

################################# The estimation itself - merged clubs: ###############################################
regression_data <- exo_data %>% filter(time == 2008 & Club > 0) %>% # Taking year 2008 - the crisis year, as an initial point for  the specialization variables
                   mutate(Club = as.factor(Club)) %>% 
                   rename(spec_BE = `spec_B-E`, spec_GI = `spec_G-I`, spec_RU = `spec_R-U`) %>%
                   mutate(capital = ifelse(geo %in% capitals, 1, 0)) %>% # Capital cities dummy
                   mutate(metro = ifelse(geo %in% metro_regions, 1, 0)) %>%
                   mutate(urban = ifelse(geo %in% urban_regions, 1, 0)) %>%
                   mutate(rural = ifelse(geo %in% rural_regions, 1, 0)) %>%
                   mutate(old_members = ifelse(substr(geo, 1, 2) %in% EU15, 1, 0)) %>%
                   left_join(spec_deltas, by=c("geo"), suffix=c("", "_delta")) %>%
                   left_join(pop_growth) %>% # will be joined by geo
                   left_join(tertiary_educated) %>%
                   left_join(scientists) %>%
                   left_join(gov_quality) %>%
                   left_join(bs) %>%
                   left_join(y_o) %>%
                   left_join(gfcf) %>%
                   left_join(wreg_df) %>%
                   left_join(gdp_per_cap) %>%
                   left_join(patents) %>%
                   left_join(inno_index) %>%
                   left_join(net_migration) %>%
                   mutate(border_region = ifelse(geo %in% border_regions, 1, 0))    


sum(complete.cases(regression_data)) # missigness problems

regression_data_neigh <- data.frame(regression_data) %>% left_join(neigh_income) # isolation data with and without neighs
regression_data_neigh_filtered <- regression_data_neigh  %>% filter(!geo %in% no_neigh_regions) # could be used for ROBUSTNESS - filtering out regions wo neighbours, while using them in inputation

################################## Robustness - ECOSYS reviewers suggestions
regression_data_robustness <- data.frame(regression_data) %>% select(time, geo, Club, capital, metro)
regression_data_robustness <- regression_data_robustness %>%
                              left_join(gdp_per_cap_ecosys) %>%
                              left_join(specialisation_rev11) %>%
                              left_join(gfcf_ecosys) %>%
                              left_join(scientists_ecosys) %>%
                              left_join(tertiary_educated_ecosys) %>%
                              left_join(neigh_income_ecosys) %>%
                              mutate(border_region = ifelse(geo %in% border_regions, 1, 0))  
regression_data_robustness_filtered <- regression_data_robustness  %>% filter(!geo %in% no_neigh_regions) # could be used for ROBUSTNESS - filtering out regions wo neighbours, while using them in inputation

famd_robustness <-imputeFAMD(regression_data_robustness %>% select(-any_of(c("time", "Club", "geo"))),
                             ncp=3, scale=TRUE)$completeObs
famd_robustness[, c("time", "Club", "geo")] <- regression_data_robustness[, c("time", "Club", "geo")]

famd_robustness_filtered <-imputeFAMD(regression_data_robustness_filtered %>% select(-any_of(c("time", "Club", "geo"))),
                             ncp=3, scale=TRUE)$completeObs
famd_robustness_filtered[, c("time", "Club", "geo")] <- regression_data_robustness_filtered[, c("time", "Club", "geo")]

################################# Imputation - FAMD #########################################################

famd <- imputeFAMD(regression_data %>% select(-any_of(c("time", "Club", "geo"))),
                   ncp=3, scale=TRUE)$completeObs
famd[, c("time", "Club", "geo")] <- regression_data[, c("time", "Club", "geo")]


famd_neigh <- imputeFAMD(regression_data_neigh %>% select(-any_of(c("time", "Club", "geo"))),
                   ncp=3, scale=TRUE)$completeObs
famd_neigh[, c("time", "Club", "geo")] <- regression_data_neigh[, c("time", "Club", "geo")]

famd_neigh_filtered <- imputeFAMD(regression_data_neigh_filtered %>% select(-any_of(c("time", "Club", "geo"))),
                         ncp=3, scale=TRUE)$completeObs
famd_neigh_filtered[, c("time", "Club", "geo")] <- regression_data_neigh_filtered[, c("time", "Club", "geo")]

################################ Imputation - kNN ################################################
knn <- knnImputation(regression_data %>% select(-any_of(c("time", "Club", "geo"))))


################################ Variable Standardization ####################################
normalized_data <- as_tibble(apply(regression_data %>% select(-c("Club", "geo", "time", "capital", "metro", "urban", "rural", "old_members")),
                                 2, scale)) # substracting mean and dividing by standart deviation for each column = z-score
normalized_famd <- imputeFAMD(normalized_data %>% select(-any_of(c("time", "Club", "geo"))),  ncp=3, scale=TRUE)$completeObs

normalized_data[, c("time", "Club", "geo", "capital", "metro", "urban", "rural", "old_members")] <- regression_data[, c("time", "Club", "geo", "capital", "metro", "urban", "rural", "old_members")]
normalized_famd[, c("time", "Club", "geo", "capital", "metro", "urban", "rural", "old_members")] <- normalized_data[, c("time", "Club", "geo", "capital", "metro", "urban", "rural", "old_members")]
summary(normalized_data)

################################# Regression ##############################################
## Results: significant are young and old ratio (y\_o), spec\_BE = Industry (except construction),
## BS, scientist_share, somewhat also eqi_score
## Most interesting seems probably the result for industry (compare to the Cutrini's industrial core stuff)
## The emphasis on scientists seem to be confirmed

####################################š DESCRIPTIVE ANALYSIS ##################################
summary(regression_data %>% select(-c("capital", "metro", "urban", "rural", "old_members")))

summary(famd %>% select(-c("capital", "metro", "urban", "rural", "old_members")))

# Dummy variables - contingency tables
lapply(regression_data[, c("Club", "capital", "metro", "urban", "rural", "old_members")], table)

ftable(xtabs(~ Club + capital + metro + urban + rural + old_members, data = regression_data))
#ftable(xtabs(~ Club + capital + metro , data = regression_data)) # top club is 6:4 formed by capitals


#### Correlation matrices
regression_data_corr <- regression_data_neigh %>% select(-c(geo, time)) %>% mutate(Club = as.integer(Club))
corrplot(cor(regression_data_corr, use = "complete.obs")) # not really readable

# Correlation
corrplot(
  cor(regression_data_corr %>% select(!starts_with('init')), use = "complete.obs")
)

# Human capital variables correlation - ter_edu and scientists_share look least correlated, can be used together
corrplot(cor(regression_data_corr %>% select(patents, ter_edu, scientists_share, inno), use = "complete.obs"))
corrplot(cor(regression_data_corr %>% select(init_patents ,init_ter_edu, init_scientists_share, init_inno), use = "complete.obs"))

corrplot(cor(regression_data_corr %>% select(inno, eqi, patents, ter_edu, scientists_share),  use = "complete.obs")) # inno and eqi two are strongly correlated! Patents less

corrplot(cor(regression_data_corr %>% select(Club, neigh_highest_income, neigh_avg_income, init_eqi, init_inno, init_ter_edu, init_patents, init_scientists_share),  use = "complete.obs"))

# Capital and finance spec.correlation:
cor(regression_data_corr %>% select(spec_K, capital), use = "complete.obs") #0.4081713, but see below spec_K has way higher avg value in Club 1 than elsewhere
cor(regression_data_corr %>% select(neigh_highest_income, neigh_avg_income), use = "complete.obs") # 0.8474535!

ftable(xtabs(~ Club + capital + metro , data = regression_data)) # top club is 6:4 formed by capitals

#### Club descriptive statistics - descriptive stats, see formula_spec_plus (see below for capital and metro)
var_stats <- regression_data_neigh %>% group_by(Club) %>% summarise(log_init_gdp = mean(log_init_gdp),
                                                 init_gfcf = mean(init_gfcf, na.rm=TRUE),
                                                 spec_A = mean(spec_A, na.rm=TRUE),
                                                 spec_K = mean(spec_K, na.rm=TRUE),
                                                 spec_BE = mean(spec_BE, na.rm=TRUE),
                                                 spec_GI = mean(spec_GI, na.rm=TRUE),
                                                 spec_RU = mean(spec_RU, na.rm=TRUE),
                                                 BS = mean(BS, na.rm=TRUE),
                                                 init_scientists_share = mean(init_scientists_share, na.rm=TRUE),
                                                 init_ter_edu = mean(init_ter_edu, na.rm=TRUE),
                                                 init_eqi = mean(init_eqi, na.rm=TRUE),
                                                 neigh_avg_income = mean(neigh_avg_income),
                                                 neigh_highest_income = mean(neigh_highest_income)
                                                 )
#var_stats %>% kbl(caption="Summary Statistics of the Explanatory variables",
#                  format="latex", col.names = c("Gender","Education","Count","Mean", "Bflm"), align="r")

stargazer(regression_data_neigh_filtered, type = "latex", title="Explanatory variables")

# get summary for convergence clubs both 2003-2015 and 2003-2019 versions
regression_data_gdp <- complete_data %>% filter(time == 2003) %>% rename(geo = code, init_gdp = value_complete) %>%
                                         mutate(log_init_gdp = log(init_gdp)) %>% inner_join(exo_data) %>% select(geo, time, Club, init_gdp, log_init_gdp)
delta_gdp_2019 <- regression_data_gdp %>% inner_join(last_gdp_per_cap) %>% mutate(delta_gdp = log_final_gdp - log_init_gdp) %>% select(geo, delta_gdp, log_final_gdp)
regression_data_gdp_2019 <- regression_data_gdp %>% left_join(delta_gdp_2019)
#regression_data_gdp <- regression_data %>% left_join(delta_gdp) # NOTE! This is for 2008, change!
regression_data_gdp_2019 %>% group_by(Club) %>% summarise(mean_log_init_gdp = mean(log_init_gdp),
                                                          mean_log_final_gdp = mean(log_final_gdp),
                                                          delta_gdp = mean(delta_gdp),
                                                          sd_log_init_gdp = sd(log_init_gdp),
                                                          sd_log_final_gdp = sd(log_final_gdp))

#2015
last_gdp_per_cap_2015 <- complete_data %>% filter(time == 2015) %>% rename(geo = code, final_gdp = value_complete) %>%
                                                               mutate(log_final_gdp = log(final_gdp)) %>% select(geo, log_final_gdp)
delta_gdp_2015 <- regression_data_gdp %>% inner_join(last_gdp_per_cap_2015) %>% mutate(delta_gdp = log_final_gdp - log_init_gdp) %>% select(geo, delta_gdp, log_final_gdp)
regression_data_gdp_2015 <- regression_data_gdp %>% left_join(delta_gdp_2015)
regression_data_gdp_2015 %>% group_by(Club) %>% summarise(mean_log_init_gdp = mean(log_init_gdp),
                                                          mean_log_final_gdp = mean(log_final_gdp),
                                                          delta_gdp = mean(delta_gdp),
                                                          sd_log_init_gdp = sd(log_init_gdp),
                                                          sd_log_final_gdp = sd(log_final_gdp))

# to latex, code from here seems to be useful https://sdaza.com/blog/2020/descriptive-tables/?fbclid=IwAR1qAgC6nxk1UTuqi3fWy5oW0zXQs2uQitkUNAFJuvRCT9nh8UHuwoEHgAY


################## FORMULAS
# i.e.listing the "basic" versions of the variables = init conditions for specs and mean for the others
# NOTE: this is meant more as an overview than anything else the polr does not converge
formula_all <- Club ~ log_init_gdp +
                      gfcf +
                      pop_growth + y_o +
                      spec_A  + spec_K + spec_BE + spec_GI + spec_L + spec_RU + BS + 
                      scientists_share + patents + inno + ter_edu +
                      capital + metro + urban + rural +
                      old_members + eqi

# Using init conditions - either 2008 or first available, the format is `init_variable`, spec vars are in the init form always
formula_init <- Club ~ log_init_gdp +
                       init_gfcf +
                       init_pop_growth + init_y_o +
                       spec_A + spec_K + spec_BE + spec_GI + spec_L  + spec_RU + BS +
                       init_scientists_share + init_patents + init_inno + init_ter_edu +
                       capital + metro + urban + rural +
                       old_members + init_eqi

                       
# Formula using specifically chosen limited set of variables - the specialisations (wo Real Estate), Eurostat innovation index, Quality of governance and capital/metro distinction
# These are variables than seem to give more interpretable results - alternatives (such as urban/rural) are used elsewhere, epecially in formula_init_alter
formula_spec_plus <- Club ~ log_init_gdp + init_gfcf +
                            spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +
                            init_scientists_share + init_ter_edu + # note inno and eqi being strongly correlated
                            capital + metro +# more significant than urban + rural, spec_K loses signif. + more in line with NEG 
                            init_eqi + border_region

# Version of formula without variables with a lot of missing vars - robustness check
# vars with a lot of missing stuff = spec_L
formula_init_nans <- Club ~ log_init_gdp +
                     init_gfcf +
                     init_pop_growth + init_y_o +
                     spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +
                     init_scientists_share + init_patents + init_inno + init_ter_edu +
                     capital + metro + urban + rural +
                     old_members + init_eqi
                                
# Formula showing changes, delta change of formula_spec_plus
                            
formula_delta <- Club ~ delta_gfcf +
                 spec_A_delta + spec_K_delta + spec_BE_delta + spec_GI_delta + spec_RU_delta + delta_BS +
                 delta_scientist_share + delta_ter_edu +
                 capital + metro +
                 delta_eqi

formula_delta_neigh <- Club ~ delta_gfcf +
                       spec_A_delta + spec_K_delta + spec_BE_delta + spec_GI_delta + spec_RU_delta + delta_BS +
                       delta_scientist_share + delta_ter_edu +
                       capital + metro + delta_eqi + 
                       delta_highest_income + delta_neigh_avg_income + border_region

# testing vars omitted from formula_spec_plus
formula_init_alter <- Club ~ #init_pop_growth + init_y_o +
                             log_init_gdp + init_gfcf +
                             spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +  
                             init_inno +
                             urban + rural + init_eqi #+ old_members

# formula_spec_plus w init_patent instead of scientists - should be used with normalized data only due to patents variable scale
formula_patent <- Club ~ log_init_gdp + init_gfcf +
                  spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +
                  init_patents +
                  capital + metro +# more significant than urban + rural, spec_K loses signif. + more in line with NEG 
                  init_eqi

# Formula using data starting 2003 suggested in the ecosys review, should be compared with `formula_spec_plus`
formula_spec_plus_ecosys <- Club ~ log_init_gdp + init_gfcf +
                                   spec_AB + spec_JK + spec_CE + spec_GI +
                                   init_scientists_share + init_ter_edu + capital + metro + neigh_highest_income + border_region

# New Economic Geography vars (this is mainly agglomeration concentration) + those inspired by Iammarino (2017)
# NEG = agglomeration effects vs. labour migration and knowledge spillovers, physical connectivity alone should not work
formula_neg <- Club ~ capital + metro + init_migration

# suggested by ecosys review - same as formula_spec_plus but with added neigh variables
formula_spec_neigh <- Club ~ log_init_gdp + init_gfcf +
                              spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +
                              init_scientists_share + init_ter_edu + # note inno and eqi being strongly correlated
                              capital + metro + init_eqi +
                              neigh_highest_income + border_region # using the avg.init gdp of neigh regions here
                        
# alternative specification for comparison purposes formula_spec1 = 
formula_spec1  <- Club ~ log_init_gdp + init_gfcf +
                  spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS +
                  init_scientists_share + init_ter_edu

formula_spec2 <- Club ~  log_init_gdp +init_gfcf + capital + metro + init_eqi + neigh_highest_income + border_region

######################################### ESTIMATION #########################
#formula_test <- Club ~ log_init_gdp + init_gfcf +
#  spec_K + spec_BE + spec_GI + spec_RU + BS +
#  init_scientists_share + init_ter_edu + # note inno and eqi being strongly correlated
#  capital + metro +# more significant than urban + rural, spec_K loses signif. + more in line with NEG 
#  init_eqi
#model_test <- polr(formula_test, 
#                   data = famd, method=c("logistic"), Hess = TRUE)
#summary(model_test)
#ocME(model_test)$out


# Some basic look - using polr
model <- polr(formula_all, 
              data = normalized_data, method=c("logistic"), Hess = TRUE)
summary(model)
marginal <- ocME(model)$out


model_selected <- polr(formula_spec_plus, 
                       data = regression_data, method=c("logistic"), Hess = TRUE)
summary(model_selected)
selected_marginal <- ocME(model_selected)$out


model_selected_norm <- polr(formula_spec_plus,
                            data = normalized_data, method=c("logistic"), Hess = TRUE)
summary(model_selected_norm)
selected_marginal_norm <- ocME(model_selected_norm)$out


### Imputed FAMD:
### Note that for gfcf the data are missing and therefore fully imputed above - do some robustness check without the variable above
model_famd <- polr(formula_all, data = famd, method=c("logistic"), Hess = TRUE)
summary(model_famd) # stargazer(famd_marginal$out$ME.all) for latex print-out
famd_marginal <- ocME(model_famd)$out


### FAMD - selected variables
model_famd_selected <- polr(formula_spec_plus, data = famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_selected) # stargazer(famd_marginal$out$ME.all) for latex print-out
famd_selected_marginal <- ocME(model_famd_selected)$out

### FAMD - initial conditions
model_famd_init <- polr(formula_init, data = famd, method=c("logistic"),
                        Hess = TRUE, start=c(model_famd$coefficients, model_famd$zeta)) # was not convergig using model_famd as starting values
summary(model_famd_init)
famd_init_marginal <- ocME(model_famd_init)$out

### FAMD - omitted vars = variables left out from the main regression

model_famd_alter <- polr(formula_init_alter, 
                         data = famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_alter)
famd_alter_marginal <- ocME(model_famd_alter)$out

### FAMD - deltas
model_famd_delta <- polr(formula_delta, data = famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_delta)
famd_delta_marginal <- ocME(model_famd_delta)$out

### FAMD - norm
model_famd_selected_norm <- polr(formula_spec_plus, data = normalized_famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_selected_norm) # stargazer(famd_marginal$out$ME.all) for latex print-out
famd_sel_marginal_norm <- ocME(model_famd_selected_norm)$out


model_famd_alter_norm <- polr(formula_init_alter, 
                              data = normalized_famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_alter_norm)
famd_alter_marginal_norm <- ocME(model_famd_alter_norm)$out


# patents have positive and significant effect
model_famd_patent_norm <- polr(formula_patent, 
                              data = normalized_famd, method=c("logistic"), Hess = TRUE)
summary(model_famd_patent_norm)
famd_patent_marginal_norm <- ocME(model_famd_patent_norm)$out

############# Weighted
model_selected_w <- polr(formula_spec_plus, data = regression_data, method = c("logistic"),
                         weights = wreg, Hess = TRUE)
summary(model_selected_w)
selected_marginal_w <- ocME(model_selected_w)$out


model_famd_w <- polr(formula_all, data = famd, method=c("logistic"), weights = wreg,
                     start = c(model_famd$coefficients, model_famd$zeta), Hess = TRUE) # not converging
summary(model_famd_w) # stargazer(famd_marginal$out$ME.all) for latex print-out
famd_marginal_w <- ocME(model_famd_w)$out


model_famd_sel_w <- polr(formula_spec_plus, data = famd, method=c("logistic"), weights = wreg,
                     Hess = TRUE)
summary(model_famd_sel_w)
famd_marginal_w <- ocME(model_famd_sel_w)$out

#### NEG:

model_neg <- polr(formula_neg, data = regression_data, method=c("logistic"), Hess = TRUE)
ocME(model_neg)$out

#### Alternative method - robust std. errors

# Robust Errors for model_famd_selected
coeftest(model_famd_selected, vcov = sandwich) # https://stackoverflow.com/questions/27367974/different-robust-standard-errors-of-logit-regression-in-stata-and-r
famd_selected_marg_robust <- ocME_robust(model_famd_selected)$out #NOTE: this appears in the text

# model_famd_selected_norm
coeftest(model_famd_selected_norm, vcov = sandwich)
famd_sel_marg_norm_robust <- ocME_robust(model_famd_selected_norm)$out

# model_famd_alter - robust std err
coeftest(model_famd_alter, vcov = sandwich)
famd_alter_marg_robust <- ocME_robust(model_famd_alter)$out

# model_famd_delta - robust std err
coeftest(model_famd_delta, vcov = sandwich)
famd_delta_marg_robust <- ocME_robust(model_famd_delta)$out

############ Ecosys - robustness
# 2003 init specializations
model_famd_selected_ecosys <- polr(formula_spec_plus_ecosys, data = famd_robustness, method=c("logistic"), Hess = TRUE)
famd_selected_marginal_ecosys <- ocME(model_famd_selected_ecosys)$out
coeftest(model_famd_selected_ecosys, vcov = sandwich)
famd_selected_marginal_ecosys_robust <- ocME_robust(model_famd_selected_ecosys)$out

# 2003 init specializations - Leaving the no neigh regions
model_famd_selected_ecosys_filtered <- polr(formula_spec_plus_ecosys, data = famd_robustness_filtered, method=c("logistic"), Hess = TRUE)
famd_selected_marginal_ecosys_robust_filtered <- ocME_robust(model_famd_selected_ecosys_filtered)$out

# adding highest neighbour income to the original data - confirm the original formula_spec_plus conclusion - (strong scietists + industry)
model_famd_neigh <- polr(formula_spec_neigh, data = famd_neigh, method=c("logistic"), Hess = TRUE)
famd_selected_marginal_neigh <- ocME(model_famd_neigh)$out
famd_selected_marginal_neigh_robust <- ocME_robust(model_famd_neigh)$out

# Leaving the no neigh regions - Manufacturing effect is kept intact but init_scientists_share loses significance, init ter edu still has it
model_famd_neigh_filtered <- polr(formula_spec_neigh, data = famd_neigh_filtered, method=c("logistic"), Hess = TRUE)
famd_selected_marginal_neigh_robust_filtered <- ocME_robust(model_famd_neigh_filtered)$out

# deltas -change in scientist share is significant, industry not really
model_famd_delta_neigh <- polr(formula_delta_neigh, data = famd_neigh, method=c("logistic"), Hess = TRUE)
famd_delta_marginal_neigh <- ocME_robust(model_famd_delta_neigh)$out

model_famd_delta_neigh_filtered <- polr(formula_delta_neigh, data = famd_neigh_filtered, method=c("logistic"), Hess = TRUE)
famd_delta_marginal_neigh_filtered <- ocME_robust(model_famd_delta_neigh_filtered)$out

# checking alternative innovation metrics - current version tests adding border regions dummy and neighbour variables.
famd_test <- famd_neigh_filtered %>% #mutate(border_region = ifelse(geo %in% border_regions, 1, 0)) %>% 
                      mutate(neigh_highest_avg_diff = neigh_highest_income - neigh_avg_income) %>% 
                      mutate(distance2highest_income = neigh_highest_income - log_init_gdp) %>%
                      mutate(distance2avg_income = neigh_avg_income - log_init_gdp) %>%
                      inner_join((income_data %>% filter(Year==2008) %>% select(GEO, y_star)), by=c("geo" = "GEO"))

formula_check <- Club ~ init_gfcf + log_init_gdp + #y_star +
                 spec_A + spec_K + spec_BE + spec_GI + spec_RU + BS + init_scientists_share + init_ter_edu + # note inno and eqi being strongly correlated
                 capital + metro +# more significant than urban + rural, spec_K loses signif. + more in line with NEG 
                 init_eqi  + distance2highest_income + neigh_avg_income + border_region
model_check <- polr(formula_check, data = famd_test, method=c("logistic"), Hess = TRUE)
ocME_robust(model_check)$out
cor(famd_test %>% select(neigh_avg_income, log_init_gdp))
cor(famd_test %>% select(neigh_highest_income, neigh_avg_income))
########################## Specifications - complements to famd_delta_marginal_neigh_filtered
# Specification 1
#formula_spec1
model_famd_filtered_spec1 <- polr(formula_spec1, data = famd_neigh_filtered, method=c("logistic"), Hess = TRUE)
famd_filtered_marginal_spec1 <- ocME_robust(model_famd_filtered_spec1)$out

model_famd_filtered_spec2 <- polr(formula_spec2, data = famd_neigh_filtered, method=c("logistic"), Hess = TRUE)
famd_filtered_marginal_spec2 <- ocME_robust(model_famd_filtered_spec2)$out

########################## Reporting results:
stargazer(famd_selected_marg_robust$ME.all) # orig. IES WP results + border region dummy
stargazer(famd_filtered_marginal_spec1$ME.all) # Specification 1 - sectoral and human capital variables - for consistency, it is also filtered 
stargazer(famd_filtered_marginal_spec2$ME.all) # Specification 2 - sectoral and human capital variables - for consistency, it is also filtered  
stargazer(famd_selected_marginal_neigh_robust_filtered$ME.all) # 2008 values with neigh. variables and with neigh regions filtered out
stargazer(famd_selected_marginal_ecosys_robust_filtered$ME.all) # 2003 init values

stargazer(regression_data_neigh_filtered, type = "latex", title="Explanatory variables") # this is descriptive_vars_filtered

stargazer(model_famd_filtered_spec1)
stargazer(model_famd_filtered_spec2)
stargazer(model_famd_neigh_filtered) # Regression results (Table 10)

# Filtering only significant marginal effects.
sapply(famd_selected_marginal_ecosys_robust_filtered, function(x){which(x[, 4] < 0.05)})

#### Goodness of fit:
#logLik(model_famd_filtered_spec1) #AIC(model_famd_filtered_spec1)
pR2(model_famd_filtered_spec1)
pR2(model_famd_filtered_spec2)
pR2(model_famd_neigh_filtered)
pR2(model_famd_selected)
pR2(model_famd_selected_ecosys_filtered)

###### TODOs + Suggestions:

###################### Write-outs ##########################
write_parquet(exo_data, "data-processed/exo_data.parquet")
write_parquet(regression_data, "data-processed/regression_data.parquet")
write_parquet(regression_data_robustness, "data-processed/regression_data_robustness.parquet")
