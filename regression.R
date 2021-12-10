library(readr)
library(dplyr)
library(eurostat)
################################ Base file ########################################################
#final_data <- income_data %>% select(Year, GEO)
final_data <- read_parquet("data-processed/income_data.parquet") %>% select(Year, GEO)


################################ Creating the features #############################################
## Do we have GVA data maybe?
## DO: K, J, A, B-E
specialisation_emp <- get_eurostat("lfst_r_lfe2en2", time_format = "num", type = "code",
                                                     filters=list(nace_r2 = c("A", "B-E", "K", "J"), age="Y15-64", time=c(2008:2019)))
                                                                                                   
specialisation_total <- get_eurostat("lfst_r_lfe2en2", time_format = "num", type = "code",
                                                      filters=list(nace_r2 = "TOTAL", age="Y15-64", time=c(2008:2019)))


specialisation <- specialisation_emp %>% 
                  inner_join(specialisation_total, by = c("geo", "time"), 
                             suffix = c("", "_total")) %>%
                  mutate(specialisation = values/values_total)

specialisation %>% group_split(nace_r2)
                  

################################# The estimation itself - merged clubs: ###############################################

model<-polr(Club~growth+density+y_o+hum_cap+scientist+BS+spec_agr+spec_con+spec_fin+spec_ret+spec_ind+spec_pub+infra+wage_difference,
            data=final_data1, method=c("logistic"), Hess = TRUE)

marginal<-ocME(model)

