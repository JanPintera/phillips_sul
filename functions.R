# Defining the data frame with coordinates:
y_star<- function(dat){
  coor<-coordinates(Geo_nuts2)
  spoints<-SpatialPointsDataFrame(coor, as.data.frame(dat))#Check if matching
  # Which d to choose:  the value that corresponds to the maximum absolute sum of the normal standard variate of the statistic Gi(d) for all i observations of the variable X
  # But basically Bartkowska and Riedl and (!) (Getis and Griffith: Comparative Spatial Filtering in Regression Analysis) suggest that:
  # the statistics G_i is evaluated at a series of increasing distances until no further spatial autocorrelation is evident - i.e.finding max G_i...
  # I choose the the Getis and Ord definition with I consided the "standard normal variate" to be this:
  #Z_i<-localG(spoints$values,nb2listw(nbn,zero.policy=TRUE,style="B"),zero.policy=TRUE)# Getis/Ord G: Check which regions are those whith NAs
  # Getis and Ord (1992) (Analysis of Spatial Association by Use of Distance Statistics) suggest that it is G_i from which we substracted
  # the mean and devided it by variation - i.e. this should be the normal standart variate actually...
  # Checking the code manually trace(localG,edit=TRUE)
  #### Choosing the optimal d, with help of localG
  d_t<-1
  sum_Z<-c(0)
  repeat{
    d_t<-d_t+1  
    nbd<-dnearneigh(spoints,0,d_t)
    Z_i<-localG(spoints$values,nb2listw(nbd,zero.policy=TRUE,style="B"),zero.policy=TRUE)
    sum_Z[d_t]<-abs(sum(Z_i,na.rm=TRUE))
    if (sum_Z[d_t]<=sum_Z[d_t-1]){
      d<-d_t-1
      break
    }
  }
  # Preparing the neighbours for the filtering, using the estimated value d...
  nbn <-dnearneigh(spoints,0,d)
  neighbours<-nb2listw(nbn,zero.policy=TRUE,style="B")
  # Writing the G_i statistics using the fact that neighbours$neighbours contains positions of each region that has the connection with given region in the spoints
  # Creating denominator: sum of all x_j not including x_j
  sum_x<-c(0)
  for(i in 1:length(spoints$values)){
    sum_x[i]<- sum(spoints$values[-c(i)])
  }
  
  wx_i<-list(NA)
  for(i in 1:length(neighbours$neighbours)){
    wx_i[[i]]<-NA  
    for(z in 1:length(neighbours$neighbours[[i]])){
      if(neighbours$neighbours[[i]]==0){
        wx_i[[i]]<-0 
      }else{
        wx_i[[i]][z]<- spoints$values[neighbours$neighbours[[i]][z]]
      }
    }
  }
  G_i<-sapply(wx_i, sum)/sum_x
  
  EG_i<- sapply(neighbours$weights, sum)/(length(neighbours$neighbours)-1) 
  # Filtering itself:
  y_star<- spoints$values*(EG_i/G_i)
  return(y_star)
}


###### Phillips and Sul functions:
# function generating h_it:
h<- function(a,b){
  act_data <- log_y_test[log_y_test$GEO %in% names & log_y_test$TIME %in% a,] 
  mean <- mean(act_data$Value)
  return(act_data[b,3]/mean)  
}


H<- function(a, b, names){
  act_data <- log_y_test[log_y_test$GEO %in% names & log_y_test$TIME %in% a,] # This gives us the stuff in same order as it is in the given year!!!
  mean <- mean(act_data$Value)
  return((act_data[b,3]/mean-1)^2)  
}


log_t_test <- function(remaining)
{
  t <- seq(3, 19, by=1)
  log_log <- sapply(t , function(x) 2*log(log(x)))
  H_sq<-list(0)
  H_t<- list(0)
  log_HH <- list(0)
  t_stat<-c(0)
  for(i in 2:length(remaining)){ 
    names <- remaining[seq(from=1, to=i, by=1)]  
    H_sq[[i]]<- foreach(b=1:i, .combine = 'cbind')%:%
      foreach(a=unique(log_y_test$TIME), .combine='c') %do% {
        H(a, b, names)
      }
    H_t[[i]]<- apply(H_sq[[i]], 1, mean)
    log_HH[[i]] <- sapply(H_t[[i]], function(x) -log(x) + log(H_t[[i]][1]))
    t_stat[i]<- summary(lm((log_HH[[i]]-log_log)~log(t)), robust = T)$coefficients[2, 3] 
  }
  return(t_stat)
}


log_t_test_c <- function(k_star, remaining)
{
  t <- seq(3, 19, by=1)
  Hc_sq<- list(0)
  Hc_t<- list(0)
  logc_HH <- list(0)
  tc_stat<-c(0)
  
  for(i in (k_star+1):length(remaining)){
    names<- remaining[c(1:k_star, i)] # NOTE this depends extremely on the previous results - here we don't have any outliers 
    Hc_sq[[i]]<- foreach(b=c(1:k_star), .combine = 'cbind')%:% 
      foreach(a=unique(log_y_test$TIME), .combine='c') %do% {
        H(a,b, names)
      }
    Hc_t[[i]]<- apply(Hc_sq[[i]],1,mean)
    logc_HH[[i]] <- sapply(Hc_t[[i]], function(x) -log(x)+log(Hc_t[[i]][1]))
    tc_stat[i]<- summary(lm((logc_HH[[i]]-log_log)~log(t)), robust = T)$coefficients[2,3]
  }
  return(tc_stat)
}


log_t_test_core <- function(names)
{
  H_sq<- foreach(b=c(1:length(names)), .combine = 'cbind')%:% 
    foreach(a=unique(log_y_test$TIME), .combine='c') %do% {
      H(a,b, names)
    }
  H_t<- apply(H_sq, 1 , mean)
  log_HH <- sapply(H_t, function(x) -log(x)+log(H_t[1]))
  reg_sum <- summary(lm((log_HH-log_log)~log(t)), robust = T)
  t_stat<- reg_sum$coefficients[2,3]
  coef <- reg_sum$coefficients[2,1]
  return(c(t_stat, coef))
}

core<- function(t_stat){
  if(t_stat[2] <= -1.65){
    print("Drop the highest and repeat")
  }else{ 
    i=1
    repeat{
      i=i+1
      if(t_stat[i]<= -1.65){
        break
      }
      print(paste("The core group has k^* equal to ",which(t_stat==max(t_stat[2:i]))))
      k_star <- which(t_stat==max(t_stat[1:i]))
    }
  }
  return(k_star)
}