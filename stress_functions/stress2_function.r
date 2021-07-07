# -------------------------------------------
# Method 2: integrate temperature curve for when temp is >site-specific threshold
# -------------------------------------------

# tempdata must have these cols: 'Site' 'Temperature' 'Date' 
# note: this function calcs stress for each site (and does not 
# break down by year)

stress2_fun <- function(tempdata,site_vector){
  
  # a) find site-specific threshold (1 stdev + mean)
  site_thresholds <- data.frame(
    Site = site_vector,
    threshold = rep(NA,length=length(site_vector)),
    site_mean = rep(NA,length=length(site_vector)),
    site_sd = rep(NA,length=length(site_vector)))
  
  for(s in 1:length(site_vector)){ #for each site...
    # fill in mean temp 
    site_thresholds[site_thresholds$Site==site_vector[s],]$site_mean <- 
      mean(tempdata[tempdata$Site==site_vector[s],]$Temperature)
    # fill in standard deviation 
    site_thresholds[site_thresholds$Site==site_vector[s],]$site_sd <- 
      sd(tempdata[tempdata$Site==site_vector[s],]$Temperature)
    # set new threshold (mean + sd)
    site_thresholds[site_thresholds$Site==site_vector[s],]$threshold <-
      site_thresholds[site_thresholds$Site==site_vector[s],]$site_mean + 
      site_thresholds[site_thresholds$Site==site_vector[s],]$site_sd}
  
  tempdata_stress2 <- left_join(tempdata,site_thresholds)
  
  
  # b) get climatology for each site
  # use loess() and set span=1.5 months
  # span = 90 / number of days in ts = fraction of time series that is 1.5 months
  tempdata_stress2$climatology <- rep(NA,length=length(tempdata_stress2$Site))
  for(s in 1:length(site_vector)){
    df <- tempdata_stress2[tempdata_stress2$Site==site_vector[s],]
    df$index <- seq(from=1,to=length(df$Site),by=1)
    test <- loess(Temperature~index,data=df,span=(90/length(df$index)))
    tempdata_stress2[tempdata_stress2$Site==site_vector[s],]$climatology <- test$fitted}
  rm(df,test,s)
  
  # c) climatology - threshold = temps above threshold (units: deg C)
  tempdata_stress2$stress2_clim_minus_thres <- tempdata_stress2$climatology - tempdata_stress2$threshold
  
  tempdata_stress2$stress2_degC_above_threshold <- ifelse(tempdata_stress2$stress2_clim_minus_thres < 0,0,tempdata_stress2$stress2_clim_minus_thres) #any negative temps (below threshold), convert to 0
  
  # ---
  # comment out the integration, do this outside the function
  # ---
  # # d) integrate the temps above threshold (units: deg C*day)
  # for(s in 1:length(site_vector)){ #for each site
  #   d <- tempdata_stress2[tempdata_stress2$Site==site_vector[s],]
  #   xx <- seq(from=1,to=length(d$Temperature),by=1)
  #   site_stress_values[site_stress_values$Site == site_vector[s],]$integral_m2 <-
  #     trapz(x=xx,y=d$stress2_degC_above_threshold) #integrate under absolute
  #   site_stress_values[site_stress_values$Site == site_vector[s],]$days_in_ts_m2 <-
  #     length(d$stress2_degC_above_threshold[!is.na(d$stress2_degC_above_threshold)])
  # }
  # rm(s,d,xx)
  
  tempdata_stress2 <- as.data.frame(tempdata_stress2)
  
  return(tempdata_stress2)
  
}

