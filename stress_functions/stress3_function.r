# -------------------------------------------
# Method 3: climatology of potential max temp - short term variability (cooling off effect)
# -------------------------------------------


stress3_fun <- function(tempdata,site_vector){
  
  
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
  
  tempdata <- left_join(tempdata,site_thresholds)
  
  
  # A) get two types of climatology 
  
  #tempdata$climatology_max <- rep(NA,length=length(tempdata$Site))
  tempdata$stress3_climatology_max_mw <- rep(NA,length=length(tempdata$Site))
  window_clim = 15
  tempdata$climatology <- rep(NA,length=length(tempdata$Site))
  
  
  for(s in 1:length(site_vector)){
    # subest to a site
    df <- tempdata[tempdata$Site==site_vector[s],]
    df <- df %>% select(Site,Date,Temperature,climatology) 
    df$index <- seq(from=1,to=length(df$Site),by=1)
    
    # get climatology for each site
    # use loess() and set span=1.5 months
    # span = 90 / number of days in ts = fraction of time series that is 1.5 months
    
    test <- loess(Temperature~index,data=df,span=(90/length(df$index)))
    tempdata[tempdata$Site==site_vector[s],]$climatology <- test$fitted
    df$climatology <- test$fitted
    
     
    # ----------------------------------------------
    # block potential climatology using all values >mean clim for now, 
    # might return if I need it
    # ----------------------------------------------
    # 1) climatology using values above mean climatolgoy
    # df$temp_above_clim <- rep(NA,length=length(df$Site))
    # # pull out values greater than climatology (mean temp)
    # df$temp_above_clim <- ifelse(df$Temperature > df$climatology,df$Temperature,NA)
    # addrows <- data.frame(matrix(data=rep(NA,length=ncol(df)),nrow=1,ncol=ncol(df)))
    # names(addrows) <- names(df)
    # addrows$temp_above_clim <- mean(df$temp_above_clim,na.rm=TRUE)
    # addrows$index <- max(df$index) + 1
    # df1 <- rbind(df,addrows)
    # tempday <- loess(temp_above_clim~index,data=df1,span=(90/length(df1$index)))
    # test <- predict(tempday,newdata=df1$index)
    # tempdata[tempdata$Site==site_vector[s],]$climatology_max <- test[1:nrow(df)]
    # rm(addrows,tempday,test,df1)
    
    # 2) in moving window (window_clim)
    dff <- subset(df,select = c('Site','Temperature'))
    #dff <- df %>% select(Site,Temperature)
    dff$index <- seq(from=1,to=length(dff$Site),by=1)
    dff$temp_mw_maxT <- rep(NA,length=length(dff$index))
    # add rows to df for moving window max
    addrows <- data.frame(matrix(data=rep(0,length=ncol(dff)),
                                 nrow=window_clim,ncol=length(rep(0,length=ncol(dff))),
                                 byrow=TRUE))
    names(addrows) <- names(dff)
    addrows$Site <- rep(site_vector[s],length=length(addrows$Site))
    dff1 <- bind_rows(dff,addrows)
    # loop over extended dff1 
    for(i in 1:(length(dff1$index)-window_clim)){ 
      # subset temp data to window
      tsub <- dff1[i:(i+window_clim-1),]
      dff1$temp_mw_maxT[i] <- max(tsub$Temperature)  }
    
    # fill in max temp in moving window col in df
    df$temp_mw_maxT <- dff1$temp_mw_maxT[1:nrow(df)]
    
    # loess using moving window max vals
    tempday1 <- loess(temp_mw_maxT~index,data=df,span=(90/length(df$index)))
    test1 <- predict(tempday1,newdata=df$index)
    tempdata[tempdata$Site==site_vector[s],]$stress3_climatology_max_mw <- test1
    rm(dff1,df,addrows,tempday1,test1)
  }
  rm(tsub,s,i)
  
  
  # B) difference between climatology_max_mw & threshol
  tempdata$stress3_clim_minus_thres_max_mw <- 
    tempdata$stress3_climatology_max_mw - tempdata$threshold
  
  # C) replace negative values with 0
  tempdata$stress3_degC_above_thres_clim_max_mw <- 
    ifelse(tempdata$stress3_clim_minus_thres_max_mw < 0,0,tempdata$stress3_clim_minus_thres_max_mw) 
  
  tempdata_stress3 = as.data.frame(tempdata)
  
  rm(tempdata)
  
  return(tempdata_stress3)
  
}
