# -------------------------------------------
# Method 1: short-term variability, integrate short-term variability curve 
# -------------------------------------------
# moving window to calculate expected T

# tempdata = should be df with 'Site', 'Temperature', 'Date' cols
# note: this function calcs stress for each site (and does not 
# break down by year)

stress1_fun <- function(tempdata,site_vector){
  
  # --- testing this function --- #
  # tempdata = temp
  
  # --- end testing --- #
  
  
  
  wts <- c(0.1,0.2,0.4,0.8)
  wts <- wts/sum(wts) #make sure weights add up to 1
  wtsp <- round(wts*100,digits=0)
  window <- length(wtsp)
  
  stress_list <- as.list(rep(NA,length=length(site_vector)))
  names(stress_list) <- site_vector
  
  for(s in 1:length(site_vector)){
    
    d = tempdata[tempdata$Site==site_vector[s],]
     
    # -- Calculate Expected T in rolling window -- #
   
    expT <- data.frame(
      expectedT = rep(NA,length=length(d$Date)-window),
      Date = rep(NA,length=length(d$Date)-window))
    
    
    for(i in 1:(length(d$Date)-window)){ #step through dates
      
      # subset temp data to window
      tsub <- d[i:(i+window-1),]
      tsub$dayseq <- c(1:window)
      tsub$wtsp <- wtsp
      
      # add more temp observations in proportion to their weight
      texp <- tsub[rep(row.names(tsub),tsub$wtsp),1:ncol(tsub)-1]
      
      # calc regression, predict 'expected T' on last day in window
      expT$expectedT[i] <- predict(lm(data=texp,Temperature~dayseq),
                                   list(dayseq=tsub$dayseq[window]))
      expT$Date[i] <- as.character(tsub$Date[window])
    } # close date loop
    
    # format cols in 
    expT$Date <- as.Date(expT$Date,format="%Y-%m-%d")
    d$Date <- as.Date(d$Date,format="%Y-%m-%d")
    d <- as.data.frame(d)
    d2 <- left_join(d,expT,by="Date")
    d2$Dstress <- d2$Temperature - d2$expectedT
    
    stress <- d2
    rm(i,tsub,texp,d2,d)
    
    # make NA vals 0
    stress$Dstress[is.na(stress$Dstress)] <- 0
    stress$expectedT[is.na(stress$expectedT)] <- 0
    stress_list[[s]] <- stress
    
    # ---
    # comment out the integration, do this outside the function
    # ---
    # # integrate under short term variability stress curve
    # yy <- abs(stress[stress$Site==site_vector[s],]$Dstress)
    # yy <- yy[!is.na(yy)]
    # xx <- seq(from=1,to=length(yy),by=1)
    # site_stress_values[site_stress_values$Site == site_vector[s],]$integral_m1 <- trapz(x=xx,y=yy)
    # site_stress_values[site_stress_values$Site == site_vector[s],]$days_in_ts_m1 <- length(xx)
    # rm(yy,xx,stress)
    
  } # close site loop
  
  tempdata_stress1 <- do.call("rbind",stress_list)
  tempdata_stress1$stress1_pos_vals <- abs(tempdata_stress1$Dstress)
  rm(stress_list,wts,wtsp,s,stress,window)
  
  return(tempdata_stress1)
  
}

