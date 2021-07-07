# -------------------------------------------
# Method 4: thermal performance curve
# -------------------------------------------


stress4_fun <- function(tempdata,site_vector){
  
  # temporary for testing ----
  
   # tempdata = mSST_temp %>% filter(year==years[1])
   # site_vector = site_vector
  # --------------------------
  
  # params from Brock and the paper he found
  alpha=0.75
  E_r=0.65
  E_i=2.035
  k=8.62e-5
  Tmax=25
  
  TT=seq(from=0,to=50,by=.01)
  
  Topt_sites <- data.frame(
    Site = site_vector,
    Topt = rep(NA,length=length(site_vector)))
  
  for(s in 1:length(site_vector)){
    Topt_sites[Topt_sites$Site == site_vector[s],]$Topt <-
      mean(tempdata[tempdata$Site==site_vector[s],]$Temperature,rm.na=TRUE)
    }
  
  TK=TT+273 # convert temp to Calvin
  
  Topt_sites$ToptK <- Topt_sites$Topt + 273 # add column of Calvin temps
  
  Topt_sites$scf1 <- 10^(-.03*(Tmax-Topt_sites$Topt)+.28)
  
  Topt_sites$E_if1 <- E_i*exp(Topt_sites$scf1)
  
  M=1
  
  curves <- as.list(rep(NA,length=length(site_vector)))
  names(curves) <- site_vector
  
  for(s in 1:length(curves)){
    
    P01 <- exp(-E_r/(k*TK))*(1+(E_r/(Topt_sites[Topt_sites$Site==site_vector[s],]$E_if1-E_r)*exp(Topt_sites[Topt_sites$Site==site_vector[s],]$E_if1*(1/(k*(Topt_sites[Topt_sites$Site==site_vector[s],]$ToptK))-1/(k*TK)))))^-1
    
    P1 <- P01*M^alpha
    
    P1=1-P1/max(P1)
    
    curves[[s]] <- data.frame(Site=rep(site_vector[s],length(P01)),perf=P01,stress=P1)
  }
  rm(P01,P1,s)
  
  curves <- bind_rows(curves)
  
  # -----------------------------------------------------------
  # stress time series using the inverted thermal performance curve
  
  Topt_sites$stress_degday <- rep(NA,length(Topt_sites[,1]))
  
  #stress4_timeseries <- as.list(rep(NA,length=length(site_vector)))
  #names(stress4_timeseries) <- site_vector
  
  tempdata$stress4_dev <- rep(NA,length(tempdata$Site)) # save 
  tempdata$stress4_abs <- rep(NA,length(tempdata$Site))
  
  for(s in 1:length(site_vector)){ # for each site..
    
    # subset data to get Temperature time series
    T1 <- tempdata[tempdata$Site==site_vector[s],]$Temperature
    
    # calculate the value of 'performance' based on temperature using equations (modified from Barneche etal 2014)
    P0=exp(-E_r/(k*(T1+273)))*(1+(E_r/(Topt_sites[Topt_sites$Site==site_vector[s],]$E_if1-E_r)*exp(Topt_sites[Topt_sites$Site==site_vector[s],]$E_if1*(1/(k*(Topt_sites[Topt_sites$Site==site_vector[s],]$Topt+273))-1/(k*(T1+273))))))^-1
    
    # invert 'performance' values (high performance = low stress),
    # and standardize from 0 to 1
    # this is the daily stress factor
    daily_stress=1-P0/max(P0)
    
    # daily stress factor * daily deviation from Topt (the mean)
    stress1_ts=abs(daily_stress*(T1 - Topt_sites[Topt_sites$Site==site_vector[s],]$Topt) )
    
    # daily stress factor * daily actual temperature
    stress2_ts=abs(daily_stress * T1  )
    
    # save both ts output in tempdata
    tempdata[tempdata$Site==site_vector[s],]$stress4_dev <- stress1_ts
    tempdata[tempdata$Site==site_vector[s],]$stress4_abs <- stress2_ts
    
    # save df: daily stress, date
    # savestress <- data.frame(
    #   Date = tempdata[tempdata$Site==site_vector[s],]$Date,
    #   stress4 = stress1_ts)
    # 
    # stress4_timeseries[[site_vector[s]]] <- savestress
    
    
    
  }
  rm(s,T1,P0,stress1_ts,stress2_ts,daily_stress)
  
  rm(M,alpha,E_r,E_i,k,TK,Tmax,TT,Topt_sites)
  
  tempdata_stress4 = as.data.frame(tempdata)
  
  tempdata_and_curves = list(tempdata_stress4,curves)
  names(tempdata_and_curves) <- c("tempdata_stress4","curves")
  rm(tempdata,tempdata_stress4,curves)
  
  return(tempdata_and_curves)
}
