# ------------------------
# CPUE vs SST around Isla Natividad
# ------------------------

# Plan
# 1) read in data
# 2) plot sst per block
# 3) convert sst to bottom temp
# 4) run stress method functions
# 5) plot stress methods

library(tidyverse)
library(ggplot2)
library(lubridate)
library(pracma)
# ------------------------


# ------------------------
# 1) read in data: SST in blocks
sst.clim <- read.csv(file="C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/data/blocks_may21/All_Block_Clims.csv")

sst.temp <- read.csv(file="C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/data/blocks_may21/All_Block_Temps.csv")

sst.clim <- sst.clim %>% gather(key="Site",value="Climatology",2:7)
sst.temp <- sst.temp %>% gather(key="Site",value="temp_sst_raw",2:7)
sst.blks <- left_join(sst.clim,sst.temp,by=c("Site","Date"))
sst.blks$Date <- as.Date(sst.blks$Date,format="%m/%d/%Y")
sst.blks <- sst.blks %>% mutate(year = year(Date))
sst.blks$flag <- ifelse(is.na(sst.blks$temp_sst_raw),"1","0")
rm(sst.clim,sst.temp)

# read in CPUE block data
cpue <- read.csv(file="C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/data/blocks_may21/cpue.csv")
cpue <- cpue %>% select(year,zona,CPUE_raw)
colnames(cpue)[colnames(cpue)=="CPUE_raw"] <- "CPUE"
cpue$Site <- paste("Zone_",cpue$zona,sep="")


# ------------------------
# 2) plot sst per block
ggplot(data=sst.blks) + 
  geom_line(aes(x=Date,y=Temperature)) +
  facet_wrap(facets = vars(Site))

# 2a) plot cpue per block
ggplot(data=cpue) + 
  geom_line(aes(x=year,y=CPUE)) +
  facet_wrap(facets = vars(Site))

# ------------------------
# 3) convert sst to bottom temp
site_vector <- unique(sst.blks$Site) # use in for loops
years <- unique(sst.blks$year) # use in for loops

# create empty cols for building the simulated time series
sst.blks$highfreqnoise <- rep(NA,length=length(sst.blks[,1]))
sst.blks$climatology_minus_2.5offset <- rep(NA,length=length(sst.blks[,1]))
sst.blks$mSST <- rep(NA,length=length(sst.blks[,1]))

# deviations of raw SST temp from climatology:
sst.blks$dev_10 <- sst.blks$Climatology - sst.blks$temp_sst_raw 


# calc high freq in SST, then use equation and convert to bottom high
# freq variance -- and store high freq var values in this df
converted_bottomdf <- data.frame(
  Site = rep(site_vector,length=length(site_vector)*length(years)),
  year= rep(years,length=length(site_vector)*length(years)),
  highfreq_SST = rep(NA,length=length(site_vector)*length(years)),
  converted_bottom_lmeq = rep(NA,length=length(site_vector)*length(years)),
  converted_bottom_logeq = rep(NA,length=length(site_vector)*length(years))
)


for(s in 1:length(site_vector)){ #for each site
  
  for(y in 1:length(years)){ #step through each year
    
    # calculate SST high freq variance for that year-site using the raw temp deviations
    SST_high_freq <- var(sst.blks[sst.blks$Site==site_vector[s] & sst.blks$year==years[y], ]$dev_10,na.rm=TRUE)
    
    # store the SST high freq var in converted_bottomdf data frame
    converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$highfreq_SST <- SST_high_freq
    
    
    # --- CHOOSE WHICH REGRESSION TO USE --- #
    # regression that uses all sites, or a subset of sites?
    # convert the SST high freq variance into high freq bottom temp variance
    
    new_bottom_var_log <-
      as.numeric((log10(SST_high_freq) - m_all_log$coefficients[1])/m_all_log$coefficients[2])
    new_bottom_var <- 10^new_bottom_var_log
    
    new_bottom_var_lm <-
      (SST_high_freq - m_strong$coefficients[1])/(m_strong$coefficients[2])
    
    # -------------------------------------- #
    
    # store the new bottom temp high freq var (log eg)
    converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_logeq <- new_bottom_var
    
    # store the new bottom temp high freq var (lm eq)
    converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_lmeq <- new_bottom_var_lm
    print(y)
  }
  print(s)
}
rm(s,y,SST_high_freq,new_bottom_var,new_bottom_var_log,new_bottom_var_lm)

# -------------------------------------- #
# -- GENERATE BOTTOM TEMP TIME SERIES -- #

# generate time series of bottom temp with converted high freq var
set.seed(42) # the answer to the universe :)

for (s in 1:length(site_vector)){ # for each site
  for (y in 1:length(years)){ # in each year
    
    # random number generation with mean=0 and sd=sqrt of converted bottom var
    sst.blks[sst.blks$Site==site_vector[s] & sst.blks$year==years[y],]$highfreqnoise <- 
      rnorm(n=length(sst.blks[sst.blks$Site==site_vector[s] & sst.blks$year==years[y],]$highfreqnoise),
            mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_logeq)
      ) # close rnorm generator 
    
  } # close year loop
  print(s)
} # close site loop
rm(s,y) # clean up
head(sst.blks) # check big df, did the cols fill in?
#summary(sst.blks)


# now that I have simulated bottom temp high freq variability in all years-sites,
# I must adjust SST climatology by the 2.5 deg C offset. 
sst.blks$climatology_minus_2.5offset <- sst.blks$Climatology - 2.5
#sst.blks$climatology_minus_2.5offset <- sst.blks$Climatology - 0

# last step to creating simulated bottom temp (filling in the mSST column):
# add in the simulated high freq variability to the climatology-offset time series
sst.blks$mSST <- sst.blks$climatology_minus_2.5offset + sst.blks$highfreqnoise




# ------------------------
# 3) run stress method functions on sst
#    get stress value per year, per block
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress1_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress2_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress3_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress4_function.r")

s1 <- stress1_fun(tempdata = sst.blks,site_vector = unique(sst.blks$Site)) 
s2 <- stress2_fun(tempdata = sst.blks,site_vector = unique(sst.blks$Site)) 
s3 <- stress3_fun(tempdata = sst.blks,site_vector = unique(sst.blks$Site)) 
s4 <- stress4_fun(tempdata = sst.blks,site_vector = unique(sst.blks$Site)) 
s4 <- s4$tempdata_stress4

s1$year <- year(s1$Date)
s3$year <- year(s3$Date)
s4$year <- year(s4$Date)

# create empty df for stress vals per year, per site/zone
yList <- as.list(rep(NA,length=length(unique(sst.blks$year))))
for(y in 1:length(yList)){
  yList[[y]] <- 
    data.frame(
      Site = unique(sst.blks$Site),
      year = rep(unique(sst.blks$year)[y],length=length(unique(sst.blks$Site))),
      val = rep(NA,length=length(unique(sst.blks$Site)) ) ) 
}
dd <-bind_rows(yList,.id=NULL) 
mList <- as.list(rep(NA,length=4))
for(m in 1:4){
  d <- dd
  d$method <- rep(paste("method_",m,sep=""),length(d[,1]))
  mList[[m]] <- d
  rm(d)  }
stress_vals <- bind_rows(mList,.id=NULL)
stress_vals$val_relative <- rep(NA,length=length(stress_vals[,1]))
rm(y,yList,dd,mList,m)


# stress 1: integrate daily stress vals per year
for(s in 1:length(unique(s1$Site))){
  for(y in 1:length(unique(s1$year))){
    d <- s1[s1$Site==unique(s1$Site)[s] & s1$year==unique(s1$year)[y],]
    xx <- seq(from=1,to=length(d[,1]),by=1)
    yy <- d$stress1_pos_vals
    stress_vals[stress_vals$Site==unique(s1$Site)[s] &
                stress_vals$year==unique(s1$year)[y] &
                stress_vals$method=="method_1",]$val <- trapz(x=xx,y=yy)
    
  }
}
rm(d,xx,yy,s,y)
# calc relative stress values (across sites & years, for method 1)
stress_vals[stress_vals$method=="method_1",]$val_relative <- 
  (stress_vals[stress_vals$method=="method_1",]$val)/max(stress_vals[stress_vals$method=="method_1",]$val)


# stress 2: integrate daily stress vals per year
for(s in 1:length(unique(s2$Site))){
  for(y in 1:length(unique(s2$year))){
    d <- s2[s2$Site==unique(s2$Site)[s] & s2$year==unique(s2$year)[y],]
    xx <- seq(from=1,to=length(d[,1]),by=1)
    yy <- d$stress2_degC_above_threshold
    stress_vals[stress_vals$Site==unique(s2$Site)[s] &
                stress_vals$year==unique(s2$year)[y] &
                stress_vals$method=="method_2",]$val <- trapz(x=xx,y=yy)
    
  }
}
rm(d,xx,yy,s,y)
# calc relative stress values (across sites & years, for method 2)
stress_vals[stress_vals$method=="method_2",]$val_relative <- 
  (stress_vals[stress_vals$method=="method_2",]$val)/max(stress_vals[stress_vals$method=="method_2",]$val)

# stress 3: integrate daily stress vals per year
for(s in 1:length(unique(s3$Site))){
  for(y in 1:length(unique(s3$year))){
    d <- s3[s3$Site==unique(s3$Site)[s] & s3$year==unique(s3$year)[y],]
    xx <- seq(from=1,to=length(d[,1]),by=1)
    yy <- d$stress3_degC_above_thres_clim_max_mw
    stress_vals[stress_vals$Site==unique(s3$Site)[s] &
                  stress_vals$year==unique(s3$year)[y] &
                  stress_vals$method=="method_3",]$val <- trapz(x=xx,y=yy)
    
  }
}
rm(d,xx,yy,s,y)
# calc relative stress values (across sites & years, for method 3)
stress_vals[stress_vals$method=="method_3",]$val_relative <- 
  (stress_vals[stress_vals$method=="method_3",]$val)/max(stress_vals[stress_vals$method=="method_3",]$val)

# stress 4: integrate daily stress vals per year
for(s in 1:length(unique(s4$Site))){
  for(y in 1:length(unique(s4$year))){
    d <- s4[s4$Site==unique(s4$Site)[s] & s4$year==unique(s4$year)[y],]
    xx <- seq(from=1,to=length(d[,1]),by=1)
    yy <- d$stress4_dev
    stress_vals[stress_vals$Site==unique(s4$Site)[s] &
                  stress_vals$year==unique(s4$year)[y] &
                  stress_vals$method=="method_4",]$val <- trapz(x=xx,y=yy)
    
  }
}
rm(d,xx,yy,s,y)
# calc relative stress values (across sites & years, for method 4)
stress_vals[stress_vals$method=="method_4",]$val_relative <- 
  (stress_vals[stress_vals$method=="method_4",]$val)/max(stress_vals[stress_vals$method=="method_4",]$val)


# ------------------------
# 4) plot stress methods
cpue$Site <- paste("Zone_",cpue$zona,sep="")
cpue <- cpue %>% select(year,CPUE,Site)
stress_vals <- stress_vals %>% select(Site,year,method,val_relative)
stress_vals1 <- stress_vals %>% spread(key=method,value=val_relative)
stress_cpue <- full_join(stress_vals1,cpue,by=c("Site","year"))
stress_cpue <- stress_cpue %>% gather("method_no","stressval",3:6)

# x = method val, y = cpue, facet by method num
ggplot(data=stress_cpue) +
  geom_point(aes(x=stressval,y=CPUE,color=Site)) +
  facet_wrap(vars(method_no))
  #facet_grid(rows = vars(Site), cols = vars(method_no))

