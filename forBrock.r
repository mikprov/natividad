

# Comparing spectra of 10 min and random grabs of daily bottom temp


library(tidyverse)
library(grid)
library(gridExtra)
library(cowplot)

# -- read in 10 min interval data -- #

d <- read.csv(file="C:/Users/Mikaela/Documents/GitHub/natividad/data/001_IslaNatividad_MorroPrietoPuntaPrieta_alldata_trimmed.csv",stringsAsFactors = FALSE)
d$Date <- as.Date(d$UTC_Date_Time,format="%Y-%m-%d")

bottom_10_MPPP <- d %>% #subset MP & PP data
  filter(Site %in% c('MorroPrieto','PuntaPrieta'), Instrument_Type == 'MiniDOT') %>% 
  select(Site,Temperature,Date,UTC_Date_Time,Year)
table(bottom_10_MPPP$Site,bottom_10_MPPP$Year)

bottom_daily_MPPP <- d %>% #subset MP & PP data, randomly grab 1 observation
  filter(Site %in% c('MorroPrieto','PuntaPrieta'), Instrument_Type == 'MiniDOT') %>% 
  group_by(Site,Date) %>% 
  sample_n(1) %>% 
  select(Site,Temperature,Date)

calc_spec <- function(ts,sampleinterval) {
  
  # ----
  # testing the function with these inputs 
  # time series:
  #ts = bottom_10_MPPP[bottom_10_MPPP$Site == "MorroPrieto",]$Temperature
  
  # sample interval:
  #sampleinterval <- 10/1440 
  # to convert freq at 10 min obs in terms of days,
  # divide freq values by the sample interval which is (10min)/(1440min in day)
  # ----
  
  
  
  # -- set smoothers -- #
  
  # m is the square root of the time series length; if it is an even number, make odd
  tmp <- ceiling(sqrt(length(1:length(ts)))) 
  if (tmp %% 2 == 0) {m <- tmp+1} else {m <- tmp} 
  #m
  
  # -- calc spectrum -- #
  
  spR <- spec.pgram(ts,spans=NULL,taper=0.1,plot = FALSE,demean = TRUE) # spectrum w/o smoother
  spS <- spec.pgram(ts,spans=m,taper=0.1,plot = FALSE,demean = TRUE) # spectrum w/smoother
  
  
  # -- process spectra output for plotting (raw & smoothed spectra) -- #
  
  freq_in_days_R <- spR$freq/sampleinterval  #convert freq in terms of days
  spec_x_freq_R <- spR$spec*(freq_in_days_R) #multiply spec by freq
  
  freq_in_days_S <- spS$freq/sampleinterval  #convert freq in terms of days
  spec_x_freq_S <- spS$spec*(freq_in_days_S) #multiply smoothed spec by freq
  
  
  # -- plot raw spectrum -- #
  df_R <- data.frame(
    spx = freq_in_days_R,
    spy = spec_x_freq_R,
    specType = rep("raw",length=length(freq_in_days_R)),
    stringsAsFactors = FALSE)
  df_S <- data.frame(
    spx = freq_in_days_S,
    spy = spec_x_freq_S,
    specType = rep("smoothed",length=length(freq_in_days_S)),
    stringsAsFactors = FALSE)
  
  specdf <- bind_rows(df_R,df_S,.id=NULL)
  rm(m,ts,spR,spS,freq_in_days_S,freq_in_days_R,spec_x_freq_R,spec_x_freq_S,df_R,df_S,tmp)
  return(specdf)
  
}

# -- try 10 min data at MP -- # 

spec_MP_10min <- calc_spec(ts = bottom_10_MPPP[bottom_10_MPPP$Site == "MorroPrieto",]$Temperature,
                sampleinterval = 10/1440 ) 

ggplot(data=spec_MP_10min[spec_MP_10min$spx < 2.5 & #plot only cycles <2.5 per day
                          spec_MP_10min$specType == "raw",]) + #raw spec (not smoothed)
  geom_line(aes(x=spx,y=log(spy))) + #log the spec*freq values
  ggtitle("Morro Prieto (10 min)") + 
  xlab("freq (units = cycles per day)") +
  ylab("log10(spec*freq)") 

ggplot(data=spec_MP_10min[spec_MP_10min$spx < 2.5 & #plot only cycles <2.5 per day
                            spec_MP_10min$specType == "smoothed",]) + #smoothed spec 
  geom_line(aes(x=spx,y=log(spy))) + #log the spec*freq values
  ggtitle("Morro Prieto (10 min)") + 
  xlab("freq (units = cycles per day)") +
  ylab("log10(spec*freq)") 


# -- try random daily temperature at MP -- # 

spec_MP_daily <- calc_spec(ts = bottom_daily_MPPP[bottom_daily_MPPP$Site == "MorroPrieto",]$Temperature,
                           sampleinterval = 1 ) 

ggplot(data=spec_MP_daily[spec_MP_daily$spx > 0.45 & #focus in on freq >0.45
                          spec_MP_daily$specType == "raw",]) + #raw spec (not smoothed)
  geom_line(aes(x=spx,y=log(spy))) + #log the spec*freq values
  ggtitle("Morro Prieto (daily) ") + 
  xlab("freq (units = cycles per day)") +
  ylab("log10(spec*freq)") 

ggplot(data=spec_MP_daily[spec_MP_daily$spx > 0.45 & #focus in on freq >0.45
                          spec_MP_daily$specType == "smoothed",]) + #smoothed spec 
  geom_line(aes(x=spx,y=log(spy))) + #log the spec*freq values
  ggtitle("Morro Prieto (daily)") + 
  xlab("freq (units = cycles per day)") +
  ylab("log10(spec*freq)") 
