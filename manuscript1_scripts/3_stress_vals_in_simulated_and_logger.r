# ---------------------- #
# -- STRESS FUNCTIONS -- #

# Do the 4 stress tests on temp logger data and simulated bottom series
# compare output at each site, for each stress method

#source("C:/Users/Mikaela/Documents/GitHub/natividad/functions.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress1_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress2_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress3_function.r")
source("C:/Users/Mikaela/Documents/GitHub/natividad/stress_functions/stress4_function.r")

# ---
# notes:
# 1) stress functions don't loop through years, I need to do this
# outside the function
#
# 2) cols I need for stress functions: Site, Temperature, Date, Year
# 'Temperature' could be bottom, SST, modified SST
#
# 3) output from stress functions: each spits out a data frame with
# daily stress values
#
# 4) the sst_bottom data frame has multiple temperature cols, I
# should subset the relevant columns and change headers bc
# functions require consistent header info
# ---


# ---
# Stress on simulated bottom temp (mSST - version 2):
# ---
mSST_temp <- SSTfede %>% select(Site,Date,year,mSST_version2) #only relevant cols
colnames(mSST_temp)[colnames(mSST_temp)=="mSST_version2"] <- "Temperature"
stress1 <- stress1_fun(tempdata = mSST_temp, site_vector = site_vector)
stress2 <- stress2_fun(tempdata = mSST_temp, site_vector = site_vector)
stress3 <- stress3_fun(tempdata = mSST_temp, site_vector = site_vector)
stress4 <- stress4_fun(tempdata = mSST_temp, site_vector = site_vector)
stress4 <- stress4$tempdata_stress4

# ---
# modify the stressX dataframes
# ---
stress1 <- stress1 %>% 
  select(Site,Date,year,Temperature,stress1_pos_vals) %>%
  mutate(method="stress1") %>%
  mutate(regtype="version2")
colnames(stress1)[colnames(stress1)=="stress1_pos_vals"] <- "daily_stress"

stress2 <- stress2 %>% 
  select(Site,Date,year,Temperature,stress2_degC_above_threshold) %>%
  mutate(method="stress2") %>%
  mutate(regtype="version2")
colnames(stress2)[colnames(stress2)=="stress2_degC_above_threshold"] <- "daily_stress"

stress3 <- stress3 %>% 
  select(Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw) %>%
  mutate(method="stress3") %>%
  mutate(regtype="version2")
colnames(stress3)[colnames(stress3)=="stress3_degC_above_thres_clim_max_mw"] <- "daily_stress"

stress4 <- stress4 %>% 
  select(Site,Date,year,Temperature,stress4_dev) %>%
  mutate(method="stress4") %>%
  mutate(regtype="version2")
colnames(stress3)[colnames(stress4)=="stress4_dev"] <- "daily_stress"

stress_version2 <- bind_rows(stress1,stress2,stress3,stress4,.id=NULL)




# ---
# Stress on simulated bottom temp (mSST):
# ---
mSST_temp <- SSTfede %>% select(Site,Date,year,
                                mSST_lm_strong,mSST_lm_weak,
                                mSST_log,mSST_log_strong,mSST_lm_all) #only relevant cols

years <- unique(mSST_temp$year) #list of years in dataset
site_vector <- unique(mSST_temp$Site)


# ---
# Run this for site specific variance
# ---
# run stress functions 1-4 on strong, weak, origin, origin-strong, log simulations
# 4 * 5 = 20 functions total



# (1) lm_strong
temp_lm_strong <- mSST_temp %>% 
  select(Site,Date,year,mSST_lm_strong) %>%
  mutate(regtype=rep("lm_strong"))
colnames(temp_lm_strong)[colnames(temp_lm_strong)=="mSST_lm_strong"] <- "Temperature" #renaming

# (2) lm_weak
temp_lm_weak <- mSST_temp %>% 
  select(Site,Date,year,mSST_lm_weak) %>%
  mutate(regtype=rep("lm_weak"))
colnames(temp_lm_weak)[colnames(temp_lm_weak)=="mSST_lm_weak"] <- "Temperature" #renaming

# (3) log
temp_log <- mSST_temp %>% 
  select(Site,Date,year,mSST_log) %>%
  mutate(regtype=rep("log_all"))
colnames(temp_log)[colnames(temp_log)=="mSST_log"] <- "Temperature" #renaming

# (4) log_strong
temp_log_strong <- mSST_temp %>% 
  select(Site,Date,year,mSST_log_strong) %>%
  mutate(regtype=rep("log_strong"))
colnames(temp_log_strong)[colnames(temp_log_strong)=="mSST_log_strong"] <- "Temperature" #renaming

# (5) lm_all
temp_lm_all <- mSST_temp %>% 
  select(Site,Date,year,mSST_lm_all) %>%
  mutate(regtype=rep("lm_all"))
colnames(temp_lm_all)[colnames(temp_lm_all)=="mSST_lm_all"] <- "Temperature" #renaming

# merge all dataframes for running stress functions
temp_all_regressions <- rbind(temp_lm_strong,
                              temp_lm_weak,
                              temp_lm_all,
                              temp_log,
                              temp_log_strong)
rm(temp_lm_strong,
   temp_lm_weak,
   temp_lm_all,
   temp_log,
   temp_log_strong)


# run this loop to calculate stress values for each
# stress method (1-4) using temperature calculated from the
# 5 different regression types.

regtypes <- unique(temp_all_regressions$regtype)
# store output from all stress functions
stress_outputL <- as.list(rep(NA,length(regtypes)))

# store threshold values for each site (methods 2,3)
thresh_vals2L <- as.list(rep(NA,length(regtypes)))
thresh_vals3L <- as.list(rep(NA,length(regtypes)))

# store stress2 & stress3 full function output for trouble shooting
stress2_outputL <- as.list(rep(NA,length(regtypes)))
stress3_outputL <- as.list(rep(NA,length(regtypes)))

old <- Sys.time() # get start time

for(r in 1:length(regtypes)){ # for each regression type (there are 5)
  
  temp <- temp_all_regressions[temp_all_regressions$regtype==regtypes[r],]
  
  # run stress 1 
  temp_stress1 <- stress1_fun(tempdata = temp, site_vector = site_vector)
  temp_stress11 <- temp_stress1 %>% 
    select(Site,Date,year,Temperature,regtype,stress1_pos_vals) %>%
    mutate(method = rep("stress1"))
  colnames(temp_stress11)[colnames(temp_stress11)=="stress1_pos_vals"] <- "daily_stress"
  rownames(temp_stress11) <- c() #rm rownames
  print("stress1")
  
  # run stress 2
  temp_stress2 <- stress2_fun(tempdata = temp, site_vector = site_vector)
  temp_stress22 <- temp_stress2 %>% 
    select(Site,Date,year,Temperature,regtype,stress2_degC_above_threshold) %>%
    mutate(method = "stress2")
  colnames(temp_stress22)[colnames(temp_stress22)=="stress2_degC_above_threshold"] <- "daily_stress"
  rownames(temp_stress22) <- c() #rm rownames
  
  stress2_outputL[[r]] <- temp_stress2 # save stress fun output
  thresh_vals2L[[r]] <- temp_stress2 %>% # save threshold values
    select(Site,threshold) %>% distinct() %>% 
    mutate(regtype=rep(regtypes[r])) %>%
    mutate(method=rep('stress2'))
  print("stress2")
  
  # run stress 3
  temp_stress3 <- stress3_fun(tempdata = temp, site_vector = site_vector)
  temp_stress33 <- temp_stress3 %>% 
    select(Site,Date,year,Temperature,regtype,stress3_degC_above_thres_clim_max_mw) %>%
    mutate(method = "stress3")
  colnames(temp_stress33)[colnames(temp_stress33)=="stress3_degC_above_thres_clim_max_mw"] <- 
    "daily_stress"
  rownames(temp_stress33) <- c() #rm rownames
  
  stress3_outputL[[r]] <- temp_stress3 # save stress fun output
  thresh_vals3L[[r]] <- temp_stress3 %>% # save threshold values
    select(Site,threshold) %>% distinct() %>% 
    mutate(regtype=rep(regtypes[r])) %>%
    mutate(method=rep('stress3'))
  print("stress3")
  
  # run stress 4
  temp_stress4 <- stress4_fun(tempdata = temp, site_vector = site_vector)
  temp_stress4 <- temp_stress4$tempdata_stress4
  temp_stress44 <- temp_stress4 %>% 
    select(Site,Date,year,Temperature,regtype,stress4_dev) %>%
    mutate(method = "stress4")
  colnames(temp_stress44)[colnames(temp_stress44)=="stress4_dev"] <- "daily_stress"
  print("stress4")
  
  # combine temp_stress11,22,33,44 and store
  stress_outputL[[r]] <- bind_rows(temp_stress11,temp_stress22,temp_stress33,temp_stress44)
  print(r)
}

new <- Sys.time() - old
print(new)
rm(new, old)

mSST_stress <- bind_rows(stress_outputL,.id=NULL)
mSST_stress$datatype <- rep("mSST",length=nrow(mSST_stress))

rm(r,temp_stress1,temp_stress2,temp_stress3,temp_stress4,
   temp_stress11,temp_stress22,temp_stress33,temp_stress44)



# ---
# trouble-shooting method 2&3 bc stress values using loggers and mSST don't correlate
# ---
stress2_output <- bind_rows(stress2_outputL,.id=NULL)
head(stress2_output) # 'Temperature'col is mSST using the 'regtype'

ptemps_lmL <- as.list(rep(NA,length(site_vector)))
ptemps_logL <- as.list(rep(NA,length(site_vector)))

for(s in 1:length(site_vector)){
  mSST_2_lm <- stress2_output[stress2_output$Site==site_vector[s] & 
                                stress2_output$regtype=="lm_all",] 
  mSST_2_log <- stress2_output[stress2_output$Site==site_vector[s] & 
                                 stress2_output$regtype=="log_all" ,] 
  
  sst_d <- SSTfede[SSTfede$Site==site_vector[s],]
  
  bot_d <- logger_temp[logger_temp$Site==site_vector[s],]
  
  stress2_logger <- logger_stress[logger_stress$Site==site_vector[s] &
                                  logger_stress$method=="stress2",]
  
  # grab dates in mSST df and sst df that are in bottom df
  mSST_2_lm <-  mSST_2_lm[mSST_2_lm$Date %in% bot_d$Date,] 
  mSST_2_log <- mSST_2_log[mSST_2_log$Date %in% bot_d$Date,] 
  sst_d <- sst_d[sst_d$Date %in% bot_d$Date,]
  
  # plot mSST (lm_all), sst, bottom
  colors <- c("mSST" = "grey50", "sst" = "grey90", "bottom" = "dodgerblue")
  ptemps_lmL[[s]] <- ggplot() +
    geom_line(data=sst_d,aes(x=Date,y=Temperature,color="sst")) +
    geom_line(data=sst_d,aes(x=Date,y=Climatology,color="sst")) +
    geom_line(data=bot_d,aes(x=Date,y=Temperature,color="bottom")) +
    geom_line(data=mSST_2_lm,aes(x=Date,y=Temperature,color="mSST")) +
    geom_line(data=mSST_2_lm,aes(x=Date,y=climatology,color="mSST")) +
    
   
    theme_classic() + ylim(c(8,27)) +
    scale_color_manual(values=colors) +
    ggtitle(paste(site_vector[s]," - linear reg., fit to all sites",sep="")) +
    labs(x="",y="Temperature",color="")
    
  
  # plot mSST (log_all), sst, bottom
  colors <- c("mSST" = "navajowhite4", "sst" = "grey90", "bottom" = "dodgerblue")
  ptemps_logL[[s]] <- ggplot() +
    geom_line(data=sst_d,aes(x=Date,y=Temperature,color="sst")) +
    geom_line(data=sst_d,aes(x=Date,y=Climatology,color="sst")) +
    geom_line(data=bot_d,aes(x=Date,y=Temperature,color="bottom")) +
    geom_line(data=mSST_2_log,aes(x=Date,y=Temperature,color="mSST")) +
    geom_line(data=mSST_2_log,aes(x=Date,y=climatology,color="mSST")) +
    theme_classic() + ylim(c(8,27)) +
    scale_color_manual(values=colors) +
    ggtitle(paste(site_vector[s]," - log reg., fit to all sites",sep="")) +
    labs(x="",y="",color="")
  
  
}

# export time series 
jpeg(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/figSX_A_comparing_time_series_lm_log.jpeg", res=400, height = 12, width = 8, units="in")

plot_grid(ptemps_lmL[[1]],ptemps_logL[[1]],
          ptemps_lmL[[2]],ptemps_logL[[2]],
          ptemps_lmL[[3]],ptemps_logL[[3]],
          ptemps_lmL[[4]],ptemps_logL[[4]],
          ptemps_lmL[[5]],ptemps_logL[[5]],
          nrow=5)
dev.off()

jpeg(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/figSX_B_comparing_time_series_lm_log.jpeg", res=400, height = 12, width = 8, units="in")
plot_grid(ptemps_lmL[[6]],ptemps_logL[[6]],
          ptemps_lmL[[7]],ptemps_logL[[7]],
          ptemps_lmL[[8]],ptemps_logL[[8]],
          ptemps_lmL[[9]],ptemps_logL[[9]],
          ptemps_lmL[[10]],ptemps_logL[[10]],
          nrow=5)
dev.off()

jpeg(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/figSX_C_comparing_time_series_lm_log.jpeg", res=400, height = 16, width = 8, units="in")
plot_grid(ptemps_lmL[[11]],ptemps_logL[[11]],
          ptemps_lmL[[12]],ptemps_logL[[12]],
          ptemps_lmL[[13]],ptemps_logL[[13]],
          ptemps_lmL[[14]],ptemps_logL[[14]],
          ptemps_lmL[[15]],ptemps_logL[[15]],
          ptemps_lmL[[16]],ptemps_logL[[16]],
          ptemps_lmL[[17]],ptemps_logL[[17]],
          nrow=7)
dev.off()

# plot mSST thresholds vs logger thresholds
head(stress2_output)
logger_stress2 <- stress2_fun(tempdata = logger_temp, site_vector = site_vector)

msst_thresholds <- stress2_output %>% 
  filter(regtype=="log_all") %>%
  select(Site,threshold,site_sd,site_mean) %>%
  distinct() %>%
  mutate(regtype=rep("log_all")) %>%
  mutate(datatype=rep("mSST"))

bot_thresholds <- logger_stress2 %>%
  select(Site,threshold,site_sd,site_mean) %>%
  distinct() %>%
  mutate(regtype=rep("logger")) %>%
  mutate(datatype=rep("logger"))

thresholds_df <- bind_cols(msst_thresholds,bot_thresholds)
thres1 <- ggplot(data=thresholds_df) +
  geom_point(aes(x=threshold1,y=threshold)) +
  geom_smooth(aes(x=threshold1,y=threshold),method="lm",se=FALSE) +
  labs(x="logger, thermal limit", y="mSST, thermal limit") +
  geom_abline(intercept = 0,slope=1) +
  theme_bw() + 
  ggtitle("Thermal Limit Comparison between\nlogger data and mSST (log_all)") +
  geom_text_repel(data=thresholds_df,
                  aes(x=threshold1,y=threshold,label=Site),segment.color = "grey",
                  size = 3,
                  na.rm = TRUE,
                  box.padding = 0.5)

sd1 <- ggplot(data=thresholds_df) +
  geom_point(aes(x=site_sd1,y=site_sd)) +
  geom_smooth(aes(x=site_sd1,y=site_sd),method="lm",se=FALSE) +
  labs(x="logger, stdev", y="mSST, stdev") +
  geom_abline(intercept = 0,slope=1) +
  theme_bw() + 
  ggtitle("Stdev Comparison between logger\ndata and mSST (log_all)") +
  geom_text_repel(data=thresholds_df,
                  aes(x=site_sd1,y=site_sd,label=Site),segment.color = "grey",
                  size = 3,
                  na.rm = TRUE,
                  box.padding = 0.5)


mean1 <- ggplot(data=thresholds_df) +
  geom_point(aes(x=site_mean1,y=site_mean)) +
  geom_smooth(aes(x=site_mean1,y=site_mean),method="lm",se=FALSE) +
  labs(x="logger, mean T", y="mSST, mean T") +
  geom_abline(intercept = 0,slope=1) +
  theme_bw() + 
  ggtitle("Mean T Comparison between logger\ndata and mSST (log_all)") +
  geom_text_repel(data=thresholds_df,
                  aes(x=site_mean1,y=site_mean,label=Site),segment.color = "grey",
                  size = 3,
                  na.rm = TRUE,
                  box.padding = 0.5)
jpeg(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/figSX_comparing_stress2_converstions_log_all.jpeg", res=400, height = 10, width = 5, units="in")
plot_grid(thres1,sd1,mean1,nrow=3)
dev.off()


# # stress 1 method
# mSST_stress1_lm_strong <- stress1_fun(tempdata = temp_lm_strong, site_vector = site_vector)
# mSST_stress1_lm_weak <- stress1_fun(tempdata = temp_lm_weak, site_vector = site_vector)
# mSST_stress1_log <- stress1_fun(tempdata = temp_log, site_vector = site_vector)
# mSST_stress1_log_strong <- stress1_fun(tempdata = temp_log_strong, site_vector = site_vector)
# mSST_stress1_lm_all <- stress1_fun(tempdata = temp_lm_all, site_vector = site_vector)
# 
# # stress 2 method
# mSST_stress2_lm_strong <- stress2_fun(tempdata = temp_lm_strong, site_vector = site_vector)
# mSST_stress2_lm_weak<- stress2_fun(tempdata = temp_lm_weak, site_vector = site_vector)
# mSST_stress2_lm_origin <- stress2_fun(tempdata = temp_lm_origin, site_vector = site_vector)
# mSST_stress2_lm_origin_strong <- stress2_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
# mSST_stress2_log <- stress2_fun(tempdata = temp_log, site_vector = site_vector)
# mSST_stress2_log_strong <- stress2_fun(tempdata = temp_log_strong, site_vector = site_vector)
# mSST_stress2_lm_all <- stress2_fun(tempdata = temp_lm_all, site_vector = site_vector)
# 
# # stress 3 method
# mSST_stress3_lm_strong <- stress3_fun(tempdata = temp_lm_strong, site_vector = site_vector)
# mSST_stress3_lm_weak <- stress3_fun(tempdata = temp_lm_weak, site_vector = site_vector)
# mSST_stress3_lm_origin <- stress3_fun(tempdata = temp_lm_origin, site_vector = site_vector)
# mSST_stress3_lm_origin_strong <- stress3_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
# mSST_stress3_log <- stress3_fun(tempdata = temp_log, site_vector = site_vector)
# mSST_stress3_log_strong <- stress3_fun(tempdata = temp_log_strong, site_vector = site_vector)
# mSST_stress3_lm_all <- stress3_fun(tempdata = temp_lm_all, site_vector = site_vector)
# 
# # stress 4 method
# output_lm_strong <- stress4_fun(tempdata = temp_lm_strong, site_vector = site_vector)
# output_lm_weak <- stress4_fun(tempdata = temp_lm_weak, site_vector = site_vector)
# output_lm_origin <- stress4_fun(tempdata = temp_lm_origin, site_vector = site_vector)
# output_lm_origin_strong <- stress4_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
# output_log <- stress4_fun(tempdata = temp_log, site_vector = site_vector)
# output_log_strong <- stress4_fun(tempdata = temp_log_strong, site_vector = site_vector)
# output_lm_all <- stress4_fun(tempdata = temp_lm_all, site_vector = site_vector)
# 
# mSST_stress4_lm_strong <-  output_lm_strong$tempdata_stress4
# mSST_stress4_lm_weak <- output_lm_weak$tempdata_stress4
# mSST_stress4_lm_origin <- output_lm_origin$tempdata_stress4
# mSST_stress4_lm_origin_strong <- output_lm_origin_strong$tempdata_stress4
# mSST_stress4_log <- output_log$tempdata_stress4
# mSST_stress4_log_strong <- output_log_strong$tempdata_stress4
# mSST_stress4_lm_all <- output_lm_all$tempdata_stress4


# ---
# Run this for site-year specific variance
# ---
# 
# mSST_stress1L_lm_strong <- as.list(rep(NA,length(years))) # loop over years, stress 1 method
# mSST_stress2L_lm_strong <- as.list(rep(NA,length(years))) # loop over years, stress 2 method
# mSST_stress3L_lm_strong <- as.list(rep(NA,length(years))) # loop over years, stress 3 method
# mSST_stress4L_lm_strong <- as.list(rep(NA,length(years))) # loop over years, stress 4 method
# 
# mSST_stress1L_lm_weak <- as.list(rep(NA,length(years))) # loop over years, stress 1 method
# mSST_stress2L_lm_weak <- as.list(rep(NA,length(years))) # loop over years, stress 2 method
# mSST_stress3L_lm_weak <- as.list(rep(NA,length(years))) # loop over years, stress 3 method
# mSST_stress4L_lm_weak <- as.list(rep(NA,length(years))) # loop over years, stress 4 method
# 
# mSST_stress1L_lm_origin <- as.list(rep(NA,length(years))) # loop over years, stress 1 method
# mSST_stress2L_lm_origin <- as.list(rep(NA,length(years))) # loop over years, stress 2 method
# mSST_stress3L_lm_origin <- as.list(rep(NA,length(years))) # loop over years, stress 3 method
# mSST_stress4L_lm_origin <- as.list(rep(NA,length(years))) # loop over years, stress 4 method
# 
# mSST_stress1L_lm_origin_strong <- as.list(rep(NA,length(years))) # loop over years, stress 1 method
# mSST_stress2L_lm_origin_strong <- as.list(rep(NA,length(years))) # loop over years, stress 2 method
# mSST_stress3L_lm_origin_strong <- as.list(rep(NA,length(years))) # loop over years, stress 3 method
# mSST_stress4L_lm_origin_strong <- as.list(rep(NA,length(years))) # loop over years, stress 4 method
# 
# mSST_stress1L_log <- as.list(rep(NA,length(years))) # loop over years, stress 1 method
# mSST_stress2L_log <- as.list(rep(NA,length(years))) # loop over years, stress 2 method
# mSST_stress3L_log <- as.list(rep(NA,length(years))) # loop over years, stress 3 method
# mSST_stress4L_log <- as.list(rep(NA,length(years))) # loop over years, stress 4 method
# 
# 
# old <- Sys.time() # get start time
# for(y in 1:length(years)){ # for each year, extract the noise data using the 5 conversions...
#   
#   # (1) lm_strong
#   temp_lm_strong <- mSST_temp %>% filter(year==years[y]) %>%
#     select(Site,Date,year,mSST_lm_strong)# filter simulated bottom to year y
#   colnames(temp_lm_strong)[colnames(temp_lm_strong)=="mSST_lm_strong"] <- "Temperature" #renaming
#   
#   # (2) lm_weak
#   temp_lm_weak <- mSST_temp %>% filter(year==years[y]) %>%
#     select(Site,Date,year,mSST_lm_weak) # filter simulated bottom to year y
#   colnames(temp_lm_weak)[colnames(temp_lm_weak)=="mSST_lm_weak"] <- "Temperature" #renaming
#   
#   # (3) lm_origin
#   temp_lm_origin <- mSST_temp %>% filter(year==years[y]) %>%
#     select(Site,Date,year,mSST_lm_origin)  # filter simulated bottom to year y
#   colnames(temp_lm_origin)[colnames(temp_lm_origin)=="mSST_lm_origin"] <- "Temperature" #renaming
#   
#   # (4) lm_origin_strong
#   temp_lm_origin_strong <- mSST_temp %>% filter(year==years[y]) %>%
#     select(Site,Date,year,mSST_lm_origin_strong) # filter simulated bottom to year y
#   colnames(temp_lm_origin_strong)[colnames(temp_lm_origin_strong)=="mSST_lm_origin_strong"] <- "Temperature" #renaming
#   
#   
#   # (5) log
#   temp_log <- mSST_temp %>% filter(year==years[y]) %>%
#     select(Site,Date,year,mSST_log) # filter simulated bottom to year y
#   colnames(temp_log)[colnames(temp_log)=="mSST_log"] <- "Temperature" #renaming
#   
#   # stress 1 method
#   mSST_stress1L_lm_strong[[y]] <-        stress1_fun(tempdata = temp_lm_strong, site_vector = site_vector)
#   mSST_stress1L_lm_weak[[y]] <-          stress1_fun(tempdata = temp_lm_weak, site_vector = site_vector)
#   mSST_stress1L_lm_origin[[y]] <-        stress1_fun(tempdata = temp_lm_origin, site_vector = site_vector)
#   mSST_stress1L_lm_origin_strong[[y]] <- stress1_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
#   mSST_stress1L_log[[y]] <-              stress1_fun(tempdata = temp_log, site_vector = site_vector)
#   
#   # stress 2 method
#   mSST_stress2L_lm_strong[[y]] <-        stress2_fun(tempdata = temp_lm_strong, site_vector = site_vector)
#   mSST_stress2L_lm_weak[[y]] <-          stress2_fun(tempdata = temp_lm_weak, site_vector = site_vector)
#   mSST_stress2L_lm_origin[[y]] <-        stress2_fun(tempdata = temp_lm_origin, site_vector = site_vector)
#   mSST_stress2L_lm_origin_strong[[y]] <- stress2_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
#   mSST_stress2L_log[[y]] <-              stress2_fun(tempdata = temp_log, site_vector = site_vector)
#   
#   # stress 3 method
#   mSST_stress3L_lm_strong[[y]] <-        stress3_fun(tempdata = temp_lm_strong, site_vector = site_vector)
#   mSST_stress3L_lm_weak[[y]] <-          stress3_fun(tempdata = temp_lm_weak, site_vector = site_vector)
#   mSST_stress3L_lm_origin[[y]] <-        stress3_fun(tempdata = temp_lm_origin, site_vector = site_vector)
#   mSST_stress3L_lm_origin_strong[[y]] <- stress3_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
#   mSST_stress3L_log[[y]] <-              stress3_fun(tempdata = temp_log, site_vector = site_vector)
#   
#   # stress 4 method
#   output_lm_strong <-        stress4_fun(tempdata = temp_lm_strong, site_vector = site_vector)
#   output_lm_weak <-          stress4_fun(tempdata = temp_lm_weak, site_vector = site_vector)
#   output_lm_origin <-        stress4_fun(tempdata = temp_lm_origin, site_vector = site_vector)
#   output_lm_origin_strong <- stress4_fun(tempdata = temp_lm_origin_strong, site_vector = site_vector)
#   output_log <-              stress4_fun(tempdata = temp_log, site_vector = site_vector)
#   
#   mSST_stress4L_lm_strong[[y]] <-        output_lm_strong$tempdata_stress4
#   mSST_stress4L_lm_weak[[y]] <-          output_lm_weak$tempdata_stress4
#   mSST_stress4L_lm_origin[[y]] <-        output_lm_origin$tempdata_stress4
#   mSST_stress4L_lm_origin_strong[[y]] <- output_lm_origin_strong$tempdata_stress4
#   mSST_stress4L_log[[y]] <-              output_log$tempdata_stress4
#   print(y)
#   
# }
# mSST_stress1_lm_strong <- bind_rows(mSST_stress1L_lm_strong,.id=NULL)
# mSST_stress2_lm_strong <- bind_rows(mSST_stress2L_lm_strong)
# mSST_stress3_lm_strong <- bind_rows(mSST_stress3L_lm_strong)
# mSST_stress4_lm_strong <- bind_rows(mSST_stress4L_lm_strong)
# print("strong")
# 
# mSST_stress1_lm_weak <- bind_rows(mSST_stress1L_lm_weak)
# mSST_stress2_lm_weak <- bind_rows(mSST_stress2L_lm_weak)
# mSST_stress3_lm_weak <- bind_rows(mSST_stress3L_lm_weak)
# mSST_stress4_lm_weak <- bind_rows(mSST_stress4L_lm_weak)
# print("weak")
# 
# mSST_stress1_lm_origin <- bind_rows(mSST_stress1L_lm_origin)
# mSST_stress2_lm_origin <- bind_rows(mSST_stress2L_lm_origin)
# mSST_stress3_lm_origin <- bind_rows(mSST_stress3L_lm_origin)
# mSST_stress4_lm_origin <- bind_rows(mSST_stress4L_lm_origin)
# print("origin")
# 
# mSST_stress1_lm_origin_strong <- bind_rows(mSST_stress1L_lm_origin_strong)
# mSST_stress2_lm_origin_strong <- bind_rows(mSST_stress2L_lm_origin_strong)
# mSST_stress3_lm_origin_strong <- bind_rows(mSST_stress3L_lm_origin_strong)
# mSST_stress4_lm_origin_strong <- bind_rows(mSST_stress4L_lm_origin_strong)
# print("origin_strong")
# 
# mSST_stress1_log <- bind_rows(mSST_stress1L_log)
# mSST_stress2_log <- bind_rows(mSST_stress2L_log)
# mSST_stress3_log <- bind_rows(mSST_stress3L_log)
# mSST_stress4_log <- bind_rows(mSST_stress4L_log)
# print("log")
# 
# rm(y,temp_lm,temp_log,output_lm,output_log,
#    mSST_stress1L_lm_strong,mSST_stress2L_lm_strong,mSST_stress3L_lm_strong,mSST_stress4L_lm_strong,
#    mSST_stress1L_lm_weak,  mSST_stress2L_lm_weak,mSST_stress3L_lm_weak,mSST_stress4L_lm_weak,
#    mSST_stress1L_lm_origin,mSST_stress2L_lm_origin,mSST_stress3L_lm_origin,mSST_stress4L_lm_origin,
#    mSST_stress1L_lm_origin_strong,mSST_stress2L_lm_origin_strong,mSST_stress3L_lm_origin_strong,
#    mSST_stress4L_lm_origin_strong,
#    mSST_stress1L_log,mSST_stress2L_log,mSST_stress3L_log,mSST_stress4L_log)
# # print elapsed time
# new <- Sys.time() - old # calculate difference
# print(new) # print in nice format


# ---
# Stress methods on bottom logger data:
# ---
bottomall <- bottomall %>% mutate(year=year(Date))
logger_temp <- bottomall %>% select(Site,Date,year,temp_bottom) #only relevant cols
colnames(logger_temp)[colnames(logger_temp)=="temp_bottom"] <- "Temperature" #renaming
logger_temp <- logger_temp[!logger_temp$Site=="VanDamme",]
logger_temp <- logger_temp %>% group_by(Site,Date) %>% sample_n(1)

years <- unique(logger_temp$year) #list of years in dataset
years <- years[!is.na(years)]

# stress 1 method
logger_stress1 <- stress1_fun(tempdata = logger_temp, site_vector = site_vector)
logger_stress1 <- logger_stress1 %>% 
  select(Site,Date,year,Temperature,stress1_pos_vals) %>%
  mutate(method=rep("stress1")) %>%
  mutate(datatype=rep("logger")) %>%
  mutate(regtype=rep("logger"))
colnames(logger_stress1)[colnames(logger_stress1)=="stress1_pos_vals"] <- "daily_stress"

# stress 2 method
logger_stress2 <- stress2_fun(tempdata = logger_temp, site_vector = site_vector)
logger_stress2 <- logger_stress2 %>%
  select(Site,Date,year,Temperature,stress2_degC_above_threshold) %>%
  mutate(method=rep("stress2")) %>%
  mutate(datatype=rep("logger")) %>%
  mutate(regtype=rep("logger"))
colnames(logger_stress2)[colnames(logger_stress2)=="stress2_degC_above_threshold"] <- "daily_stress"

# stress 3 method
logger_stress3 <- stress3_fun(tempdata = logger_temp, site_vector = site_vector)
logger_stress3 <- logger_stress3 %>%
  select(Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw) %>%
  mutate(method=rep("stress3")) %>%
  mutate(datatype=rep("logger")) %>%
  mutate(regtype=rep("logger"))
colnames(logger_stress3)[colnames(logger_stress3)=="stress3_degC_above_thres_clim_max_mw"] <- "daily_stress"

# stress 4 method
output <- stress4_fun(tempdata = logger_temp, site_vector = site_vector)
test <- output$tempdata_stress4
logger_stress4 <- output$tempdata_stress4
logger_stress4 <- logger_stress4 %>%
  select(Site,Date,year,Temperature,stress4_dev) %>%
  mutate(method=rep("stress4")) %>%
  mutate(datatype=rep("logger")) %>%
  mutate(regtype=rep("logger"))
colnames(logger_stress4)[colnames(logger_stress4)=="stress4_dev"] <- "daily_stress"

# combine logger stress function output
logger_stress <- bind_rows(logger_stress1,logger_stress2,logger_stress3,logger_stress4,.id=NULL)
rm(logger_stress1,logger_stress2,logger_stress3,logger_stress4)

# ---
# Combine dfs of simulated bottom and logger bottom for each method
# keep only days in both data sets (logger much smaller than mSST)
# ---

# # --- stress method 1 --- #
# mSST_stress1_lm_strong$datatype <- rep('mSST_lm_strong',length(mSST_stress1_lm_strong[,1]))
# mSST_stress1_lm_weak$datatype <- rep('mSST_lm_weak',length(mSST_stress1_lm_weak[,1]))
# mSST_stress1_log$datatype <- rep('mSST_log',length(mSST_stress1_log[,1]))
# mSST_stress1_log_strong$datatype <- rep('mSST_log_strong',length(mSST_stress1_log_strong[,1]))
# mSST_stress1_lm_all$datatype <- rep('mSST_lm_all',length(mSST_stress1_lm_all[,1]))
# logger_stress1$datatype <- rep('logger',length(logger_stress1[,1]))
# mSST_stress1_lm_strong <- mSST_stress1_lm_strong %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# mSST_stress1_lm_weak <- mSST_stress1_lm_weak %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# mSST_stress1_log <- mSST_stress1_log %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# mSST_stress1_log_strong <- mSST_stress1_log_strong %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# mSST_stress1_lm_all <- mSST_stress1_lm_all %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# logger_stress1 <- logger_stress1 %>% select(datatype,Site,Date,year,Temperature,stress1_pos_vals)
# stress1_data <- rbind(logger_stress1,mSST_stress1_lm_strong,
#                       mSST_stress1_lm_weak, mSST_stress1_lm_origin,
#                       mSST_stress1_lm_origin_strong, mSST_stress1_log,
#                       mSST_stress1_lm_all, mSST_stress1_log_strong)
# stress1_data <- stress1_data[stress1_data$Date %in% unique(logger_stress1$Date),]
# stress1_data$stress_method <- rep(1,length(stress1_data[,1]))
# colnames(stress1_data)[colnames(stress1_data)=="stress1_pos_vals"] <- 'daily_stress'
# 
# # --- stress method 2 --- #
# mSST_stress2_lm_strong$datatype <- rep('mSST_lm_strong',length(mSST_stress2_lm_strong[,1]))
# mSST_stress2_lm_weak$datatype <- rep('mSST_lm_weak',length(mSST_stress2_lm_weak[,1]))
# mSST_stress2_lm_origin$datatype <- rep('mSST_lm_origin',length(mSST_stress2_lm_origin[,1]))
# mSST_stress2_lm_origin_strong$datatype <- rep('mSST_lm_origin_strong',length(mSST_stress2_lm_origin_strong[,1]))
# mSST_stress2_log$datatype <- rep('mSST_log',length(mSST_stress2_log[,1]))
# mSST_stress2_log_strong$datatype <- rep('mSST_log_strong',length(mSST_stress2_log_strong[,1]))
# mSST_stress2_lm_all$datatype <- rep('mSST_lm_all',length(mSST_stress2_lm_all[,1]))
# logger_stress2$datatype <- rep('logger',length(logger_stress2[,1]))
# mSST_stress2_lm_strong <- mSST_stress2_lm_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_lm_weak <- mSST_stress2_lm_weak %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_lm_origin <- mSST_stress2_lm_origin %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_lm_origin_strong <- mSST_stress2_lm_origin_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_log <- mSST_stress2_log %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_log_strong <- mSST_stress2_log_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# mSST_stress2_lm_all <- mSST_stress2_lm_all %>% 
#   select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# logger_stress2 <- logger_stress2 %>% select(datatype,Site,Date,year,Temperature,stress2_degC_above_threshold)
# stress2_data <- rbind(logger_stress2,mSST_stress2_lm_strong,
#                       mSST_stress2_lm_weak,mSST_stress2_lm_origin,
#                       mSST_stress2_lm_origin_strong,mSST_stress2_log,
#                       mSST_stress2_log_strong,mSST_stress2_lm_all)
# stress2_data <- stress2_data[stress2_data$Date %in% unique(logger_stress2$Date),]
# stress2_data$stress_method <- rep(2,length(stress2_data[,1]))
# colnames(stress2_data)[colnames(stress2_data)=="stress2_degC_above_threshold"] <- 'daily_stress'
# 
# # --- stress method 3 --- #
# mSST_stress3_lm_strong$datatype <- rep('mSST_lm_strong',length(mSST_stress3_lm_strong[,1]))
# mSST_stress3_lm_weak$datatype <- rep('mSST_lm_weak',length(mSST_stress3_lm_weak[,1]))
# mSST_stress3_lm_origin$datatype <- rep('mSST_lm_origin',length(mSST_stress3_lm_origin[,1]))
# mSST_stress3_lm_origin_strong$datatype <- rep('mSST_lm_origin_strong',length(mSST_stress3_lm_origin_strong[,1]))
# mSST_stress3_log$datatype <- rep('mSST_log',length(mSST_stress3_log[,1]))
# mSST_stress3_log_strong$datatype <- rep('mSST_log_strong',length(mSST_stress3_log_strong[,1]))
# mSST_stress3_lm_all$datatype <- rep('mSST_lm_all',length(mSST_stress3_lm_all[,1]))
# logger_stress3$datatype <- rep('logger',length(logger_stress3[,1]))
# mSST_stress3_lm_strong <- mSST_stress3_lm_strong %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_lm_weak <- mSST_stress3_lm_weak %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_lm_origin <- mSST_stress3_lm_origin %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_lm_origin_strong <- mSST_stress3_lm_origin_strong %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_log <- mSST_stress3_log %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_log_strong <- mSST_stress3_log_strong %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# mSST_stress3_lm_all <- mSST_stress3_lm_all %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# logger_stress3 <- logger_stress3 %>%
#   select(datatype,Site,Date,year,Temperature,stress3_degC_above_thres_clim_max_mw)
# stress3_data <- rbind(logger_stress3,mSST_stress3_lm_strong,
#                       mSST_stress3_lm_weak,mSST_stress3_lm_origin,
#                       mSST_stress3_lm_origin_strong,mSST_stress3_log,
#                       mSST_stress3_log_strong,mSST_stress3_lm_all)
# stress3_data <- stress3_data[stress3_data$Date %in% unique(logger_stress3$Date),]
# stress3_data$stress_method <- rep(3,length(stress3_data[,1]))
# colnames(stress3_data)[colnames(stress3_data)=="stress3_degC_above_thres_clim_max_mw"] <- 'daily_stress'
# 
# # --- stress method 4 --- #
# mSST_stress4_lm_strong$datatype <- rep('mSST_lm_strong',length(mSST_stress4_lm_strong[,1]))
# mSST_stress4_lm_weak$datatype <- rep('mSST_lm_weak',length(mSST_stress4_lm_weak[,1]))
# mSST_stress4_lm_origin$datatype <- rep('mSST_lm_origin',length(mSST_stress4_lm_origin[,1]))
# mSST_stress4_lm_origin_strong$datatype <- rep('mSST_lm_origin_strong',length(mSST_stress4_lm_origin_strong[,1]))
# mSST_stress4_log$datatype <- rep('mSST_log',length(mSST_stress4_log[,1]))
# mSST_stress4_log_strong$datatype <- rep('mSST_log_strong',length(mSST_stress4_log_strong[,1]))
# mSST_stress4_lm_all$datatype <- rep('mSST_lm_all',length(mSST_stress4_lm_all[,1]))
# logger_stress4$datatype <- rep('logger',length(logger_stress4[,1]))
# mSST_stress4_lm_strong <- mSST_stress4_lm_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_lm_weak <- mSST_stress4_lm_weak %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_lm_origin <- mSST_stress4_lm_origin %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_lm_origin_strong <- mSST_stress4_lm_origin_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_log <- mSST_stress4_log %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_log_strong <- mSST_stress4_log_strong %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# mSST_stress4_lm_all <- mSST_stress4_lm_all %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# logger_stress4 <- logger_stress4 %>% 
#   select(datatype,Site,Date,year,Temperature,stress4_dev)
# stress4_data <- rbind(logger_stress4,mSST_stress4_lm_strong,
#                       mSST_stress4_lm_weak,mSST_stress4_lm_origin,
#                       mSST_stress4_lm_origin_strong,mSST_stress4_log,
#                       mSST_stress4_lm_all,mSST_stress4_log_strong)
# stress4_data <- stress4_data[stress4_data$Date %in% unique(logger_stress4$Date),]
# stress4_data$stress_method <- rep(4,length(stress4_data[,1]))
# colnames(stress4_data)[colnames(stress4_data)=="stress4_dev"] <- 'daily_stress'
# 
# # all stress data sets together!
# stressALL_data <- as.data.frame(bind_rows(stress1_data,stress2_data,stress3_data,stress4_data))
# rm(stress1_data,stress2_data,stress3_data,stress4_data,
#    mSST_stress1_lm_strong,mSST_stress1_lm_weak,
#    mSST_stress1_lm_origin,mSST_stress1_lm_origin_strong,
#    mSST_stress1_log,mSST_stress1_log_strong,mSST_stress1_lm_all,
#    
#    mSST_stress2_lm_strong,mSST_stress2_lm_weak,
#    mSST_stress2_lm_origin,mSST_stress2_lm_origin_strong,
#    mSST_stress2_log,mSST_stress2_log_strong,mSST_stress2_lm_all,
#    
#    mSST_stress3_lm_strong,mSST_stress3_lm_weak,
#    mSST_stress3_lm_origin,mSST_stress3_lm_origin_strong,
#    mSST_stress3_log,mSST_stress3_log_strong,mSST_stress3_lm_all,
#    
#    mSST_stress4_lm_strong,mSST_stress4_lm_weak,
#    mSST_stress4_lm_origin,mSST_stress4_lm_origin_strong,
#    mSST_stress4_log,mSST_stress4_log_strong,mSST_stress4_lm_all)

# ===
# Integrate daily stress values for each site-year
# ===
# datatypes <- c('logger','mSST')
# stress_methods <- c(1,2,3,4)
# years <- unique(stressALL_data$year)
# stress_valsL <- as.list(rep(NA,length(stress_methods)))
# for(m in 1:length(stress_methods)){ #creating empty dataframe for logger
#   stress_valsL[[m]] <- data.frame(
#       Site = rep(site_vector,each=length(years)),
#       year = rep(years,times=length(site_vector)),
#       integral = rep(NA,length=length(rep(site_vector,each=length(years)))),
#       datatype = rep('logger',length=length(rep(site_vector,each=length(years)))),
#       stress_method = rep(stress_methods[m],length(rep(site_vector,each=length(years))))
#     )
# }
# stress_valslogger <- bind_rows(stress_valsL)
# rm(m,stress_valsL)
# stress_valsL <- as.list(rep(NA,length(stress_methods)))
# for(m in 1:length(stress_methods)){ #creating empty dataframe for mSST
#   stress_valsL[[m]] <- data.frame(
#       Site = rep(site_vector,each=length(years)),
#       year = rep(years,times=length(site_vector)),
#       integral = rep(NA,length=length(rep(site_vector,each=length(years)))),
#       datatype = rep('mSST',length=length(rep(site_vector,each=length(years)))),
#       stress_method = rep(stress_methods[m],length(rep(site_vector,each=length(years))))
#     )
# }
# stress_valsmsst <- bind_rows(stress_valsL)
# rm(m,stress_valsL)
# stress_vals <- rbind(stress_valslogger, stress_valsmsst)
# rm(stress_valslogger, stress_valsmsst)
# 
# 
# # integrate daily stress values (per method, site, year)
# for(m in 1:length(stress_methods)){
#   
#   for(d in 1:length(datatypes)){ # for each site
#     
#     for(s in 1:length(site_vector)){ # and for each datatype
#       
#       for(y in 1:length(years)){ # step through each year
#         
#         # integrate daily stress values
#         #yy <- sdy$daily_stress
#         yy <- stressALL_data[stressALL_data$stress_method==stress_methods[m] &
#                                stressALL_data$datatype==datatypes[d] &
#                                stressALL_data$Site==site_vector[s] &
#                                stressALL_data$year==years[y],]$daily_stress
#         
#         yy <- yy[!is.na(yy)]
#         xx <- seq(from=1,to=length(yy),by=1)
#         stress_vals[stress_vals$Site==site_vector[s] &
#                       stress_vals$datatype==datatypes[d] &
#                       stress_vals$year==years[y] &
#                       stress_vals$stress_method==stress_methods[m],]$integral <- trapz(x=xx,y=yy)
#       } #close year loop
#     } #close datatypes
#   } #close site
# } #close stress method
# rm(m,d,s,y,yy,xx)



# ===
# Integrate daily stress values for each site (years combined!)
# ===
stressALL_data <- bind_rows(mSST_stress,logger_stress,.id=NULL)
regtypes <- unique(stressALL_data$regtype)
method <- unique(stressALL_data$method)
stress_vals_s <- expand.grid(site_vector,method,regtypes)
colnames(stress_vals_s) <- c("Site","method","regtype")
stress_vals_s$integral <- rep(NA,length=nrow(stress_vals_s))


# ====
# integrate daily stress values (per method, site, years combined!)
for(m in 1:length(method)){ # step through stress methods (1-4)
  
  for(r in 1:length(regtypes)){ # for each datatype (mSST & logger)
    
    for(s in 1:length(site_vector)){ # and for each site
      # integrate daily stress values
      yy <- stressALL_data[stressALL_data$method==method[m] &
                           stressALL_data$regtype==regtypes[r] &
                           stressALL_data$Site==site_vector[s] ,]$daily_stress
      
      yy <- yy[!is.na(yy)]
      xx <- seq(from=1,to=length(yy),by=1)
      stress_vals_s[stress_vals_s$Site==site_vector[s] &
                    stress_vals_s$regtype==regtypes[r] &
                    stress_vals_s$method==method[m],]$integral <- 
        trapz(x=xx,y=yy)
      print(s)

    } #close site loop
    print(r)
  } #close datatype
  print(m)
} #close stress method
rm(m,r,s,yy,xx)




# ---
# standardize stress values 
# 2 approaches:
# 1) normalize integral values across years and sites (within each method)
# 2) normalize integral values across sites (within each method and year)
# ---

# --- testing: normalize integral values across sites --- #
stress_vals_s$integral_relative <- rep(NA,length=length(stress_vals_s[,1]))

for(r in 1:length(regtypes)){ #for each conversion type
  
  for(m in 1:length(method)){ # walk through 4 methods
    
    # min stress integral value among sites for that regression type and stress method
    min_int <- min(stress_vals_s[stress_vals_s$regtype==regtypes[r] &
                                 stress_vals_s$method==method[m],]$integral)
    
    # max stress integral value among sites for that regression type and stress method
    max_int <- max(stress_vals_s[stress_vals_s$regtype==regtypes[r] &
                                   stress_vals_s$method==method[m],]$integral)
    
    # relative integral value
    stress_vals_s[stress_vals_s$regtype==regtypes[r] &
                  stress_vals_s$method==method[m],]$integral_relative <-
      
      ((stress_vals_s[stress_vals_s$regtype==regtypes[r] &
                      stress_vals_s$method==method[m],]$integral) - min_int) / (max_int - min_int)
      
  }
}




# try getting rid of LaJolla
#stress_vals_s <- stress_vals_s[!stress_vals_s$Site=="LaJolla",]

# ---
# (1) correlation in stress values: lm_strong for all 4 methods
lm_strong_regs <- stress_vals_s[stress_vals_s$regtype %in% c('logger','lm_strong'),] %>%
  select(Site,integral_relative,method,regtype) %>%
  spread(key=regtype,value=integral_relative) %>%
  mutate(r2=rep(NA)) %>% 
  mutate(yint=rep(NA)) %>%
  mutate(slope=rep(NA))

for(m in 1:length(method)){
  temp <- lm_strong_regs[lm_strong_regs$method==method[m],]
  temp_lm <- lm(temp$lm_strong ~ temp$logger)
  
  lm_strong_regs[lm_strong_regs$method==method[m],]$r2 <-
    rep(summary(temp_lm)$r.squared)
  
  lm_strong_regs[lm_strong_regs$method==method[m],]$yint <-
    rep(summary(temp_lm)$coefficient[1])
  
  lm_strong_regs[lm_strong_regs$method==method[m],]$slope <-
    rep(summary(temp_lm)$coefficient[2])} #close loop

lm_strong_regs$labels <- paste(lm_strong_regs$method,"   r2=",round(lm_strong_regs$r2,digits=2),sep="")

p1 <- ggplot(data=lm_strong_regs) +
  facet_wrap(vars(labels)) +
  geom_point(aes(x=logger,y=lm_strong)) +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(data=lm_strong_regs,aes(slope=slope,intercept = yint),linetype="dashed") +
  geom_text_repel(aes(x=logger,y=lm_strong,label=Site),
                  size=3,color="grey") +
  theme_bw() +
  ylab("mSST (lm_strong)") +
  ggtitle("Linear regression fit to strong upwelling") 
rm(m,temp,temp_lm,lm_strong_regs)


# ---
# (2) correlation in stress values: lm_weak for all 4 methods
lm_weak_regs <- stress_vals_s[stress_vals_s$regtype %in% c('logger','lm_weak'),] %>%
  select(Site,integral_relative,method,regtype) %>%
  spread(key=regtype,value=integral_relative) %>%
  mutate(r2=rep(NA)) %>% 
  mutate(yint=rep(NA)) %>%
  mutate(slope=rep(NA))

for(m in 1:length(method)){
  temp <- lm_weak_regs[lm_weak_regs$method==method[m],]
  temp_lm <- lm(temp$lm_weak ~ temp$logger)
  
  lm_weak_regs[lm_weak_regs$method==method[m],]$r2 <-
    rep(summary(temp_lm)$r.squared)
  
  lm_weak_regs[lm_weak_regs$method==method[m],]$yint <-
    rep(summary(temp_lm)$coefficient[1])
  
  lm_weak_regs[lm_weak_regs$method==method[m],]$slope <-
    rep(summary(temp_lm)$coefficient[2])} #close loop

lm_weak_regs$labels <- paste(lm_weak_regs$method,"   r2=",round(lm_weak_regs$r2,digits=2),sep="")

p2 <- ggplot(data=lm_weak_regs) +
  facet_wrap(vars(labels)) +
  geom_point(aes(x=logger,y=lm_weak)) +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(data=lm_weak_regs,aes(slope=slope,intercept = yint),linetype="dashed") +
  geom_text_repel(aes(x=logger,y=lm_weak,label=Site),
                  size=3,color="grey") +
  theme_bw() +
  ylab("mSST (lm_weak)") +
  ggtitle("Linear, weak upwelling") 
rm(m,temp,temp_lm,lm_weak_regs)


# ---
# (3) correlation in stress values: log for all sites
log_all_regs <- stress_vals_s[stress_vals_s$regtype %in% c('logger','log_all'),] %>%
  select(Site,integral_relative,method,regtype) %>%
  spread(key=regtype,value=integral_relative) %>%
  mutate(r2=rep(NA)) %>% 
  mutate(yint=rep(NA)) %>%
  mutate(slope=rep(NA))

for(m in 1:length(method)){
  temp <- log_all_regs[log_all_regs$method==method[m],]
  temp_lm <- lm(temp$log_all ~ temp$logger)
  
  log_all_regs[log_all_regs$method==method[m],]$r2 <-
    rep(summary(temp_lm)$r.squared)
  
  log_all_regs[log_all_regs$method==method[m],]$yint <-
    rep(summary(temp_lm)$coefficient[1])
  
  log_all_regs[log_all_regs$method==method[m],]$slope <-
    rep(summary(temp_lm)$coefficient[2])} #close loop

log_all_regs$labels <- paste(log_all_regs$method,"   r2=",round(log_all_regs$r2,digits=2),sep="")


p3 <- ggplot(data=log_all_regs) +
  facet_wrap(vars(labels)) +
  geom_point(aes(x=logger,y=log_all)) +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(data=log_all_regs,aes(slope=slope,intercept = yint),linetype="dashed") +
  geom_text_repel(aes(x=logger,y=log_all,label=Site),
                  size=3,color="grey") +
  theme_bw() +
  ylab("mSST (log_all)")
  ggtitle("Power law, all sites") 
rm(m,temp,temp_lm,stress_vals_s1)

# ---
# (4) correlation in stress values: log for strong sites
log_strong_regs <- stress_vals_s[stress_vals_s$regtype %in% c('logger','log_strong'),] %>%
  select(Site,integral_relative,method,regtype) %>%
  spread(key=regtype,value=integral_relative) %>%
  mutate(r2=rep(NA)) %>% 
  mutate(yint=rep(NA)) %>%
  mutate(slope=rep(NA))

for(m in 1:length(method)){
  temp <- log_strong_regs[log_strong_regs$method==method[m],]
  temp_lm <- lm(temp$log_strong ~ temp$logger)
  
  log_strong_regs[log_strong_regs$method==method[m],]$r2 <-
    rep(summary(temp_lm)$r.squared)
  
  log_strong_regs[log_strong_regs$method==method[m],]$yint <-
    rep(summary(temp_lm)$coefficient[1])
  
  log_strong_regs[log_strong_regs$method==method[m],]$slope <-
    rep(summary(temp_lm)$coefficient[2])} #close loop

log_strong_regs$labels <- paste(log_strong_regs$method,"   r2=",round(log_strong_regs$r2,digits=2),sep="")

p4 <- ggplot(data=log_strong_regs) +
  facet_wrap(vars(labels)) +
  geom_point(aes(x=logger,y=log_strong)) +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(data=log_strong_regs,aes(slope=slope,intercept = yint),linetype="dashed") +
  geom_text_repel(aes(x=logger,y=log_strong,label=Site),
                  size=3,color="grey") +
  theme_bw() +
  ylab("mSST (log_strong)") +
  ggtitle("Log, strong sites") 
rm(m,temp,temp_lm,log_strong_regs)

# ---
# (5) correlation in stress values: lm all sites
lm_all_regs <- stress_vals_s[stress_vals_s$regtype %in% c('logger','lm_all'),] %>%
  select(Site,integral_relative,method,regtype) %>%
  spread(key=regtype,value=integral_relative) %>%
  mutate(r2=rep(NA)) %>% 
  mutate(yint=rep(NA)) %>%
  mutate(slope=rep(NA))

for(m in 1:length(method)){
  temp <- lm_all_regs[lm_all_regs$method==method[m],]
  temp_lm <- lm(temp$lm_all ~ temp$logger)
  
  lm_all_regs[lm_all_regs$method==method[m],]$r2 <-
    rep(summary(temp_lm)$r.squared)
  
  lm_all_regs[lm_all_regs$method==method[m],]$yint <-
    rep(summary(temp_lm)$coefficient[1])
  
  lm_all_regs[lm_all_regs$method==method[m],]$slope <-
    rep(summary(temp_lm)$coefficient[2])} #close loop

lm_all_regs$labels <- paste(lm_all_regs$method,"   r2=",round(lm_all_regs$r2,digits=2),sep="")

p5 <- ggplot(data=lm_all_regs) +
  facet_wrap(vars(labels)) +
  geom_point(aes(x=logger,y=lm_all)) +
  geom_abline(intercept = 0, slope=1) +
  geom_abline(data=lm_all_regs,aes(slope=slope,intercept = yint),linetype="dashed") +
  geom_text_repel(aes(x=logger,y=lm_all,label=Site),
                  size=3,color="grey") +
  theme_bw() +
  ylab("mSST (lm_all)") +
  ggtitle("Linear, all sites") 

rm(m,temp,temp_lm,lm_all_regs)

tiff(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/conversion_validation_v4_sst_averaged_5regs.tiff", res=300, height = 13, width = 12, units="in")
plot_grid(p1,p2,p3,p4,p5,
          nrow=3,
          align="v",
          labels=c("a)","b)","c)","d)","e)"))
dev.off()
rm(p1,p2,p3,p4,p5)
# ------------------------------------------------------- #


# stress_vals$relative_stress_sy <- rep(NA,length(stress_vals[,1]))
# stress_vals$relative_stress_s <- rep(NA,length(stress_vals[,1]))
# for(d in 1:length(datatypes)){ # for each datatype
#   for(m in 1:length(stress_methods)){ # step through methods
#     for(y in 1:length(years)){ # and step through years
#       
#       # --- normalize across sites & years --- #
#       # fill in relative stress among sites (divide each integral by the max integral)
#       stress_vals[stress_vals$datatype==datatypes[d] &
#                     stress_vals$stress_method==stress_methods[m] &
#                     stress_vals$year==years[y],]$relative_stress_sy <-
#         stress_vals[stress_vals$datatype==datatypes[d] &
#                       stress_vals$stress_method==stress_methods[m] &
#                       stress_vals$year==years[y],]$integral /
#         # max integral across sites and years
#         max(stress_vals[stress_vals$datatype==datatypes[d] &
#                           stress_vals$stress_method==stress_methods[m],]$integral)
#       
#       # --- normalize across sites (& within years) --- # 
#       # fill in relative stress among sites (divide each integral by the max integral)
#       stress_vals[stress_vals$datatype==datatypes[d] &
#                     stress_vals$stress_method==stress_methods[m] &
#                     stress_vals$year==years[y],]$relative_stress_s <-
#         stress_vals[stress_vals$datatype==datatypes[d] &
#                       stress_vals$stress_method==stress_methods[m] &
#                       stress_vals$year==years[y],]$integral /
#         # max integral across sites (& within years)
#         max(stress_vals[stress_vals$datatype==datatypes[d] &
#                           stress_vals$stress_method==stress_methods[m] &
#                           stress_vals$year==years[y],]$integral)
#       
#     }
#   }
# }
# rm(d,m,y)
# 
# 
# # format data to plot (relative_stress_sy)
# s1.17 <- stress_vals %>% 
#   filter(stress_method==1,year=='2017') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('1'))
# 
# s1.18 <- stress_vals %>% 
#   filter(stress_method==1,year=='2018') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('1'))
# 
# s2.17 <- stress_vals %>% 
#   filter(stress_method==2,year=='2017') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('2'))
# s2.18 <- stress_vals %>% 
#   filter(stress_method==2,year=='2018') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('2'))
# 
# s3.17 <- stress_vals %>% 
#   filter(stress_method==3,year=='2017') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('3'))
# s3.18 <- stress_vals %>% 
#   filter(stress_method==3,year=='2018') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('3'))
# 
# s4.17 <- stress_vals %>% 
#   filter(stress_method==4,year=='2017') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('4'))
# s4.18 <- stress_vals %>% 
#   filter(stress_method==4,year=='2018') %>% 
#   select(Site,datatype,relative_stress_sy) %>% 
#   spread(key=datatype,value=relative_stress_sy) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('4'))
# 
# stress_vals_plot_sy <- rbind(s1.17,s1.18,
#                           s2.17,s2.18,
#                           s3.17,s3.18,
#                           s4.17,s4.18)
# rm(s1.17,s1.18,
#    s2.17,s2.18,
#    s3.17,s3.18,
#    s4.17,s4.18)
# 
# # format data to plot (relative_stress_s)
# s1.17 <- stress_vals %>% 
#   filter(stress_method==1,year=='2017') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('1'))
# 
# s1.18 <- stress_vals %>% 
#   filter(stress_method==1,year=='2018') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('1'))
# 
# s2.17 <- stress_vals %>% 
#   filter(stress_method==2,year=='2017') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('2'))
# s2.18 <- stress_vals %>% 
#   filter(stress_method==2,year=='2018') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('2'))
# 
# s3.17 <- stress_vals %>% 
#   filter(stress_method==3,year=='2017') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('3'))
# s3.18 <- stress_vals %>% 
#   filter(stress_method==3,year=='2018') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('3'))
# 
# s4.17 <- stress_vals %>% 
#   filter(stress_method==4,year=='2017') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2017')) %>% mutate(stress_method=rep('4'))
# s4.18 <- stress_vals %>% 
#   filter(stress_method==4,year=='2018') %>% 
#   select(Site,datatype,relative_stress_s) %>% 
#   spread(key=datatype,value=relative_stress_s) %>%
#   mutate(year=rep('2018')) %>% mutate(stress_method=rep('4'))
# 
# stress_vals_plot_s <- rbind(s1.17,s1.18,
#                              s2.17,s2.18,
#                              s3.17,s3.18,
#                              s4.17,s4.18)
# rm(s1.17,s1.18,
#    s2.17,s2.18,
#    s3.17,s3.18,
#    s4.17,s4.18)
# 
# ggplot(data=stress_vals_plot_sy) +
#   facet_wrap(~stress_method) +
#   geom_point(aes(x=logger,y=mSST,color=year)) +
#   geom_abline(slope=1,intercept = 0) +
#   ggtitle("SST converstion uses power log equation (r2=0.7);\nstress values normalized across sites and yrs")
# 
# ggplot(data=stress_vals_plot_s) +
#   facet_wrap(~stress_method) +
#   geom_point(aes(x=logger,y=mSST,color=year)) +
#   geom_abline(slope=1,intercept = 0) +
#   ggtitle("SST converstion uses power log equation (r2=0.7);\nstress values normalized across sites & w/in yr")
# 

  


