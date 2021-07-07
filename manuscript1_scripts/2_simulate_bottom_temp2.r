# ---
# Simulate bottom temp data from SST using regression and 2.5 offset
# Compare measures of stress using simulated data and bottom logger data
# ---

site_vector <- unique(SSTfede$Site) # use in for loops
years <- unique(SSTfede$year) # use in for loops

# create empty cols for building the simulated time series
SSTfede$highfreqnoise_lm_strong <- rep(NA,length=length(SSTfede[,1]))
SSTfede$highfreqnoise_lm_weak <- rep(NA,length=length(SSTfede[,1]))
SSTfede$highfreqnoise_log <- rep(NA,length=length(SSTfede[,1]))
SSTfede$highfreqnoise_log_strong <- rep(NA,length=length(SSTfede[,1]))
SSTfede$highfreqnoise_lm_all <- rep(NA,length=length(SSTfede[,1]))
SSTfede$highfreqnoise_version2 <- rep(NA,length=length(SSTfede[,1]))

SSTfede$climatology_minus_2.5offset <- rep(NA,length=length(SSTfede[,1]))

SSTfede$mSST_lm_strong <- rep(NA,length=length(SSTfede[,1]))
SSTfede$mSST_lm_weak <- rep(NA,length=length(SSTfede[,1]))
SSTfede$mSST_log <- rep(NA,length=length(SSTfede[,1]))
SSTfede$mSST_log_strong <- rep(NA,length=length(SSTfede[,1]))
SSTfede$mSST_lm_all <- rep(NA,length=length(SSTfede[,1]))
SSTfede$mSST_version2 <- rep(NA,length=length(SSTfede[,1]))

# deviations of raw SST temp from climatology:
SSTfede$dev_10 <- SSTfede$Climatology - SSTfede$temp_sst_raw 



# ----------------- 
# --- site -------- 
# -----------------
# calc high freq in SST, then use equation and convert to bottom high
# freq variance -- and store high freq var values in this df
converted_bottomdf_s <- data.frame(
  Site = site_vector,
  highfreq_SST = rep(NA,length=length(site_vector)),
  converted_bottom_lm_strong = rep(NA,length=length(site_vector)),
  converted_bottom_lm_weak = rep(NA,length=length(site_vector)),
  converted_bottom_log_all = rep(NA,length=length(site_vector)),
  converted_bottom_log_strong = rep(NA,length=length(site_vector)),
  converted_bottom_lm_all = rep(NA,length=length(site_vector))
)

# ---
# run lm_weak regression on weak sites, run log_strong regression on strong sites
# ---
for(s in 1:length(site_vector)){
  
  # ---
  # calculate the average SST high freq per year
  var.yrs <- data.frame(year = unique(SSTfede$year),
                        var = rep(NA,length=length(unique(SSTfede$year))))
  for (y in 1:length(unique(SSTfede$year))){
    var.yrs[var.yrs$year == var.yrs$year[y],]$var <-
      var(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==var.yrs$year[y], ]$dev_10,na.rm=TRUE)
  }
  SST_high_freq <- mean(var.yrs$var)
  rm(var.yrs)
  
  # store the SST high freq var in converted_bottomdf_s data frame
  converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$highfreq_SST <- 
    SST_high_freq
  
  # --- RUNNING REGRESSIONS --- #
  
  if(regdf[regdf$Site==site_vector[s],]$upwelling==0){
    
    converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_weak <- 
      as.numeric(SST_high_freq*m_weak$coefficients[2] + m_weak$coefficients[1])
    
  } else if(regdf[regdf$Site==site_vector[s],]$upwelling==1) {
    
    converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_log_strong <- 
      as.numeric((10^(m_strong_log$coefficients[1]))*(SST_high_freq^(m_strong_log$coefficients[2])))
  }
}

# reformat converted_bottomdf_s:
converted_bottomdf_s <- converted_bottomdf_s %>% 
  mutate(converted_bottom_var = coalesce(converted_bottom_lm_weak,converted_bottom_log_strong)) %>%
  select(Site, highfreq_SST, converted_bottom_var)

# -- GENERATE BOTTOM TEMP TIME SERIES -- #
for(s in 1:length(site_vector)){
  
  SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_version2 <- 
    rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
          mean=0, sd=sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_var)
    ) # close rnorm generator 
}

SSTfede$climatology_minus_2.5offset <- SSTfede$Climatology - 2.5
SSTfede$mSST_version2 <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_version2




# # ---
# # run all 5 regression on all sites
# # ---
# for(s in 1:length(site_vector)){
#   # calculate SST high freq variance for each site using the raw temp deviations,
#   
#   # ---
#   # option 1
#   # calculate the average SST high freq per year
#   var.yrs <- data.frame(year = unique(SSTfede$year),
#                         var = rep(NA,length=length(unique(SSTfede$year))))
#   for (y in 1:length(unique(SSTfede$year))){
#     var.yrs[var.yrs$year == var.yrs$year[y],]$var <-
#       var(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==var.yrs$year[y], ]$dev_10,na.rm=TRUE)
#   }
#   SST_high_freq <- mean(var.yrs$var)
#   rm(var.yrs)
#   
#   # ---
#   # option 2
#   #SST_high_freq <- var(SSTfede[SSTfede$Site==site_vector[s], ]$dev_10,na.rm=TRUE)
#   
#   
#   # store the SST high freq var in converted_bottomdf_s data frame
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$highfreq_SST <- 
#     SST_high_freq
#   
#   
#   # --- RUNNING 5 REGRESSIONS --- #
#   
#   # (1) power law equation fit to all sites
#   new_bottom_var_log <- 
#     as.numeric((10^(m_all_log$coefficients[1]))*(SST_high_freq^(m_all_log$coefficients[2])))
#   
#   # (2) lm_strong
#   new_bottom_var_lm_strong <-
#     as.numeric(SST_high_freq*m_strong$coefficients[2] + m_strong$coefficients[1])
#   
#   # (3) lm_weak
#   new_bottom_var_lm_weak <-
#     as.numeric(SST_high_freq*m_weak$coefficients[2] + m_weak$coefficients[1])
#   
#   # (4) lm_all
#   new_bottom_var_lm_all <-
#     as.numeric(SST_high_freq*m_all_lm$coefficients[2] + m_all_lm$coefficients[1])
#   
#   # (5) log_strong
#   new_bottom_var_log_strong <-
#     as.numeric((10^(m_strong_log$coefficients[1]))*(SST_high_freq^(m_strong_log$coefficients[2])))
#   
#   
#   # -------------------------------------- #
#   
#   # (1) store the new bottom temp high freq var (log_all)
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_log_all <- 
#     new_bottom_var_log
#   
#   # (2) store the new bottom temp high freq var (lm_strong)
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_strong <- 
#     new_bottom_var_lm_strong
#   
#   # (3) store the new bottom temp high freq var (lm_weak)
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_weak <- 
#     new_bottom_var_lm_weak
#   
#   # (4) store the new bottom temp high freq var (lm_all)
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_all <- 
#     new_bottom_var_lm_all
#   
#   # (5) store the new bottom temp high freq var (log_strong)
#   converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_log_strong <- 
#     new_bottom_var_log_strong
#   
#   print(s)
#   
# }
# rm(s,y,
#    new_bottom_var_log,
#    new_bottom_var_lm_strong,
#    new_bottom_var_lm_weak,
#    new_bottom_var_lm_all,
#    new_bottom_var_log_strong)
# 
# ---
# plots
# ---
# SST high freq var vs the converted bottom high freq var (SITE-YEAR)
# Look for: are there negative high freq var values? --> if yes, need to address
# ggplot() + 
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_log,y=highfreq_SST),color="blue") +
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_strong,y=highfreq_SST), color="orange") +
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_weak,y=highfreq_SST), color="green") +
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_origin,y=highfreq_SST), color="purple") +
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_origin_strong,y=highfreq_SST), color="red") +
#   ylab("high freq var SST") +
#   xlab("converted high freq bottom var") +
#   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
#   ggtitle("High freq var in SST 10 yr series is converted to high\n freq var for simulated bottom temp time series\n(each point is a site-year)\nBlue=power eq, Orange=lm_strong,\n Green=lm_weak, Purple=lm_origin, Red=lm_origin_strong") +
#   xlim(c(-1,10))

# SST high freq var vs the converted bottom high freq var (SITE)
# Look for: are there negative high freq var values? --> if yes, need to address
ggplot() + 
  geom_point(data=converted_bottomdf_s,
             aes(y=converted_bottom_var,x=highfreq_SST),color="blue") +
  xlab("high freq var SST") +
  ylab("converted high freq bottom var") +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  ggtitle("High freq var in SST 10 yr series is converted to high\n freq var for simulated bottom temp time series\n(each point is a site)") 


# calculate thresholds (mean + stdev) for plotting
thresholds <- expand.grid(site_vector,datatypes)
colnames(thresholds) <- c("Site","datatype")
thresholds <- thresholds %>%
  mutate(mean = rep(NA)) %>%
  mutate(stdev = rep(NA)) %>%
  mutate(threshold = rep(NA))

SSTfede_forplot <- SSTfede[SSTfede$Date %in% bottomall$Date,]
for(s in 1:length(site_vector)){
  # calc mean mSST temp for each site 
  thresholds[thresholds$datatype=="mSST" &
               thresholds$Site==site_vector[s],]$mean <-
    mean(SSTfede_forplot[SSTfede_forplot$Site==site_vector[s],]$mSST_version2)
  # calc stdev mSST for each site
  thresholds[thresholds$datatype=="mSST" &
               thresholds$Site==site_vector[s],]$stdev <-
    sd(SSTfede_forplot[SSTfede_forplot$Site==site_vector[s],]$mSST_version2)

}
for(s in 1:length(site_vector)){
  # calc mean logger temp for each site 
  thresholds[thresholds$datatype=="logger" &
               thresholds$Site==site_vector[s],]$mean <-
    mean(bottomall[bottomall$Site==site_vector[s],]$temp_bottom)
  # calc stdev logger temp for each site
  thresholds[thresholds$datatype=="logger" &
               thresholds$Site==site_vector[s],]$stdev <-
    sd(bottomall[bottomall$Site==site_vector[s],]$temp_bottom)
}
thresholds$threshold <- thresholds$mean + thresholds$stdev

# ---
# plot temp time series and thresholds
pL <- as.list(rep(NA,length=length(site_vector)))
for(s in 1:length(site_vector)){
  pL[[s]] <- 
    ggplot()+
    # ---
    # plot settings
    scale_colour_manual(name="",
                        values=c(mSST="mediumpurple", logger="sandybrown")) +
    theme_classic() +
    labs(title=paste(site_vector[s]),
         x="",y="Temperature") +
    ylim(8,28) +
    # ---
    # plot mSST: time series, threshold, mean
    geom_line(data=SSTfede_forplot[SSTfede_forplot$Site==site_vector[s],],
              aes(x=Date,y=mSST_version2,color="mSST")) +
    geom_hline(yintercept =thresholds[thresholds$datatype=="mSST" & 
                                        thresholds$Site==site_vector[s],]$threshold,
               color="mediumpurple") +
    geom_hline(yintercept =thresholds[thresholds$datatype=="mSST" & 
                                        thresholds$Site==site_vector[s],]$mean,
               color="mediumpurple",linetype="dashed") +
    # ---
    # plot logger: time series, threshold, mean 
    geom_line(data=bottomall[bottomall$Site==site_vector[s],],
              aes(x=Date,y=temp_bottom,color="logger")) +
    geom_hline(yintercept = thresholds[thresholds$datatype=="logger" & 
                                         thresholds$Site==site_vector[s],]$threshold,
               color="sandybrown") +
    geom_hline(yintercept = thresholds[thresholds$datatype=="logger" & 
                                         thresholds$Site==site_vector[s],]$mean,
               color="sandybrown",linetype="dashed") 
}

jpeg(file="C:/Users/Mikaela/Documents/GitHub/natividad/manuscript2_figs/figS5_forBrock1a.jpeg", 
     units="in", width=8, height=10, res=300)
do.call("grid.arrange", c(pL[1:8], ncol = 2))
dev.off()

jpeg(file="C:/Users/Mikaela/Documents/GitHub/natividad/manuscript2_figs/figS5_forBrock1b.jpeg", 
     units="in", width=8, height=10, res=300)
do.call("grid.arrange", c(pL[9:17], ncol = 2))
dev.off()
rm(pL)

# needed editing (copy and pasted from other script)
thres1 <- ggplot(data=thresholds) +
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

tiff(file="C:/Users/Mikaela/Documents/GitHub/natividad/manuscript2_figs/fig0_forBrock2.tiff", 
     units="in", width=12, height=32, res=300)
ggplot(data=thresholds)
dev.off()
  

# ggplot() + 
#   geom_point(data=converted_bottomdf_s,
#              aes(y=converted_bottom_log_all,x=highfreq_SST),color="blue") +
#   geom_point(data=converted_bottomdf_s,
#              aes(y=converted_bottom_lm_strong,x=highfreq_SST), color="lightgreen") +
#   geom_point(data=converted_bottomdf_s,
#              aes(y=converted_bottom_lm_weak,x=highfreq_SST), color="green") +
#   geom_point(data=converted_bottomdf_s,
#              aes(y=converted_bottom_lm_all,x=highfreq_SST), color="darkgreen") +
#   geom_point(data=converted_bottomdf_s,
#              aes(y=converted_bottom_log_strong,x=highfreq_SST), color="darkblue") +
#   xlab("high freq var SST") +
#   ylab("converted high freq bottom var") +
#   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
#   ggtitle("High freq var in SST 10 yr series is converted to high\n freq var for simulated bottom temp time series\n(each point is a site)\nBlue=log_all, Darkblue=log_strong, Lightgreen=lm_strong,\n Green=lm_weak, Darkgreen=lm_all") 


# # Which sites have negative high freq bottom var? (--> you have to make positive!)
# # these sites are probably sites in bays
# table(converted_bottomdf_s[converted_bottomdf_s$converted_bottom_lm_strong < 0,]$Site)
# table(converted_bottomdf_s[converted_bottomdf_s$converted_bottom_lm_weak < 0,]$Site)
# table(converted_bottomdf_s[converted_bottomdf_s$converted_bottom_lm_all < 0,]$Site)
# converted_bottomdf_s[converted_bottomdf_s$converted_bottom_lm_strong < 0,]$converted_bottom_lm_strong <-0.01 #make var really close to 0
# converted_bottomdf_s[converted_bottomdf_s$converted_bottom_lm_all < 0,]$converted_bottom_lm_all <-0.01 #make var really close to 0

# Which sites and years have negative high freq bottom var? (--> you have to filter these out!)
# these sites are probably sites in bays
# dd <- table(converted_bottomdf[converted_bottomdf$converted_bottom_lm_strong < 0,]$Site,
#       converted_bottomdf[converted_bottomdf$converted_bottom_lm_strong < 0,]$year)
# write.csv(dd, file="C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/negative_var_sites_yrs_lmstrong.csv")
# 
# dd <- table(converted_bottomdf[converted_bottomdf$converted_bottom_lm_weak < 0,]$Site,
#             converted_bottomdf[converted_bottomdf$converted_bottom_lm_weak < 0,]$year)
# write.csv(dd, file="C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/negative_var_sites_yrs_lmweak.csv")
# rm(dd)

# if high freq bottom var is negative, change it to the smallest
# positive value of converted bottom for that site
# ****note**** might need to change how I deal with negative variances

# # lm_strong
# converted_bottomdf$converted_bottom_lm_strong_noneg <- 
#   ifelse(converted_bottomdf$converted_bottom_lm_strong <0, #if var is negative
#          min(converted_bottomdf[converted_bottomdf$converted_bottom_lm_strong>0,]$converted_bottom_lm_strong), #replace with smallest pos var
#          converted_bottomdf$converted_bottom_lm_strong)
# 
# # lm_weak
# converted_bottomdf$converted_bottom_lm_weak_noneg <- 
#   ifelse(converted_bottomdf$converted_bottom_lm_weak <0, #if var is negative
#          min(converted_bottomdf[converted_bottomdf$converted_bottom_lm_weak>0,]$converted_bottom_lm_weak), #replace with smallest pos var
#          converted_bottomdf$converted_bottom_lm_weak)
# 
# # now plot SST high freq var vs converted high freq var (no negatives)
# 
# p1 <- ggplot() + 
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_weak_noneg,y=highfreq_SST),color="darkgrey") +
#   geom_point(data=converted_bottomdf,
#              aes(x=converted_bottom_lm_strong_noneg,y=highfreq_SST),color="blue") +
#   ylab("high freq var SST") +
#   xlab("converted high freq bottom var ") +
#   geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0, linetype="dashed") +
#   theme_bw() +
#   ggtitle("If converted bottom temp high freq var < 0, \nmake equal to smallest positive var")
# 
# tiff(filename = "C:/Users/Mikaela/Documents/GitHub/natividad/manuscript1_figs/figS4_linear_regs_strong_weak.tiff", res=300, height = 6, width = 7, units="in")
# p1
# dev.off()
# rm(p1)


# # -------------------------------------- #
# # -- GENERATE BOTTOM TEMP TIME SERIES -- #
# 
# # generate time series of bottom temp with converted high freq var
# set.seed(42) # the answer to the universe :)
# 
# site_vector <- unique(SSTfede$Site)
# 
# # ----
# # Run this 1 loop to sim bottom data with the 
# # site-specific high freq var
# # -----
# for (s in 1:length(site_vector)){ # for each site
#   
#   # (1) lm_strong:
#   # random number generation with mean=0 and sd=sqrt of converted bottom var
#   SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_lm_strong <- 
#     rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
#           mean=0, sd=sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_strong)
#     ) # close rnorm generator 
#   
#   # (2) lm_weak:
#   # random number generation with mean=0 and sd=sqrt of converted bottom var
#   SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_lm_weak <- 
#     rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
#           mean=0, sd= sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_weak)
#     ) # close rnorm generator
#   
#   # (3) log_all:
#   # random number generation with mean=0 and sd=sqrt of converted bottom var
#   SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_log <- 
#     rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
#           mean=0, sd= sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_log_all)
#     ) #close rnorm generator
#   
#   # (4) log_strong:
#   # random number generation with mean=0 and sd=sqrt of converted bottom var
#   SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_log_strong <- 
#     rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
#           mean=0, sd= sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_log_strong)
#     ) #close rnorm generator
#   
#   # (5) lm_all:
#   # random number generation with mean=0 and sd=sqrt of converted bottom var
#   SSTfede[SSTfede$Site==site_vector[s],]$highfreqnoise_lm_all <- 
#     rnorm(n=nrow(SSTfede[SSTfede$Site==site_vector[s],]),
#           mean=0, sd= sqrt(converted_bottomdf_s[converted_bottomdf_s$Site==site_vector[s],]$converted_bottom_lm_all)
#     ) #close rnorm generator
#   print(s)
# } # close site loop
# rm(s,SST_high_freq) # clean up

# ------
# Run these 2 loops to sim bottom data using 
# site-year specific variances -----> need to add the 2 more regressions
# ------
# for (s in 1:length(site_vector)){ # for each site
#   for (y in 1:length(years)){ # in each year
#     
#     # (1) lm_strong (noneg):
#     # random number generation with mean=0 and sd=sqrt of converted bottom var
#     SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_strong <- 
#       rnorm(n=length(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_strong),
#             mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_lm_strong_noneg)
#       ) # close rnorm generator 
#     
#     # (2) lm_weak (noneg):
#     # random number generation with mean=0 and sd=sqrt of converted bottom var
#     SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_weak <- 
#       rnorm(n=length(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_weak),
#             mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_lm_weak_noneg)
#       ) # close rnorm generator
#     
#     # (3) lm_origin:
#     # random number generation with mean=0 and sd=sqrt of converted bottom var
#     SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_origin <- 
#       rnorm(n=length(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_origin),
#             mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_lm_origin)
#       ) # close rnorm generator
#     
#     # (4) lm_origin_strong:
#     # random number generation with mean=0 and sd=sqrt of converted bottom var
#     SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_origin_strong <- 
#       rnorm(n=length(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_lm_origin_strong),
#             mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_lm_origin_strong)
#       ) # close rnorm generator
#     
#     # (5) log:
#     # random number generation with mean=0 and sd=sqrt of converted bottom var
#     SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_log <- 
#       rnorm(n=length(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y],]$highfreqnoise_log),
#             mean=0, sd= sqrt(converted_bottomdf[converted_bottomdf$Site==site_vector[s] & converted_bottomdf$year==years[y],]$converted_bottom_log)
#       ) #close rnorm generator
#             
#     print(y)
#   } # close year loop
#   print(s)
# } # close site loop
# rm(s,y) # clean up
#head(SSTfede) # check big df, did the cols fill in?
#summary(SSTfede)


# # now that I have simulated bottom temp high freq variability in all years-sites,
# # adjust SST climatology by the 2.5 deg C offset. 
# SSTfede$climatology_minus_2.5offset <- SSTfede$Climatology - 2.5
# #SSTfede$climatology_minus_2.5offset <- SSTfede$Climatology - 0

# # last step to creating simulated bottom temp (filling in the mSST column):
# # add in the simulated high freq variability to the climatology-offset time series
# SSTfede$mSST_lm_strong <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_lm_strong
# SSTfede$mSST_lm_weak <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_lm_weak
# SSTfede$mSST_log <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_log
# SSTfede$mSST_log_strong <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_log_strong
# SSTfede$mSST_lm_all <- SSTfede$climatology_minus_2.5offset + SSTfede$highfreqnoise_lm_all


# ---
# check: how much does simulated bottom temp match temp logger data?
# note: I have much more simulated bottom temp, only plot time that 
# overlaps with temp loggers
# ---

# subset SSTfede to include only dates in bottomall (for plotting)
# sst_bottom <- semi_join(SSTfede,bottomall,by=c("Date","Site"))
# sst_bottom <- merge(sst_bottom,bottomall, by.x=c("Site", "Date"), by.y=c("Site", "Date"), all.x=FALSE, all.y=FALSE)
# pL <- as.list(rep(NA,length(site_vector)))
# for(s in 1:length(site_vector)){
#   pL[[s]] <- 
#     ggplot() +
#     geom_line(data=sst_bottom[sst_bottom$Site==site_vector[s],],
#               aes(x=Date,y=mSST,color="mSST")) +
#     geom_line(data=sst_bottom[sst_bottom$Site==site_vector[s],],
#               aes(x=Date,y=temp_bottom,
#                   color="temp logger")) +
#     geom_line(data=sst_bottom[sst_bottom$Site==site_vector[s],],
#               aes(x=Date,y=Temperature,
#                   color="SST")) +
#     ggtitle(paste(site_vector[s]))
# }
# rm(s)

# run this do.call code to plot modified SST (simulated bottom temp),
# actual bottom temp, and SST temp (raw and flagged observations)
# do.call(grid.arrange,c(pL,ncol=2))





