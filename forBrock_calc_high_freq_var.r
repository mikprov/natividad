# For Brock:
# Calc high freq var in SST and bottom logger data

# all SST site files:
SSTfede <- read.csv("C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/forBrock_high_freq_var/SSTfede.csv")

# pull out raw SST observations, not interpolated
SSTfede$temp_sst_raw <- rep(NA,length=length(SSTfede[,1])) 
SSTfede$temp_sst_raw <- ifelse(SSTfede$flag < 1, SSTfede$Temperature, "NA")
SSTfede$temp_sst_raw <- as.numeric(as.character(SSTfede$temp_sst_raw))

# deviations between raw SST and Brock's climatology:
SSTfede$devs <- SSTfede$Climatology - SSTfede$temp_sst_raw 

site_vector <- unique(SSTfede$Site) # use in for loops
years <- unique(SSTfede$year) # use in for loops

# ---
# calc high freq variance in SST  
# ---

# empty df to store average yearly high freq variance per site (averaged over years)
variance_reg <- data.frame(
  Site = site_vector,
  variance_SST = rep(NA,length=length(site_vector)))

for(s in 1:length(site_vector)){ #for each site
  var_years <- rep(NA,length(years)) # create empty vector for variance in each year
  
  for(y in 1:length(years)){ #step through each year
    var_years[y] <- # calculate variance for that year-site using the raw temp deviations
      var(SSTfede[SSTfede$Site==site_vector[s] & SSTfede$year==years[y], ]$devs,na.rm=TRUE)
  }
  variance_reg[variance_reg$Site==site_vector[s],]$variance_SST <- 
    mean(var_years) # average the variances across years per site
}
rm(y,s,var_years) #clean up


# ---
# Bottom logger data
# ---

bottomall <- read.csv(bottomall,file="C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/forBrock_high_freq_var/bottomall.csv")

# keep only one temp observation per day (miniDOT collects temp every 10 min)
bottomall <- bottomall %>% group_by(Site,Date) %>% sample_n(1)

site_vector <- unique(bottomall$Site) # use in for loops


# ----------
# bottom spectrums 

sampleinterval = 10/(10*6*24) #units is days, 10*6=min in hr, *24=min in day

pBR <- as.list(rep(NA,length=length(site_vector))) #for plots
pBRlog <- as.list(rep(NA,length=length(site_vector))) #for plots, yaxis is log

regressiondf <- data.frame( #store bottom high freq var here
  Site = site_vector,
  highvar_bottom = rep(NA,length=length(site_vector)))

for(s in 1:length(site_vector)){ #for each site
  
  # subset to site
  d <- bottomall[bottomall$Site == site_vector[s],]
  
  # bottom spectrum 
  spR <- spec.pgram(d$temp_bottom,spans=NULL,taper=0.1,plot = FALSE,demean = TRUE)
    # demean: removes and overall increasing or decreasing trend
    # taper: remove 10% of time series on either end 
    # spans: I could set a window over which to smooth, I don't do that here
  
  # df for plotting
  spRdf <- data.frame(
    freq = spR$freq/sampleinterval, #divide freq by the sampleinterval, units=freq/day
    freq1 = spR$freq, #just freq
    spec = spR$spec*(spR$freq/sampleinterval), #multiply spec by freq/day
    spec1 = spR$spec)
  
  # store plots
  pBRlog[[s]] <- ggplot(data=spRdf,aes(x=log10(freq),y=log10(spec))) + 
    geom_line() + ggtitle(paste(d$Site[1]," - 10 min data",sep="")) +
    xlab("log10(freq (1/day))") + ylab("log10(spec*freq)") +
    theme(legend.position="none") 
  
  pBR[[s]] <- ggplot(data=spRdf[spRdf$freq < 10,],aes(x=freq,y=log10(spec))) + 
    geom_line() + ggtitle(paste(d$Site[1]," - 10 min data",sep="")) +
    xlab("freq (1/day)") + ylab("log10(spec*freq)") +
    theme(legend.position="none") 
  
  # try different high freq intervals 
  # regressiondf[regressiondf$Site == site_vector[s],]$highvar_bottom <-
  #   trapz(x=spRdf[spRdf$freq > 0.75 & spRdf$freq < 2.25 ,]$freq1,
  #         y=spRdf[spRdf$freq > 0.75 & spRdf$freq < 2.25 ,]$spec1 * 2)
  
  # high freqs = >0.1
  regressiondf[regressiondf$Site == site_vector[s],]$highvar_bottom <-
    trapz(x=spRdf[spRdf$freq > 0.1 ,]$freq1,
          y=spRdf[spRdf$freq > 0.1 ,]$spec1 * 2) #multiply by 2 bc Helen Wearing says so
  
}
rm(spRdf,d,s,spR,site_vector)


# merge SST and bottom var numbers, relabel site names based on conversation w/B&F
regdf <- left_join(regressiondf,variance_reg)
sitekey <- read.csv(file="C:/Users/Mikaela/Box Sync/Hopkins_postdoc/natividad/data/siteskey_fixed.csv",stringsAsFactors = F)
colnames(sitekey)[colnames(sitekey)=="bsites"] <- "Site"
sitekey$Ssites <- NULL
regdf <- left_join(regdf,sitekey)
rm(variance_reg,sitekey)

