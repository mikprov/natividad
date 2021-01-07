# Functions


# ---
# function to calc moving average: x=time series, n=window length
#ma <- function(x, n = 2){stats::filter(x, rep(1 / n, n), sides = 2)} 
ma <- function(x, n) {rollapply(x, width=n, FUN=function(x) mean(x, na.rm=TRUE), by=1, fill=NA)}
# ---
# function to shift daily T ahead: x=dataframe$colname, n=lagtime
shift <- function(x, n){  c( rep(NA,n) , x[1:(length(x)-n)] )  }

# ---
# Returns df with each site's offset

calc_offset <- function(lagtime,combo,smoothingwindow){
  
  # lagtime = time in days, start with 30
  # combo = combo df
  # smoothingwindow = time in days, start with 30
  
  # prep for loop
  #offsetplots <- as.list(rep(NA,length=length(unique(combo$Site))))
  site_offsets <- rep(NA,length=length(unique(combo$Site)))
  
  for(s in 1:length(unique(combo$Site))){
    # data for site s 
    dd <- combo[combo$Site==unique(combo$Site)[s],]
    dd$month <- as.numeric(format(dd$Date, "%m"))
    
    # smooth temp with 30 day window
    dd$smoothSST <- ma(dd$temp_sst,n=smoothingwindow)
    dd$smoothbot <- ma(dd$temp_bottom,n=smoothingwindow)
    
    # shift smooth SST 30 days ahead
    dd$smoothlagSST <- shift(x=dd$smoothSST,n=lagtime)
    
    # difference between smooth bottom & lagged smSST
    dd$diff_in_smoothed <- dd$smoothlagSST - dd$smoothbot
    dd$diff_in_smoothed <- as.numeric(dd$diff_in_smoothed)
    
    # average offset between smoothed SST and bottom
    site_offsets[s] <- mean(dd$diff_in_smoothed,na.rm=TRUE)
    
    # dd$monthF <- factor(dd$month) # for plotting
    # 
    # # plot monthly distribution of offsets
    # offsetplots[[s]] <- 
    #   ggplot(data=dd,aes(x=monthF,y=diff_in_smoothed)) + 
    #   geom_boxplot() +
    #   ggtitle(paste(unique(combo$Site)[s])) +
    #   ylab("Daily differences (deg C)") +
    #   xlab("Month") +
    #   theme_classic()
  
  }
  site_offsets <- data.frame(mean_diff = as.numeric(as.character(site_offsets)),Site = unique(combo$Site))
  site_offsets$Site <- factor(site_offsets$Site, levels=site_offsets[order(site_offsets$mean_diff),]$Site, ordered=TRUE)
  
  return(site_offsets) #df with each site's offset, list of monthly offset plots
  rm(s)
}

# ---
## Rolling 4 day window of stress accumulation 

#This model calculates the linear regression coefficients on 4 days of temp data. And then estimates the 'expected' temperature on the 4th day. This 'expected' temperature is one data point in the smoothed function. The difference between the 'expected' temperature and 'experienced' is a measure of stress. 

calc_stress <- function(d,weights){
  
  # d = data frame with at least these columns: Temperature,Date
  # wtsp = vector of weights that say which days should count more in regression
  # the length(wtsp) is the window length
  # wts <- c(0.1,0.2,0.4,0.8)
  # wts <- wts/sum(wts)
  # wtsp <- round(wts*100,digits=0)
  # weights = wtsp
  # d = bottomall[bottomall$Site == site_vector[9],]
  # 
  window <- length(weights)
  #nrow(d)
  # -- Calculate Expected T in rolling window -- #
  #d <- d[complete.cases(d),]
  
  expectedT <- rep(NA,length=length(d$Date)-window)
  Date <- rep(NA,length=length(expectedT))
  expT <- data.frame(cbind(expectedT,Date))
  rm(expectedT,Date)
  
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
  }
  expT$Date <- as.Date(expT$Date,format="%Y-%m-%d")
  d$Date <- as.Date(d$Date,format="%Y-%m-%d")
  d <- as.data.frame(d)
  d2 <- left_join(d,expT,by="Date")
  d2$Dstress <- d2$Temperature - d2$expectedT
  rm(i,tsub,texp)
  return(d2)
  
}




# library(pracma)
# # Use spectrum to generate new time series -- NOT WORKING
# 
#   # Snippet of code for replicating time series with the same spectrum as ENSO signal:
#   # Z = fft(ENSO) # do the FFT
#   # Theta = rand(length(ENSO)) * 2 * pi # obtain uniform random variables on the interval [0 2pi]
#   # Z = abs(Z) .* exp(1i * Theta) # add noise to the phase of the spectrum (the imaginary part)
#   # Z2 = ifft(Z) # IFFT to return to the time domain
# 
# # fft(x) in Matlab returns the Fourier transform of x
# # fft(x) in R computes the discrete Fourier transform. If x is a vector, the value returned is the unnormalized univariate discrete Fourier transform 
# # ifft(z) returns the value of the normalized discrete, univariate, inverse Fast Fourier Transform of the values
# 
# ggplot(data=combo[combo$Site == "Clam_Bay",]) + 
#   geom_line(aes(x=Date,y=temp_bottom,color="bottom")) +
#   geom_line(aes(x=Date,y=temp_sst,color="SST"))
# 

# smoother <- sqrt(length(combo[combo$Site == "Clam_Bay",]$temp_bottom))
# smoother = 19
# 
# b <- spectrum(combo[combo$Site == "Clam_Bay",]$temp_bottom,span=NULL,demean=TRUE,plot=FALSE,log="no")
# s <- spectrum(combo[combo$Site == "Clam_Bay",]$temp_sst,span=NULL,plot=TRUE)
# df <- data.frame(
#   freq.b = b$freq,
#   spec.b = b$spec,
#   freq.s = s$freq,
#   spec.s = s$spec)
# 
# trapz(x=b$freq,y=b$spec*2)
# var(combo[combo$Site == "Clam_Bay",]$temp_bottom)
# sum(df$spec.b*2*min(df$freq.b))
# var(combo[combo$Site == "Clam_Bay",]$temp_sst)
# 
# 
# ggplot(data=df) +
#   geom_line(aes(x=freq.b,y=log10(spec.b),color="bottom")) +
#   geom_line(aes(x=freq.s,y=log10(spec.s),color="sst"))
# 
# 
# 
# 
# # Can I modify this code???
# ENSO <- combo[combo$Site == "Clam_Bay",]$temp_sst #time series
# Z <- (fft(ENSO)*2)/length(ENSO) #vector of complex numbers (read + imaginary); use 'z' to refer to complex numbers
# Zabs <- abs(Z)
# Zabs2 <- Zabs^2
# 
# 
# Theta <- runif(n=length(ENSO),min=0,max=1)*2*pi # obtain uniform random variables on the interval [0 2pi]
# Z = abs(Z) * exp(1i * Theta) # add noise to the phase of the spectrum (the imaginary part); in Matlab '.*' is element-wise multiplication
# Z2 = ifft(Z) # IFFT to return to the time domain
# 
# 
# 
# 
# 
# # z2sp <- spec.pgram(Z2,spans=NULL,plot=FALSE)
# # ensosp <-spec.pgram(ENSO,spans=NULL,plot=FALSE)
# # 
# # sp <- data.frame(
# #   Z2spec = z2sp$spec,
# #   ensospec = ensosp$spec,
# #   freqZ2 = z2sp$freq,
# #   freqENSO = ensosp$freq)
# #   
# # ggplot(data=sp) + 
# #   geom_line(aes(x=freqZ2,y=log10(Z2spec),color="Z2")) + 
# #   geom_line(aes(x=freqZ2,y=log10(ensospec),color="enso")) 
# # 
# # 
# # ts <- data.frame(
# #   sstraw = ENSO,
# #   sstnew = Z2,
# #   day = seq(from=1,to=length(Z2),by=1))
# # ggplot(data=ts) + 
# #   geom_line(aes(x=day,y=sstraw,color="sst, raw")) + 
# #   geom_point(aes(x=day,y=sstnew,color="sst, new")) 
# # 
# 
# 
# 
