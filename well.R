library(readxl)
library(dplyr)
library(lubridate)
library(timeDate)
library(zoo)
library(tseries)
library(forecast)
library(ggplot2)

#read in well csv
well_orig <- read_excel("C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Well_Data\\Well Data\\G-3549.xlsx", sheet = 3)

#roll-up to hourly data
well_hourly <- well_orig %>% 
  group_by(date(date), hour(time), tz_cd) %>% 
  summarise(well_ft = mean(Well_ft, na.rm = TRUE), 
            corrected = mean(Corrected, na.rm=TRUE)) %>%
  rename(hours = `hour(time)`, date=`date(date)`)

#combine date and hour columns
well_hourly$date <- as_datetime(well_hourly$date)
hour(well_hourly$date) <- well_hourly$hours


#adjust time for daylight savings
#create new column, adjust time, convert back to datetime
well_hourly$datetime_correct <- well_hourly$date
well_hourly$datetime_correct <- ifelse(well_hourly$tz_cd == "EDT", well_hourly$datetime_correct - hours(1), well_hourly$datetime_correct)
well_hourly$datetime_correct <- as_datetime(well_hourly$datetime_correct)

#Make time sequence to join in and make missing values where there are gaps
ts <- data.frame(timeSequence(from = "2007-10-01", to = "2018-06-13", by = "hours"))
colnames(ts) <- "time_series" 

#match well to time series
join <- well_hourly %>% 
  right_join(ts, by = c("datetime_correct" = "time_series")) 

#remove extra 2 hours at end of join
join <- join[-c(93792:93793),]

#create df with only corrected datetime and corrected well height
well <- join[,c(6,5)]

#impute missing values
#create zoo object, impute, convert back to df, rename columns
well.z <- zoo(x = well$corrected, well$datetime_correct) #Convert to zoo object to impute
well.z <- na.approx(well.z) #Impute the missing values
well <- fortify.zoo(well.z)
names(well)[1] <- "datetime"
names(well)[2] <- "height"

#well has negative values, create shifted height to check constant variance
#well$adj_height = log(well$height+1)

#make ts 
#frequency = 365.25* 24 because we have annual season with hourly data
well.ts <- ts(well$height, start = c(2007.75), frequency = 8766)

well.ts <- msts(well$height, start = c(2007.75), seasonal.periods=c(8766,24*7))
plot(well.ts)


#check decomposition
decomp_well <- stl(well.ts, s.window = 25)
plot(decomp_well)


#we want to use 4 years of data.
#create new df with data starting at June 1, 2014
well_short <- well[58441:93504,]
well_short.ts <- msts(well_short$height,start = c(2014.5), seasonal.periods=c(8766,24*14))
plot(mstl(well_short.ts, s.window=25))


well_2year <- well[72337:93791,]
well_2year.ts <- msts(well_2year$height,start = c(2016), seasonal.periods=c(8766,24))
plot(mstl(well_2year.ts, s.window=25))

#split into training and validation data sets

train <- subset(well_short.ts, end = length(well_short.ts) - 169)
test <- subset(well_short.ts, start = length(well_short.ts) - 168)

well.ts <- msts(well_short$height, start = c(2014.5), seasonal.periods=c(8766,24*14))
plot(mstl(well.ts, s.window = 25))

#fit seasonality with fourier series
#using k=20, because simmons said it could be a large number
#anything over 20 is very slow
#still appears to be trend, add index variable to fit trend
x<- cbind(fourier(train,K=10), seq(1:length(train)))

fit.fourier <-Arima(train,order=c(0,0,0),xreg=x)

#check stationarity of residuals
#appears to be stationary (p-value = 0.01)
adf.test(fit.fourier$residuals, k=10)

#plot ACF and PACF
tsdisplay(fit.fourier$residuals)
Box.test(fit.fourier$residuals, lag=10, type="Ljung-Box")

fit.fourier2 <-Arima(train,order=c(12,0,12),
                     xreg=x)
Box.test(fit.fourier2$residuals, lag=10, type="Ljung-Box")

#Box.test(well.seasonal.fit$residuals, lag=10, type="Ljung-Box")

#fit seasonality with differencing
fit.diff <- Arima(train,order=c(0,0,0), seasonal=c(0,8766,0))


#Box.test(well.seasonal.fit.diff, lag=10, type="Ljung-Box")
#lag.plot(well.ts, lag = 2, main = "Scatterplots of Y with First 2 Lags", diag = FALSE, layout = c(1, 2))
#write.csv(well_end, "C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Well_Data\\Well Data\\adj_well_data.csv")
