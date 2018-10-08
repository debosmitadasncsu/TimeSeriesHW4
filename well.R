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
well.ts <- ts(well$height, start = 2007, frequency = 8765.25)

#adj_well.ts <- ts(well$adj_height, start = 2007, frequency = 8765.25)
plot(well.ts)

#check decomposition
decomp_well <- stl(well.ts, s.window = 7)
plot(decomp_well)

decomp_adj <- stl(adj_well.ts, s.window = 7)
plot(decomp_adj)


#split into training and validation data sets

train <- subset(well.ts, end = length(well.ts) - 169)
test <- subset(well.ts, start = length(well.ts) -168)


#fit seasonality with fourier series
well.seasonal.fit <-Arima(train,order=c(0,0,0),xreg=fourier(train,K=5))

#Box.test(well.seasonal.fit$residuals, lag=10, type="Ljung-Box")

#fit seasonality with differencing
well.seasonal.fit.diff <- diff(well.ts, lag=8766, differences=1)
Box.test(well.seasonal.fit.diff, lag=10, type="Ljung-Box")

#lag.plot(well.ts, lag = 2, main = "Scatterplots of Y with First 2 Lags", diag = FALSE, layout = c(1, 2))
#write.csv(well, "C:\\Users\\amyha\\Dropbox\\NCSU-MSA\\MSA - Fall\\Time Series II\\Well_Data\\Well Data\\adj_well_data.csv")
