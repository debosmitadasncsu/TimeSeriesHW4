library(haven)
library(forecast)
library(readxl)
library(lubridate)
library(magrittr)
library(dplyr)
library(xts)
library(ggplot2)


df <- read_xlsx("C:\\Users\\Debosmita\\Documents\\TimeSeriesHW4\\G-3549.xlsx", 3)


# time adjusted data
df1 <- df %>%
  mutate(
    hour = hour(time),
    EST = date + hours(
      ifelse( 
        hour == 0,
        24 + hour+ ifelse(tz_cd == 'EDT', -1, 0),
        hour+ ifelse(tz_cd == 'EDT', -1, 0)
      )
    ),
    EDT = date + hours(
      hour + ifelse(tz_cd == 'EST', 1, 0)
    ),
    UTC = (EDT + hours(4))
  )

# data with more than 1 observation per hour
df2 <- df1 %>%
  group_by(UTC) %>%
  summarise(n = n(), avg.Corrected = mean(Corrected)) %>%
  filter(n > 1)

# data with exaclty one observation per hour
df3 <- df1 %>%
  group_by(UTC) %>%
  summarise(n = n(), avg.Corrected = mean(Corrected)) %>%
  filter(n == 1)


EST = seq(df1[[1,'EST']], df1[[nrow(df1),'EST']], "hour")
EDT = seq(df1[[1,'EDT']], df1[[nrow(df1),'EDT']], "hour")
UTC = seq(df1[[1,'UTC']], df1[[nrow(df1),'UTC']], "hour")

cleaned_well <- data.frame(UTC) %>%
  full_join(
    df2 %>% rbind(df3)
  )


cleaned_well$appx.Corrected <- na.approx(cleaned_well$avg.Corrected)

remove(df, df1, df2, df3)

# Build a holdout dataset
Ending <- nrow(cleaned_well) - 168
Well_Training= cleaned_well[1:Ending,]
Well_Validation= cleaned_well[(Ending+1):nrow(cleaned_well),]
nrow(cleaned_well)
nrow(Well_Training)
nrow(Well_Validation)

Well_Training <- ts(Well_Training$avg.Corrected, start = c(2007,10), frequency = 8766)
Well_Validation <- ts(Well_Validation$avg.Corrected, start = 2007.75, frequency = 8766)

#decomposition of time series - need to check this - the seasonality is not looking good to me
#either it has multiple seasonal patterns or my window is wrong
decomp_stl <- stl(Well_Training,s.window=24,na.action = na.approx)
plot(decomp_stl)


#fitting sarima
fit <- auto.arima(Well_Training, seasonal=TRUE,
                  xreg=fourier(Well_Training, K=c(2,24)))
summary(fit)
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")



