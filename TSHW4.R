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

# Build a holdout dataset - training has data of 4 years except one month - validation has only 1 month of data
Ending <- nrow(cleaned_well) - 720
Well_Training= cleaned_well[58008:Ending,]
Well_Validation= cleaned_well[(Ending+1):nrow(cleaned_well),]
nrow(cleaned_well)
nrow(Well_Training)
nrow(Well_Validation)

Well_Training <- ts(Well_Training$appx.Corrected, start = c(2014,5), frequency = 8766)
Well_Validation <- ts(Well_Validation$appx.Corrected, start = c(2018,5), frequency = 24)

#decomposition of time
decomp_stl <- stl(Well_Training,s.window=24)
plot(decomp_stl)


#### Sine and Cosine in ARIMA
index.ts=seq(1,length(Well_Training))
x1.sin=sin(2*pi*index.ts*1/8766)
x1.cos=cos(2*pi*index.ts*1/8766)
x2.sin=sin(2*pi*index.ts*2/8766)
x2.cos=cos(2*pi*index.ts*2/8766)
x3.sin=sin(2*pi*index.ts*3/8766)
x3.cos=cos(2*pi*index.ts*3/8766)
x4.sin=sin(2*pi*index.ts*4/8766)
x4.cos=cos(2*pi*index.ts*4/8766)
x.reg=cbind(x1.sin,x1.cos)
arima.1<-Arima(Well_Training,order=c(0,0,0),xreg=x.reg)
summary(arima.1)

# 
# 
# Results
# # 
# Series: Well_Training 
# Regression with ARIMA(0,0,0) errors 
# 
# Coefficients:
#   intercept  x1.sin   x1.cos
# 0.2468  0.2495  -0.1822
# s.e.     0.0013  0.0018   0.0018
# 
# sigma^2 estimated as 0.05799:  log likelihood=169.98
# AIC=-331.96   AICc=-331.96   BIC=-298.1
# 
# Training set error measures:
#   ME      RMSE      MAE       MPE     MAPE     MASE      ACF1
# Training set -1.489569e-14 0.2408006 0.176406 -14.14593 230.0092 0.692116 0.9987756
