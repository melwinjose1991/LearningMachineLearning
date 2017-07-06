# Function for checking and installing reuqired packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages() [, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only=TRUE)
}

# Creating package Vector
ts_pckg <- c("devtools", "ggplot2", "lubridate", "zoo", "xts", "tidyverse", "xlsx", 
             "openxlsx", "Deducer", "Hmisc", "timeSeries", "Deducer", "tframePlus", "reshape2",
             "tseries", "forecast", "xts", "zoo")

#"reshape2","scales", "printr", "graphics","gridExtra","knitr", 
#"zoo", "xts", "dplyr", "plyr", "MTS", "timekit", "TTR", "astsa"

# Initializing ipak function for installing and loading of the req packages
ipak(ts_pckg)

# Loading Data in R
data <- read.csv("TS_R1.csv", stringsAsFactors = FALSE, header = TRUE)
str(data)

#Converting to data frame
df <- data.frame(data)

# Dataset Dimensions 
numMonths <- nrow(df)
numItems <-ncol(df) -1

# Assigning proper data type
#For Items
for(i in 2:numItems) {
  df[,i] <- as.numeric(df[,i])
}

#For Date
df$Date<-as.Date(df[,1], "%m/%d/%Y")

# Adding seperate columns for Month & Year
df$Month <- months(df$Date)
df$Year <- year(df$Date)

colnames(df)

#Assigning proper class to new added Variables
class(df$Month)
df$Month <- as.factor(df$Month)


df$Month <- factor(df$Month, levels = c("January","February","March","April",
                                        "May","June","July","August","September",
                                        "October","November","December"),ordered=TRUE)

# Distribution Transformer

par(mfrow=c(1,2))
ts.DistributionTransformers <- ts(df$DistributionTransformers)

plot(ts.DistributionTransformers,
     main="Actual sale of Distribution Transformer" ,
     col="navy" ,
     ylab="Distribution Transformer (Actual Sale)")

ts.DistributionTransformersMean <- ts(yearlyMean$DistributionTransformers,
                                      start = c(2013,1), end = c(2016,12) ,
                                      frequency = 1)

View(ts.DistributionTransformersMean)

plot(ts.DistributionTransformersMean,
     main="Average Sale of Distribution Transformer" ,
     col="darkred" ,
     ylab="Distribution Transformer (Aggregated for Year)")



# Creating a holdout sample for Model Accuracy test

train <- df[1:nrow(df),]
View(train)

# Examining the Distribution transformer data

train$Date <- as.Date(train$Date)

ggplot(train, aes(Date, train$DistributionTransformers)) + geom_line() +
  scale_x_date("Year") + ylab("Monthly Sale of Distribution Transformer") + xlab("")

# Outlier and missing value treatment

sale <- ts(train[, c("DistributionTransformers")])

train$cleaned_Dist_Transf <- tsclean(sale)

ggplot() + 
  geom_line(data = train, aes(x = Date, y = cleaned_Dist_Transf)) +
  ylab("Cleaned Distribution Transfoer sale Data")

# Plot smoothing

train$dt_ma = ma(train$cleaned_Dist_Transf, order =3) # using cleaned ddata with quaterly MA

train$dt_ma

ggplot() + 
  geom_line(data = train, aes(x = Date, y = cleaned_Dist_Transf, colour = "Dist_Trans_Sale")) +
  geom_line(data = train, aes(x = Date, y = dt_ma, colour = "Quaterly MA")) +
  ylab("Dist_Trans Sale")

#Datesets for Time series Analysis

dist.trans <- ts(na.omit(train$DistributionTransformers), frequency = 12)
dist.trans_ma <- ts(na.omit(train$dt_ma), frequency = 12)
dist.trans_qtr<- aggregate(dist.trans, nfrequency=4, mean)

        ###################################################################
        ##       ************ Monthly Forecast ******************       ##
        ###################################################################

# Decomposing the Time Series
tsdisplay(dist.trans, lag.max = 50, xlab = "Year 2013 to 2016", 
          ylab = "Price of Disttribution Transformer")

decomp_sale <- stl(dist.trans, s.window = "periodic")
decomp_sale$time.series
deseasonal_sale <- seasadj(decomp_sale)

plot(decomp_sale)

# Checking Stationarity of the series

adf.test(deseasonal_sale, alternative = "stationary") # Test Fails
kpss.test(deseasonal_sale, null="Trend")

Acf(deseasonal_sale, main="")
Pacf(deseasonal_sale, main="")

# Diffrencing the series for stationarity

diff_deseasonal.sale <- diff(deseasonal_sale, difference = 1)

plot(diff_deseasonal.sale)

tsdisplay(deseasonal_sale, xlab = "Year 2013 to 2016", ylab = "Price of Disttribution Transformer")

adf.test(diff_deseasonal.sale, alternative = "stationary")#Test Passed Series Stationary, hence diff is req
kpss.test(diff_deseasonal.sale, null = "Trend")

Acf(diff_deseasonal.sale)
Pacf(diff_deseasonal.sale)

# Using Auto ARIMA function to check for the best ARIMA Model

auto.arima(deseasonal_sale, seasonal = FALSE) # ARIMA(1,1,0) with zero mean
auto.arima(deseasonal_sale, seasonal = TRUE)  # ARIMA(1,1,0)(1,0,0)[12] with zero mean


# Testing Best Auto ARIMA Model 
fit <- auto.arima(deseasonal_sale, seasonal = FALSE)
fit_seasonality <- auto.arima(deseasonal_sale, seasonal = TRUE)

tsdiag(fit,gof.lag=12)
tsdiag(fit_seasonality, gof.lag = 12)

tsdisplay(residuals(fit), lag.max = 45, main = "ARIMA(1,0,0) Model residual")
tsdisplay(residuals(fit_seasonality), lag.max = 45, main = "ARIMA(1,1,0)(1,0,0)[12] Model residual")

# Testing the model with holdout sample

hold <- window(ts(deseasonal_sale), start=42)

fit_no_holdout <- arima(ts(deseasonal_sale[-c(43:48)]),order = c(1,1,0), 
                        seasonal = list(order=c(1,0,0)))

fcast_no_holdout <- forecast(fit_no_holdout, h=6)

# Using the best model to forecast

fcast <- forecast(fit_seasonality, h=6)
fcast
plot(fcast)


              ###################################################################
              ##    ******* Monthly Forecast w/ Moving Average *********       ##
              ###################################################################


# Decomposing the Time Series
tsdisplay(dist.trans_ma, lag.max = 50, xlab = "Year 2013 to 2016", 
          ylab = "Price of Disttribution Transformer")

decomp_sale <- stl(dist.trans_ma, s.window = "periodic")
decomp_sale$time.series
deseasonal_sale <- seasadj(decomp_sale)

plot(decomp_sale)

# Checking Stationarity of the series

adf.test(deseasonal_sale, alternative = "stationary") # Test Fails
kpss.test(deseasonal_sale, null="Trend")

Acf(deseasonal_sale, main="")
Pacf(deseasonal_sale, main="")

# Diffrencing the series for stationarity

diff_deseasonal.sale <- diff(deseasonal_sale, difference = 1)

plot(diff_deseasonal.sale)

tsdisplay(deseasonal_sale, xlab = "Year 2013 to 2016", ylab = "Price of Disttribution Transformer")

adf.test(diff_deseasonal.sale, alternative = "stationary")#Test Passed Series Stationary, hence diff is req
kpss.test(diff_deseasonal.sale, null = "Trend")

Acf(diff_deseasonal.sale)
Pacf(diff_deseasonal.sale)

# Using Auto ARIMA function to check for the best ARIMA Model

auto.arima(deseasonal_sale, seasonal = FALSE) # ARIMA(0,1,3) with zero mean
auto.arima(deseasonal_sale, seasonal = TRUE)  # ARIMA(0,1,0)(1,0,0)[12] with zero mean


# Testing Best Auto ARIMA Model 
fit <- auto.arima(deseasonal_sale, seasonal = FALSE)
fit_seasonality <- auto.arima(deseasonal_sale, seasonal = TRUE)

tsdiag(fit,gof.lag=12)
tsdiag(fit_seasonality, gof.lag = 12)

tsdisplay(residuals(fit), lag.max = 45, main = "ARIMA(0,1,3) Model residual")
tsdisplay(residuals(fit_seasonality), lag.max = 45, main = "ARIMA(0,1,0)(1,0,0)[12] Model residual")

# Testing the model with holdout sample with Ma term set to 15 as per ACF of seasionality integrated model

hold <- window(ts(deseasonal_sale), start=36)

fit_no_holdout <- arima(ts(deseasonal_sale[-c(37:46)]),order = c(0,1,15), 
                        seasonal = list(order=c(1,0,0)))

fcast_no_holdout <- forecast(fit_no_holdout, h=10)

View(fcast_no_holdout)

plot(fcast_no_holdout)
lines(ts(deseasonal_sale))

# Using the best model to forecast

fcast <- forecast(arima(deseasonal_sale, order =c(0,1,15), seasonal = list(order=c(1,0,0))), h=12)
fcast
plot(fcast)

                ###################################################################
                ##            ******* Quarterly Mean Forecast *********          ##
                ###################################################################


# Decomposing the Time Series
tsdisplay(dist.trans_qtr, lag.max = 16, xlab = "Year 2013 to 2016", 
          ylab = "Price of Disttribution Transformer")

decomp_sale <- stl(dist.trans_qtr, s.window = "periodic")
decomp_sale$time.series
deseasonal_sale <- seasadj(decomp_sale)

plot(decomp_sale)

# Checking Stationarity of the series

adf.test(deseasonal_sale, alternative = "stationary") # Test Fails
kpss.test(deseasonal_sale)

Acf(deseasonal_sale, main="")
Pacf(deseasonal_sale, main="")

# Diffrencing the series for stationarity

diff_deseasonal.sale <- diff(deseasonal_sale, difference = 0)

plot(diff_deseasonal.sale)

tsdisplay(deseasonal_sale, xlab = "Year 2013 to 2016", ylab = "QTR Price of Disttribution Transformer")

adf.test(diff_deseasonal.sale, alternative = "stationary")#Test Passed Series Stationary, hence diff is req
kpss.test(diff_deseasonal.sale, null = "Trend")

Acf(diff_deseasonal.sale)
Pacf(diff_deseasonal.sale)

# Using Auto ARIMA function to check for the best ARIMA Model

auto.arima(deseasonal_sale, seasonal = FALSE) # ARIMA(0,1,0) with zero mean
auto.arima(deseasonal_sale, seasonal = TRUE)  # ARIMA(0,1,0)(0,0,0)[12] with zero mean


# Testing Best Auto ARIMA Model 
fit <- auto.arima(deseasonal_sale, seasonal = FALSE)
fit_seasonality <- auto.arima(deseasonal_sale, seasonal = TRUE)

tsdiag(fit,gof.lag=12)
tsdiag(fit_seasonality, gof.lag = 12)

tsdisplay(residuals(fit), lag.max = 16, main = "ARIMA(0,1,0) Model residual")
tsdisplay(residuals(fit_seasonality), lag.max = 16, main = "ARIMA(0,1,0)(0,0,0)[12] Model residual")

# Testing the model with holdout sample with Ma term set to 15 as per ACF of seasionality integrated model

hold <- window(ts(deseasonal_sale), start=12)

fit_no_holdout <- arima(ts(deseasonal_sale[-c(13:16)]),order = c(0,1,0), 
                        seasonal = list(order=c(0,0,0)))

fcast_no_holdout <- forecast(fit_no_holdout, h=4)

fcast_no_holdout

plot(fcast_no_holdout)
lines(ts(deseasonal_sale))

# Using the best model to forecast

fcast <- forecast(fit_seasonality, h=4)
fcast
plot(fcast)

