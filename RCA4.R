
# To read the Healthcare dataset
df <- read.csv("C:/Users/intel/Desktop/data scince pdf/Health.csv")

# viewing the structure of the dataset
View(df)
head(df)
str(df)

# To rename and extract the columns in a separate dataframe
df2 <- subset(df, select = c(TotalDischargesMale, TotalDischargesFemale, Year))
View(df2)

# Performing Data exploratory analysis by  removing blank spaces, strings, null values. 

df2$TotalDischargesFemale <- as.numeric(gsub(",","",df2$TotalDischargesFemale))

df2$TotalDischargesMale <- as.numeric(gsub(",","",df2$TotalDischargesMale))

df2$Year <- as.numeric(df2$Year)

View(df2)

# Eliminating the  null values for data exploratory analysis

df2[df2 == "0"] <- NA
df2 <- na.omit(df2)

View(df2)

# Extracting only Male Discharges by creating subsets
library(dplyr)

DischargesMale <- subset(df2, select=c(TotalDischargesMale,Year))
str(DischargesMale)

View(DischargesMale)

# Extracting only Female Discharges by creating subsets
library(dplyr)
DischargesFemale <- subset(df2, select=c(TotalDischargesFemale,Year))
str(DischargesFemale)


View(DischargesFemale)

# importing libraries for perfroming time series and forecasting
library('ggplot2')
library('forecast')
library('tseries')

#  Using ggplot to show the Total Female Discharges through the years

ts_FemaleDischarges <- ts(DischargesFemale$TotalDischargesFemale, start=c(2015, 1), end=c(2017), frequency=12)
ts_FemaleDischarges

ggseasonplot(ts_FemaleDischarges)

# Using ggplot to show the Total Male Discharges through the years
ts_MaleDischarges <- ts(DischargesMale$TotalDischargesMale, start=c(2015, 1), end=c(2017), frequency=12)
ts_MaleDischarges
ggseasonplot(ts_MaleDischarges)





# to plot the stationary series for Male Discharges
library('tseries')
ts_MaleDischarges <- ts(DischargesMale$TotalDischargesMale, start=c(2015, 1), end=c(2017), frequency=12)
plot(ts_MaleDischarges)

# Plotting the stationary series for Discharges[Female] - Tseries
library('tseries')
ts_FemaleDischarges <- ts(DischargesFemale$TotalDischargesFemale, start=c(2015, 1), end=c(2017), frequency=12)
plot(ts_FemaleDischarges)


## Adf Test
adf_Maledischarges <- ndiffs(ts_MaleDischarges, test='adf')  # 2
adf_Maledischarges
# KPSS test
ndiffs(y, test='kpss') 

# ACF series -  MaleDischarges

library(tseries)
library(forecast)

MaleDis_acf<- Acf(ts_MaleDischarges)
MaleDis_acf

# ACF series - FemaleDischarges
library(forecast)
library(tseries)
FemaleDis_acf <- Acf(ts_FemaleDischarges)
FemaleDis_acf

#  Perfoming Arima model for MaleDischarges
arima_MaleDis <- Arima(ts_MaleDischarges,  order = c(0,2,4))
arima_MaleDis 

forecast(arima_MaleDis)

# Arima model for FemaleDischarges
arima_FemaleDis <- Arima(ts_FemaleDischarges,  order = c(0,2,4))
arima_FemaleDis

forecast(arima_FemaleDis)

# MaleDischarges - qq plot  fitting line
# the source points falls on the line 
qqnorm(arima_MaleDis$residuals)
qqline(arima_MaleDis$residuals)

# qq plot for fiting line for FemaleDischarges
# the primary points falls on the line 
qqnorm(arima_FemaleDis$residuals)
qqline(arima_FemaleDis$residuals)


# Using Box plot for identifying the value fits for the model of FemaleDischarges

Box.test(arima_FemaleDis$residuals,type = "Ljung-Box")
forecast(arima_FemaleDis,15)

plot(forecast(arima_FemaleDis, 15), xlab = "Year", ylab = "Total Discharges for Female")


# Using Box plot for identifying the value fits for the model of MaleDischarges

Box.test(arima_MaleDis$residuals,type = "Ljung-Box")
forecast(arima_MaleDis,15)

plot(forecast(arima_MaleDis, 15), xlab = "Year", ylab = "Total Discharges for Male")

