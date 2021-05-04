##################################################
### Time Series                                 ## 
##################################################
#                                               ##
##################################################
# Written by Bharat Thakur                      ##
#                                               ##
#                                               ##
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/Kala/Google Drive/Conestoga/Data Analysis/Assignment 3 - Time Series")

options(scipen=9)


##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(tseries)){install.packages("tseries")}
library("tseries")
  
if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

####################################################
## SECTION 1                                      ##
## TIME SERIES  - Monthly Temperature -Welland    ##
####################################################


###################################################
## 1.Data Transformation-Convert to Time Series  ##
###################################################

#Read in the data
Temperature_BT <- read.csv("Welland20F.csv", header=TRUE)
head(Temperature_BT)


#Dropping the Date Column
Temperature_BT <- Temperature_BT[c(-1)]
head(Temperature_BT)

#Convert to a Time Series datatype
TempStudy_BT <- ts(Temperature_BT, frequency = 12, start=c(1985,1))
head(TempStudy_BT)


###################################################
## 2. Descriptive Data Analysis                  ##
###################################################

#1. Summarize
stat.desc(TempStudy_BT)


#2. PLOT THE TIME SERIES 
plot.ts(TempStudy_BT, main="Average Monthly Temperature - Welland",  ylim = c(-12, 25)) 



#3. Decompose
decompTemp_BT <- decompose(TempStudy_BT, type="additive")  
#decompTemp_BT
plot(decompTemp_BT)


#4 Stationarity
adf.test(TempStudy_BT) 

#5. Deseasonalize

TempStudy_Seas_Adj_BT <- TempStudy_BT - decompTemp_BT$seasonal
#head(TempStudy_Seas_Adj_BT)

plot.ts(TempStudy_Seas_Adj_BT, main="Deseasonalized - Average Temperature Monthly - Welland", ylim = c(-15, 25))

#####################################################
## SECTION 2                                       ##
## TIME SERIES - Waterloo  Total Precipitation     ##
#####################################################


###################################################
## 1.Data Transformation-Convert to Time Series  ##
###################################################



Precipitation_BT <- read.csv("Waterloo20F.csv", header=TRUE)
head(Precipitation_BT)

#Dropping the Date Column
Precipitation_BT <- Precipitation_BT[c(-1)]
head(Precipitation_BT)

PrecipStudy_BT <- ts(Precipitation_BT, frequency = 1, start=c(1970))  #Converts to Time Series
head(PrecipStudy_BT)


###################################################
## 2. Descriptive Data Analysis                  ##
###################################################

#1. Summarize
stat.desc(PrecipStudy_BT)


#2. PLOT THE TIME SERIES ####

plot.ts(PrecipStudy_BT, main="Total Precipitation - Waterloo")



#3. Spot trends by smoothing
par(mfrow=c(4,1))

plot.ts(PrecipStudy_BT, main="Total Precip - Waterloo")

PrecipStudySMA2_BT <- SMA(PrecipStudy_BT,n=2)
plot.ts(PrecipStudySMA2_BT, main="Moving Average (2 Years)") #Too much noise

PrecipStudySMA3_BT <- SMA(PrecipStudy_BT,n=3)
plot.ts(PrecipStudySMA3_BT, main="Moving Average (3 Years)") 

PrecipStudySMA5_BT <- SMA(PrecipStudy_BT,n=5)
plot.ts(PrecipStudySMA5_BT, main="Moving Average (5 Years)")   

par(mfrow=c(1,1))


#4. Stationarity
adf.test(PrecipStudy_BT) 

#5. Autocorrelations
acf(PrecipStudy_BT) 

###################################################
## 3. Prediction                                 ##
###################################################

#3.1 Moving Average Forecast- Let's build a forecast

move_avg_BT <- sma(PrecipStudy_BT)
move_avg_BT
move_avg_BT <- forecast(move_avg_BT, h=5,level=0.75)   #h - periods to forecast; level=Prediction Level
move_avg_BT
plot(move_avg_BT, main ='Simple Moving Average Forecast (C.I. 75%)')

#3.2 Exponential Smoothed Forecast

ES_avg_BT <- es(PrecipStudy_BT)
ES_avg_BT
ES_avg_BT <- forecast(ES_avg_BT, h=5,level=0.75)
ES_avg_BT
plot(ES_avg_BT, main ='Exponentially Smoothed Forecast (C.I. 75%)') 

