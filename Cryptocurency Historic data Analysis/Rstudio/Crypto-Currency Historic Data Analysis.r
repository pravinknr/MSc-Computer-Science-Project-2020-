#Crypto-Currency Historic Data Analysis

library(forecast)
library(tseries)
library(lubridate)
library(factoextra)
library(zoo)
library(caret)
library(fpp)
library(smooth)
bitcoin_cash <- read.csv(file.choose())

bitcoin <- read.csv(file.choose())

bitconnect <- read.csv(file.choose())

dash <- read.csv(file.choose())

ethereum <- read.csv(file.choose())

ethereum_classic <- read.csv(file.choose())

iota <- read.csv(file.choose())

litecoin <- read.csv(file.choose())

monero <- read.csv(file.choose())

nem <- read.csv(file.choose())

neo <- read.csv(file.choose())

numeraire <- read.csv(file.choose())

omisego <- read.csv(file.choose())

qtum <- read.csv(file.choose())

ripple <- read.csv(file.choose())

stratis <- read.csv(file.choose())

waves <- read.csv(file.choose())

#Bitcoin_Cash
View(bitcoin_cash) #To view the dataset
summary(bitcoin_cash) #We get the Summary of the Data
str(bitcoin_cash) #Gives the Structure of the Dataset

#As we could see that the Volume as well as Market Cap variables are defined as Character While it should be a Numeric Variable
#Lets Convert them as Numeric Variable
bitcoin_cash$Market.Cap <- as.numeric(gsub(",", "", bitcoin_cash$Market.Cap))
bitcoin_cash$Volume <- as.numeric(gsub(",", "", bitcoin_cash$Volume))

boxplot(bitcoin_cash[,-1])

#bitcoin_cash[bitcoin_cash==0] <- NA #To replace 0 values with NA.
#is.na(bitcoin_cash) #To check if there is any NA value in the dataset
#bitcoin_cash <- na.omit(bitcoin_cash) #To remove NA values

typeof(bitcoin_cash$Date) #To check the Type of the Date Column
bitcoin_cash$Date <- as.Date(bitcoin_cash$Date, format = '%B %d, %Y') #To Convert the date Column 
bitcoin_cash$Date
bitcoin_cash <- bitcoin_cash[order(bitcoin_cash$Date),] #To sort the data with respect to date column


plot(bitcoin_cash$Date,bitcoin_cash$High, type = "l") #To Plot a Line chart
bitcoin_cash_High <- bitcoin_cash[,c(1,3)]

#Lets Add Dummy Variables for building multiple Models and comparing their RMSE values to Choose the Best Model for Prediction
#Lets Add a new column "t" that will have a sequence of numbers from 1 to total nuber of rows
bitcoin_cash_High["t"] <- 1:213

#Add Another Column that will have the square value of "t" column
bitcoin_cash_High["t_square"] <- bitcoin_cash_High$t * bitcoin_cash_High$t

#Lets Add another column that will have log values of "High" column
bitcoin_cash_High["log_High"] <- log(bitcoin_cash_High$High)

#bitcoin_cash_ts <- ts(bitcoin_cash_High, start =decimal_date(as.Date("2017-07-23")), frequency = 365.25)
#View(bitcoin_cash_ts)
#str(bitcoin_cash_ts)
#plot(bitcoin_cash_ts)

#Lets Split the Entire dataset into Train and Test Data
#indatapartition <- createDataPartition(bitcoin_cash_High$High, p=.75, list = FALSE)
train_high <- bitcoin_cash_High[1:149,]
test_high <- bitcoin_cash_High[150:213,]



############## Using HoltWinters Method to see the level, Trend And Seasonlity (Smoothing Parameters)#######
#Convert the Train and Test data to Time Series

amts <- ts(bitcoin_cash$High, frequency = 365.25, start = c(2017,07))
plot(amts)

train1 <- amts[1:149]
test1 <- amts[150:213]
plot(train1)
plot(test1)

#The Time series Object does not work well with Daily data so i will use zoo series
myzoo <- zoo(bitcoin_cash$High, seq(from = bitcoin_cash$Date[1], to = bitcoin_cash$Date[213], by = 1))
myzoo

#We have created a Zoo Series, now lets Divide the Data into Train and Test sets
train<- myzoo[1:149]
test <- myzoo[150:213]
#Lets Visualize and see
plot(train)
plot(test)

#Lets Build the Models
#HoltWinters function consists of Optimum Values like Alpha, Beta and Gama with Alpha = 0.2 as default value
#These Optimum Values hold certain Characteristics like Level, Trend And Seasonality. #Lets Create Multiple Models based on these Optimum Values.

# Assuming time series data has only level parameter
hw_a<-HoltWinters(train1,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=64))
plot(forecast(hw_a,h=64)) # By looking at the plot the forecasted values are not showing any characters of train data 
hwa_mape<-MAPE(hwa_pred$fit,test1)*100
hwa_mape

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train1,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 64))
plot(forecast(hw_ab,h=64)) # by looking at the plot the forecasted values are still missing some characters exhibited by train data
hwab_mape<-MAPE(hwab_pred$fit,test1)*100
hwab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
#hw_abg<-HoltWinters(train1,alpha = 0.2,beta = 0.1,gamma = 0.1)
#hw_abg
#hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 64))
# by looking at the plot the characters of forecasted values are closely following historical data
#plot(forecast(hw_abg,h=4))
#hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 64))
hwna_pred
plot(forecast(hw_na,h=64))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=64))
hwnab_pred
plot(forecast(hw_nab,h=64))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

#hw_nabg<-HoltWinters(train)
#hw_nabg
#hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
#hwnabg_pred
#plot(forecast(hw_nabg,h=4))
#hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwna_mape","hwnab_mape"),c(hwa_mape,hwab_mape,hwna_mape,hwnab_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)


########## ARIMA Model ############
model <- auto.arima(amts)
accuracy(model)
summary(model)
pred <- data.frame(forecast(model))
pred
plot(forecast(model, h = 10))

acf(model$residuals)
pacf(model$residuals)


################################Bitcoin
summary(bitcoin)
str(bitcoin)
#As we could see that the Volume as well as Market Cap variables are defined as Character While it should be a Numeric Variable
#Lets Convert them as Numeric Variable
bitcoin$Market.Cap <- as.numeric(gsub(",", "", bitcoin$Market.Cap))
bitcoin$Volume <- as.numeric(gsub(",", "", bitcoin$Volume))

boxplot(bitcoin[,-1])

#bitcoin_cash[bitcoin_cash==0] <- NA #To replace 0 values with NA.
#is.na(bitcoin_cash) #To check if there is any NA value in the dataset
#bitcoin_cash <- na.omit(bitcoin_cash) #To remove NA values

typeof(bitcoin$Date) #To check the Type of the Date Column
bitcoin$Date <- as.Date(bitcoin$Date, format = '%B %d, %Y') #To Convert the date Column 
bitcoin$Date
bitcoin <- bitcoin[order(bitcoin$Date),] #To sort the data with respect to date column


plot(bitcoin$Date,bitcoin$High, type = "l") #To Plot a Line chart
bitcoin_High <- bitcoin[,c(1,3)]

#Lets Add Dummy Variables for building multiple Models and comparing their RMSE values to Choose the Best Model for Prediction
#Lets Add a new column "t" that will have a sequence of numbers from 1 to total nuber of rows
bitcoin_High["t"] <- 1:1760

#Add Another Column that will have the square value of "t" column
bitcoin_High["t_square"] <- bitcoin_High$t * bitcoin_High$t

#Lets Add another column that will have log values of "High" column
bitcoin_High["log_High"] <- log(bitcoin_High$High)

#bitcoin_cash_ts <- ts(bitcoin_cash_High, start =decimal_date(as.Date("2017-07-23")), frequency = 365.25)
#View(bitcoin_cash_ts)
#str(bitcoin_cash_ts)
#plot(bitcoin_cash_ts)

#Lets Split the Entire dataset into Train and Test Data
#indatapartition <- createDataPartition(bitcoin_cash_High$High, p=.75, list = FALSE)
train_high <- bitcoin_High[1:1460,]
test_high <- bitcoin_High[1461:1760,]



############## Using HoltWinters Method to see the level, Trend And Seasonlity (Smoothing Parameters)#######
#Convert the Train and Test data to Time Series

amts <- ts(bitcoin$High, frequency = 365.25, start = c(2013,04,28))
plot(amts)

train1 <- amts[1:1460]
test1 <- amts[1461:1760]
plot(train1, type = "l")
plot(test1, type = "l")

#The Time series Object does not work well with Daily data so i will use zoo series
myzoo <- zoo(bitcoin$High, seq(from = bitcoin$Date[1], to = bitcoin$Date[1760], by = 1))
myzoo

#We have created a Zoo Series, now lets Divide the Data into Train and Test sets
train<- myzoo[1:1460]
test <- myzoo[1461:1760]
#Lets Visualize and see
plot(train)
plot(test)

#Lets Build the Models
#HoltWinters function consists of Optimum Values like Alpha, Beta and Gama with Alpha = 0.2 as default value
#These Optimum Values hold certain Characteristics like Level, Trend And Seasonality. #Lets Create Multiple Models based on these Optimum Values.

# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_a
hwa_pred<-data.frame(predict(hw_a,n.ahead=300))
plot(forecast(hw_a,h=300)) # By looking at the plot the forecasted values are not showing any characters of train data 
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape

# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)
hw_ab
hwab_pred<-data.frame(predict(hw_ab,n.ahead = 300))
plot(forecast(hw_ab,h=300)) # by looking at the plot the forecasted values are still missing some characters exhibited by train data
hwab_mape<-MAPE(hwab_pred$fit,test)*100
hwab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
#hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
#hw_abg
#hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 64))
# by looking at the plot the characters of forecasted values are closely following historical data
#plot(forecast(hw_abg,h=4))
#hwabg_mape<-MAPE(hwabg_pred$fit,test)*100

# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 300))
hwna_pred
plot(forecast(hw_na,h=300))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=300))
hwnab_pred
plot(forecast(hw_nab,h=300))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

#hw_nabg<-HoltWinters(train)
#hw_nabg
#hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =300))
#hwnabg_pred
#plot(forecast(hw_nabg,h=300))
#hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100

df_mape<-data.frame(c("hwa_mape","hwab_mape","hwna_mape","hwnab_mape"),c(hwa_mape,hwab_mape,hwna_mape,hwnab_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)


########## ARIMA Model ############
model <- auto.arima(amts)
accuracy(model)
summary(model)
pred <- data.frame(forecast(model))
pred$Point.Forecast
plot(forecast(model, h = 300))

acf(model$residuals)
pacf(model$residuals)





#Dash
summary(dash)
dash[dash==0] <- NA
bitcoin <- na.omit(bitcoin)

typeof(dash$Date)
dash$Date <- as.Date(dash$Date, format = '%b %d, %Y')
dash <- dash[order(dash$Date),]

plot(dash$Date, dash$Open, type = "l")

#Ethereum
summary(ethereum)
ethereum[ethereum==0] <- NA
ethereum <- na.omit(ethereum)

typeof(ethereum$Date)
ethereum$Date <- as.Date(ethereum$Date, format = '%b %d, %Y')
ethereum <- ethereum[order(ethereum$Date),]

plot(ethereum$Date,ethereum$Open, type = "l")

#Ethereum_classic
summary(ethereum_classic)
ethereum_classic[ethereum_classic==0] <- NA
ethereum_classic <- na.omit(ethereum_classic)

typeof(ethereum_classic$Date)
ethereum_classic$Date <- as.Date(ethereum_classic$Date, format = '%b %d, %Y')
ethereum_classic <- ethereum_classic[order(ethereum_classic$Date),]

plot(ethereum_classic$Date,ethereum_classic$Open, type = "l")

#Iota
summary(iota)
iota[iota==0] <- NA
iota <- na.omit(iota)

typeof(iota$Date)
iota$Date <- as.Date(iota$Date, format = '%b %d, %Y')
iota <- iota[order(iota$Date),]

plot(iota$Date,iota$Open, type = "l")

#Litecoin
summary(litecoin)
litecoin[litecoin==0] <- NA
litecoin <- na.omit(litecoin)

typeof(litecoin$Date)
litecoin$Date <- as.Date(litecoin$Date, format = '%b %d, %Y')
litecoin <- litecoin[order(litecoin$Date),]

plot(litecoin$Date,litecoin$Open, type = "l")

#Monero
summary(monero)
monero[monero==0] <- NA
monero <- na.omit(monero)

typeof(monero$Date)
monero$Date <- as.Date(monero$Date, format = '%b %d, %Y')
monero <- monero[order(monero$Date),]

plot(monero$Date,monero$Open, type = "l")

#Nem
summary(nem)
bitcoin[bitcoin==0] <- NA
bitcoin <- na.omit(bitcoin)

typeof(bitcoin$Date)
bitcoin$Date <- as.Date(bitcoin$Date, format = '%b %d, %Y')
bitcoin <- bitcoin[order(bitcoin$Date),]

plot(bitcoin$Date,bitcoin$Open, type = "l")

#neo
summary(neo)
neo[neo==0] <- NA
neo <- na.omit(neon)

typeof(neo$Date)
neo$Date <- as.Date(neo$Date, format = '%b %d, %Y')
neo <- neo[order(neo$Date),]

plot(neo$Date,neo$Open, type = "l")

#numeraire
summary(neo)
neo[neo==0] <- NA
neo <- na.omit(neo)

typeof(neo$Date)
neo$Date <- as.Date(neo$Date, format = '%b %d, %Y')
neo <- neo[order(neo$Date),]

plot(neo$Date,neo$Open, type = "l")

#Omisego
summary(omisego)
omisego[omisego==0] <- NA
omisego <- na.omit(omisego)

typeof(omisego$Date)
omisego$Date <- as.Date(omisego$Date, format = '%b %d, %Y')
omisego <- omisego[order(omisego$Date),]

plot(omisego$Date,omisego$Open, type = "l")

#Qtum
summary(qtum)
qtum[qtum==0] <- NA
qtum <- na.omit(qtum)

typeof(qtum$Date)
qtum$Date <- as.Date(qtum$Date, format = '%b %d, %Y')
qtum <- qtum[order(qtum$Date),]

plot(qtum$Date,qtum$Open, type = "l")

#Ripple
summary(ripple)
ripple[ripple==0] <- NA
ripple <- na.omit(ripple)

typeof(ripple$Date)
ripple$Date <- as.Date(ripple$Date, format = '%b %d, %Y')
ripple <- ripple[order(ripple$Date),]

plot(ripple$Date,ripple$Open, type = "l")

#Stratis
summary(ripple)
ripple[ripple==0] <- NA
ripple <- na.omit(ripple)

typeof(ripple$Date)
ripple$Date <- as.Date(ripple$Date, format = '%b %d, %Y')
ripple <- ripple[order(ripple$Date),]

plot(ripple$Date,ripple$Open, type = "l")

#Waves
summary(waves)
waves[waves==0] <- NA
waves <- na.omit(waves)

typeof(waves$Date)
waves$Date <- as.Date(waves$Date, format = '%b %d, %Y')
waves <- waves[order(waves$Date),]

plot(waves$Date,waves$Open, type = "l")



