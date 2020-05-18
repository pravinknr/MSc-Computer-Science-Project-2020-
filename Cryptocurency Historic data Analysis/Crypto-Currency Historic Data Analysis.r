#Crypto-Currency Historic Data Analysis

library(forecast)
library(tseries)
library(lubridate)
library(factoextra)
library(zoo)
library(caret)

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

boxplot(bitcoin_cash)

bitcoin_cash[bitcoin_cash==0] <- NA #To replace 0 values with NA.
is.na(bitcoin_cash) #To check if there is any NA value in the dataset
bitcoin_cash <- na.omit(bitcoin_cash) #To remove NA values

typeof(bitcoin_cash$Date) #To check the Type of the Date Column
bitcoin_cash$Date <- as.Date(bitcoin_cash$Date, format = '%B %d, %Y') #To Convert the date Column 
bitcoin_cash$Date
bitcoin_cash <- bitcoin_cash[order(bitcoin_cash$Date),] #To sort the data with respect to date column



plot(bitcoin_cash$Date,bitcoin_cash$High, type = "l") #To Plot a Line chart
bitcoin_cash_High <- bitcoin_cash[,c(1,3)]

#bitcoin_cash_ts <- ts(bitcoin_cash_High, start =decimal_date(as.Date("2017-07-23")), frequency = 365.25)
#View(bitcoin_cash_ts)
#str(bitcoin_cash_ts)
#plot(bitcoin_cash_ts)

#Lets Split the Entire dataset into Train and Test Data
indatapartition <- createDataPartition(bitcoin_cash_High$High, p=.75, list = FALSE)
train_high <- bitcoin_cash_High[indatapartition,]
test_high <- bitcoin_cash_High[-indatapartition,]

#Convert the Train and Test data to Time Series
#train<- ts(train_high, frequency = 365.25)
#test <- ts(test_high, frequency = 365.25)

plot(train_high, type = "l")
plot(test_high, type = "l")

model <- auto.arima(train)
accuracy(model)
summary(model)
pred <- data.frame(forecast(test))
pred





#Bitcoin
summary(bitcoin)
bitcoin[bitcoin==0] <- NA
bitcoin <- na.omit(bitcoin)

typeof(bitcoin$Date)
bitcoin$Date <- as.Date(bitcoin$Date, format = '%b %d, %Y')
bitcoin <- bitcoin[order(bitcoin$Date),]

plot(bitcoin$Date,bitcoin$Open, type = "l")




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



