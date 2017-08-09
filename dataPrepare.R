#Initialization*****************************************************************
rm(list = ls()) # clear environment
cat("\014") # clear console
setwd("/Users/bin/Dropbox/My Research/Paper 3/Code")

#Download Stock Market data*****************************************************
library(quantmod)

date.from <- as.Date("01/01/2013", format = "%m/%d/%Y")
date.to   <- as.Date("02/01/2017", format = "%m/%d/%Y")
ticker <- "AAPL"

getSymbols(ticker, src = "yahoo", from = date.from, to = date.to) #Download data from Yahoo

date.market <- data.frame(index(AAPL))
price.market <- as.data.frame(AAPL)
data.market <- cbind.data.frame(date.market,price.market,row.names=NULL)#Change to dataframe

adjust.coff <- data.market[,7]/data.market[,5]#Adjust the price for spliting
data.market[,c(2,3,4,5)] <- apply(data.market[,c(2,3,4,5)], 2, 
                                  function(x) x*adjust.coff)
data.market <- data.market[,1:6]
colnames(data.market) <- c("Date","Open","High","Low","Close","Volume")

#Download Wikipedia Data********************************************************
library(wikipediatrend)
wiki.traffic <- wp_trend("Apple_Inc.", from = date.from, to = date.to,
                         lang = "en")

data.wiki = wiki.traffic[wiki.traffic$date %in% date.market[,1],] #Keep only the market date
data.wiki = data.wiki[,1:2]  #keep only the date and traffic
colnames(data.wiki) = c("Date","Wiki")

#Download News count and sentiment**********************************************
library(Quandl)
code = paste0("NS1/",ticker,"_US")
news.info <- Quandl(code, api_key="9CRZsCcjtYCtjxoFs5-u")
data.news <- news.info[news.info$Date %in% date.market[,1],]

#Merge three data sources to one dataframe**************************************
library(plyr)
#try1 = merge(data.market,data.wiki,by="Date")
#try2 <- merge(try1,news.info,by="Date")
data.three <- join_all(list(data.market,data.wiki,data.news),
                       by="Date", type = "full")

data.three[is.na(data.three)] <- 0

#Generate the new variable 1: Market data***************************************
library(TTR)

price.sma.10 <- SMA(data.three$Close,10)
price.wma.10 <- WMA(data.three$Close,10)
price.cmo.10 <- CMO(data.three$Close,10)
price.rsi <- RSI(data.three$Close)
price.macd <- MACD(data.three$Close)[,1]

price.osc <- data.frame(stoch(data.three[,c("High","Low","Close")]))
price.k <- price.osc$fastK
price.d <- price.osc$fastD
price.cad <- chaikinAD(data.three[,c("High","Low","Close")],data.three$Volume)

data.three$priceSMA <- price.sma.10
data.three$priceWMA <- price.wma.10
data.three$priceMomentum <- price.cmo.10
data.three$priceRSI <- price.rsi
data.three$priceMACD <- price.macd
data.three$priceK <- price.k
data.three$priceD <- price.d
data.three$priceAD <- price.cad

#Generate the new variable 2: Wikipedia data************************************

wiki.sma.10 <- SMA(data.three$Wiki,10)
wiki.wma.10 <- WMA(data.three$Wiki,10)
wiki.cmo.10 <- CMO(data.three$Wiki,10)
wiki.rsi <- RSI(data.three$Wiki)
wiki.macd <- MACD(data.three$Wiki)[,1]

data.three$wikiSMA <- wiki.sma.10
data.three$wikiWMA <- wiki.wma.10
data.three$wikiMomentum <- wiki.cmo.10
data.three$wikiRSI <- wiki.rsi
data.three$wikiMACD <- wiki.macd

#Generate the new variable 3: Market Sentiment**********************************
sentiment.sma.10 <- SMA(data.three$Sentiment ,10)
sentiment.wma.10 <- WMA(data.three$Sentiment,10)
sentiment.cmo.10 <- CMO(data.three$Sentiment,10)
sentiment.rsi <- RSI(data.three$Sentiment)
sentiment.macd <- MACD(data.three$Sentiment)[,1]

sentiment.osc <- data.frame(stoch(data.three[,c("Sentiment High",
                                                "Sentiment Low","Sentiment")]))
sentiment.k <- sentiment.osc$fastK
sentiment.d <- sentiment.osc$fastD
sentiment.cad <- chaikinAD(data.three[,c("Sentiment High","Sentiment Low",
                                         "Sentiment")],data.three$`News Volume`)

data.three$sentimentSMA <- sentiment.sma.10
data.three$sentimentWMA <- sentiment.wma.10
data.three$sentimentMomentum <- sentiment.cmo.10
data.three$sentimentRSI <- sentiment.rsi
data.three$sentimentMACD <- sentiment.macd
data.three$sentimentK <- sentiment.k
data.three$sentimentD <- sentiment.d
data.three$sentimentAD <- sentiment.cad

#Generate the new variable 4: Online News***************************************

news.sma.10 <- SMA(data.three$`News Volume`,10)
news.wma.10 <- WMA(data.three$`News Volume`,10)
news.cmo.10 <- CMO(data.three$`News Volume`,10)
news.rsi <- RSI(data.three$`News Volume`)
news.macd <- MACD(data.three$`News Volume`)[,1]

data.three$newsSMA <- news.sma.10
data.three$newsWMA <- news.wma.10
data.three$newsMomentum <- news.cmo.10
data.three$newsRSI <- news.rsi
data.three$newsMACD <- news.macd

#Create the target**************************************************************
period <- 1  #looking for one day ahead


close.diff <- diff(data.three$Close,lag = period)
rate.change <- close.diff/data.three$Close[1:length(close.diff)]*100

tClass.checker <- function(change){
     if(change <= -2){
          "C1"
     }else if (change > -2 & change <= -1){
          "C2"
     }else if (change > -1 & change <= 0){
          "C3"
     }else if (change > 0 & change <= 1){
          "C4"
     }else if (change > 1 & change <= 2){
          "C5"
     }else{
          "C6"
     }
}

target.class <- lapply(rate.change, tClass.checker)

target<- rep(NA, nrow(data.three))
target[1:length(target.class)] <- target.class
target <- as.factor(unlist(target))

data.three$Target <- target
data.three <- as.data.frame(data.three)

#Finalize the data prepare******************************************************
data.final <- data.three[complete.cases(data.three),]
data.final$Target <- as.factor(data.final$Target)

drops <- c("Date","Close")
data.final <- data.final[,!(names(data.final) %in% drops)]

#Export the data to CSV*********************************************************
#write.csv(data.final, file = "Paper3_data.csv")
