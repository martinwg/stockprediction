#_______________________________________________________________________________
#Load library
library(quantmod)
library(TTR)
library(Quandl)
library(pageviews)
library(lubridate)
library(wikipediatrend)
library(gtrendsR)

#_______________________________________________________________________________

#Get the stock market data
get_stockMarket = function(start_date, end_date, ticker){
     df_stockMarket = getSymbols(ticker, src = 'yahoo', from = start_date,
                                 to = end_date, auto.assign = FALSE) 
     market_date = as.data.frame(index(df_stockMarket))
     market_price = as.data.frame(df_stockMarket)
     df_stockMarket = cbind.data.frame(market_date,market_price,row.names=NULL)
     adjust_coff = df_stockMarket[,7]/df_stockMarket[,5]
     df_stockMarket[,c(2,3,4,5)] = apply(df_stockMarket[,c(2,3,4,5)],2,
                                         function(x) x*adjust_coff)
     df_stockMarket = df_stockMarket[,1:6]
     colnames(df_stockMarket) = c('Date','Open','High','Low','Close','Volume')
     colnames(market_date) = 'Date'
     return(list(df_stockMarket = df_stockMarket, market_date = market_date))
}
#_______________________________________________________________________________

#Get Index data
get_index = function(start_date,end_date,index){
     df_index = getSymbols(index, src = 'yahoo', from = start_date,
                           to = end_date, auto.assign = FALSE) 
     index_data = df_index[,4]
     date = as.data.frame(index(df_index))
     df_index = cbind.data.frame(date,index_data)
     colnames(df_index) = c('Date','Index')
     return(df_index)
}
#_______________________________________________________________________________

#Get Technical Indicators
get_indicator = function(df_market, market_date){
     stockOSC = data.frame(stoch(df_market[,c("High","Low","Close")]))
     fastK = stockOSC$fastK
     fastD = stockOSC$fastD
     slowD = stockOSC$slowD
     RSI = RSI(df_market$Close)
     CMO = CMO(df_market$Close)
     CCI = CCI(df_market[,c("High","Low","Close")])
     MACD = MACD(df_market$Close)[,1]
     MA5 = SMA(df_market$Close, 5)
     MA10 = SMA(df_market$Close, 10)
     ROC = ROC(df_market$Close)
     
     df_indicator = data.frame(Date = market_date,Market_fastK = fastK, 
                               Market_fastD = fastD, Market_slowD = slowD,
                               Market_RSI = RSI, Market_CMO = CMO, 
                               Market_CCI = CCI, Market_MACD = MACD, 
                               Market_MA5 = MA5, Market_MA10 = MA10, 
                               Market_ROC = ROC)
     return(df_indicator)
}
#_______________________________________________________________________________

#Get Financial news count and sentiment
get_news = function(start_date, end_date, ticker, market_date){
     code = paste0("NS1/",ticker,"_US")
     news_info = Quandl(code, api_key="9CRZsCcjtYCtjxoFs5-u")
     news_data = merge(market_date, news_info, by='Date', all.x=TRUE)
     newsCount = as.data.frame(news_data[,5])
     newsSentiment = as.data.frame(news_data[,2])
     # colnames(newsCount) = 'newsCount'
     # colnames(newsSentiment) = 'newsSentiment'
     df_newsCount = cbind.data.frame(market_date,newsCount)
     df_newsSentiment = cbind.data.frame(market_date,newsSentiment)
     colnames(df_newsCount) = c('Date','newsCount')
     colnames(df_newsSentiment) = c('Date','newsSentiment')
     return(list(df_newsCount = df_newsCount, df_newsSentiment = df_newsSentiment))
}
#_______________________________________________________________________________

#Get Wikipedia traffic
pageviewPKG = function(start_date, end_date, search_term){
     date.from_pv = format(as.Date(start_date),'%Y%m%d%H')
     date.to_pv = format(as.Date(end_date),'%Y%m%d%H')
     df.pageview = article_pageviews(project = "en.wikipedia",
                                     article = search_term,
                                     start = date.from_pv,end = date.to_pv,
                                     user_type = "user", platform = c("desktop"))
     df.wiki = data.frame(as.Date(df.pageview$date),df.pageview$views)
     colnames(df.wiki) = c("Date", "Wikitraffic")
     return(df.wiki)
}

wikitrendPKG = function(start_date, end_date, search_term){
     wiki.traffic = wp_trend(search_term, from = start_date, to = end_date,
                             lang = "en")
     trendpkg = data.frame(as.Date(wiki.traffic$date),wiki.traffic$count)  
     colnames(trendpkg) = c("Date","Wikitraffic")
     return(trendpkg)
}


get_wiki = function(start_date, end_date, search_term, market_date){
     if(year(end_date)<2015){
          df.wiki = wikitrendPKG(start_date, end_date, search_term)
     }else if(year(end_date) == 2015 & month(start_date) <= 7){
          df.wiki = wikitrendPKG(start_date, end_date, search_term)
     }else if(year(start_date)>= 2016){
          df.wiki = pageviewPKG(start_date, end_date, search_term)
     }else if(year(start_date)==2015 & month(start_date) >7){
          df.wiki = pageviewPKG(start_date, end_date, search_term)
     }else{
          date.break_1 = "2015-06-30"
          date.break_2 = "2015-07-01"
          df.wiki_1 = wikitrendPKG(start_date, date.break_1, search_term)
          df.wiki_2 = pageviewPKG(date.break_2, end_date, search_term)
          df.wiki = rbind(df.wiki_1,df.wiki_2)
     }
     wiki_data = merge(market_date, df.wiki, by='Date', all.x=TRUE)
     # wiki_data = as.data.frame(wiki_data[,2])
     # colnames(wiki_data) = 'wikiTraffic'
     return(wiki_data)
}
#_______________________________________________________________________________

#Get Google trends 
date.generator <- function(year,month){
     month = formatC(month,width = 2,flag = "0")
     year = as.character(year)
     month = as.character(month)
     date = paste0(year,'-',month,'-','01')
     return(date)
}

# get_gTrends = function(start_date, end_date, search_term, market_date){
#      user <- "au.stock.research@gmail.com"
#      psw <- "Auburn University"
#      gconnect(user, psw)
#      print("Connect sucessful.Warning: This is the free version 
#            GoogleTrends API, please allow 10 seconds systems break 
#            for each aquirison")
#      
#      year.start <- year(start_date)
#      year.end <- year(end_date)-1
#      years <- seq(from=year.start, to=year.end)
#      months <- c(1,4,7,10)
#      res.daily <- list()
#      counter <- 1
#      
#      for(year in years){
#           for(month in months){
#                start_date_daily = date.generator(year,month)
#                month_end = as.numeric(month)+3
#                if(month_end == 13){
#                     year = as.numeric(year)+1
#                     month_end = 1
#                }
#                end_date_daily = date.generator(year,month_end)
#                end_date_daily = as.character(as.Date(end_date_daily)-1)
#                res.daily[[counter]] = gtrends(search_term, geo = ("US"), 
#                                               start_date = start_date_daily, 
#                                               end_date = end_date_daily)$trend
#                counter = counter +1
#                print(paste(start_date_daily,end_date_daily, collapse = '>>>'))
#                Sys.sleep(10)  #Don't know the minimum Google allowed
#           }
#      }
#      
#      df.daily <- do.call("rbind", res.daily)
#      df.weekly <- gtrends(search_term, geo = ("US"), start_date = start_date, 
#                           end_date = end_date)$trend
#      df.merged <- merge(df.daily,df.weekly, by="start", all.x = T)
#      df.merged$adjustment_factor <- df.merged$hits.y/df.merged$hits.x
#      
#      for(i in 2:nrow(df.merged)){
#           if(!is.finite(df.merged$adjustment_factor[i])){
#                df.merged$adjustment_factor[i] = df.merged$adjustment_factor[i-1]
#           }
#      }
#      
#      nonNa.factor <- which(!is.na(df.merged$adjustment_factor))
#      last.Na <- min(nonNa.factor) -1
#      df.merged <- tail(df.merged, -last.Na)
#      df.merged$Daily <- df.merged$adjustment_factor*df.merged$hits.x
#      df.gTrends <- data.frame(df.merged$start,df.merged$Daily)
#      colnames(df.gTrends)  <- c("Date", "gTrend")
#      df.gTrends$Date = as.Date(df.gTrends$Date)
#      trends_data = merge(market_date, df.gTrends, by='Date', all.x=TRUE)
#      # trends_data = as.data.frame(trends_data[,2])
#      # colnames(trends_data) = 'gTrend'
#      return(trends_data)
# }
#_______________________________________________________________________________
#If install the gtrendsR package use installpackage, there is limitation
#for the same IP in 24hrs. So the developer of the package updated a new
#version api and install the package use 
#devtools::install_github('PMassicotte/gtrendsR', ref = 'new-api')
#The data structure is changed, and the function is rewrite to meet
#the new api

get_gTrends = function(start_date, end_date, search_term, market_date){
     #user <- "au.stock.research@gmail.com"
     #psw <- "Auburn University"
     #gconnect(user, psw)
     #print("Connect sucessful.Warning: This is the free version 
     #      GoogleTrends API, please allow 10 seconds systems break 
     #      for each aquirison")
     
     year.start <- year(start_date)
     year.end <- year(end_date)-1
     years <- seq(from=year.start, to=year.end)
     months <- c(1,4,7,10)
     res.daily <- list()
     counter <- 1
     
     for(year in years){
          for(month in months){
               start_date_daily = date.generator(year,month)
               month_end = as.numeric(month)+3
               if(month_end == 13){
                    year = as.numeric(year)+1
                    month_end = 1
               }
               end_date_daily = date.generator(year,month_end)
               end_date_daily = as.character(as.Date(end_date_daily)-1)
               time_daily = paste(start_date_daily,end_date_daily,sep = ' ')
               res.daily[[counter]] = gtrends(search_term, geo = ("US"), 
                                              time = time_daily
                                              )$interest_over_time
               counter = counter +1
               print(paste(start_date_daily,end_date_daily, collapse = '>>>'))
               #Sys.sleep(2)  #Don't know the minimum Google allowed
          }
     }
     
     df.daily <- do.call("rbind", res.daily)
     time_weekly = paste(start_date,end_date,sep = ' ')
     df.weekly <- gtrends(search_term, geo = ("US"), time = time_weekly
                          )$interest_over_time
     df.merged <- merge(df.daily,df.weekly, by="date", all.x = T)
     df.merged$adjustment_factor <- df.merged$hits.y/df.merged$hits.x
     
     for(i in 2:nrow(df.merged)){
          if(!is.finite(df.merged$adjustment_factor[i])){
               df.merged$adjustment_factor[i] = df.merged$adjustment_factor[i-1]
          }
     }
     
     nonNa.factor <- which(!is.na(df.merged$adjustment_factor))
     last.Na <- min(nonNa.factor) -1
     df.merged <- tail(df.merged, -last.Na)
     df.merged$Daily <- df.merged$adjustment_factor*df.merged$hits.x
     df.gTrends <- data.frame(df.merged$date,df.merged$Daily)
     colnames(df.gTrends)  <- c("Date", "gTrend")
     df.gTrends$Date = as.Date(df.gTrends$Date)
     trends_data = merge(market_date, df.gTrends, by='Date', all.x=TRUE)
     # trends_data = as.data.frame(trends_data[,2])
     # colnames(trends_data) = 'gTrend'
     return(trends_data)
}






