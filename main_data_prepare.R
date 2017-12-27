#Initialization*****************************************************************
rm(list = ls()) # clear environment
cat("\014") # clear console
#setwd("~/Dropbox/Research/Stock Prediction with News Sentiment/Ensemble Code")

#Load library
library(plyr)

start_date = "2013-01-01"
end_date = "2017-01-01"
date.from <- as.Date("01/01/2013", format = "%m/%d/%Y")
date.to   <- as.Date("01/01/2017", format = "%m/%d/%Y")
ticker = "FB"
ticker1 = 'FB'
index = "^IXIC"  #^NYA"  #DJI, NYA, IXIC, GSPC
search_term = "Facebook Inc"

source('func_dataAcquire.R')
df_market_withDate = get_stockMarket(start_date,end_date,ticker)
df_market = df_market_withDate$df_stockMarket
market_date = df_market_withDate$market_date
df_index = get_index(start_date,end_date,index)
df_indicator = get_indicator(df_market, market_date)
df_news =get_news(start_date, end_date, ticker1, market_date)
df_newsCount = df_news$df_newsCount
df_newsSentiment = df_news$df_newsSentiment
df_wiki <- article_pageviews(project = "en.wikipedia",
                                  article = "Facebook", platform = "all",
                                  user_type = "all", start = date.from, end = date.to, reformat = TRUE)
df_wiki = df_wiki[,7:8]  #keep only the date and traffic
colnames(df_wiki) = c("Date","Wiki")
#df_wiki = get_wiki(start_date, end_date, search_term,market_date)
df_gTrends = get_gTrends(start_date, end_date, search_term, market_date)


source('func_varsGenerate.R')
df_newsCount_new = generate_vars(df_newsCount)
df_gTrends_new = generate_vars(df_gTrends)
df_wiki_new = generate_vars(df_wiki)
df_target = generate_targets(df_market)
df_return = generate_returns_all(start_date,end_date, ticker, market_date)
df_wiki_new$Date <- as.Date (df_wiki_new$Date)

df_full = join_all(list(df_market,df_index,df_indicator,df_newsSentiment,
                        df_newsCount_new, df_gTrends_new, df_target,df_return), by = 'Date', type = 'full')

df_full <- merge(df_full,df_wiki_new,by="Date")


# df_full = join_all(list(df_market,df_index,df_indicator,
#                         df_gTrends_new,df_wiki_new,
#                         df_target,df_return), by = 'Date', type = 'full')
df_final = finalize_df(df_full)

Y = df_final[,which(names(df_final) %in% c('Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5', 'Lag6', 'Lag7', 'Lag8', 'Lag9', 'Lag10', 'dailyReturn', 'weeklyReturn', 'monthlyReturn'))]
X = df_final[,-c(2,which(names(df_final) %in% c('Lag1', 'Lag2', 'Lag3', 'Lag4', 'Lag5', 'Lag6', 'Lag7', 'Lag8', 'Lag9', 'Lag10', 'dailyReturn', 'weeklyReturn', 'monthlyReturn')))]


write.csv(df_final,'face_final_data.csv')


