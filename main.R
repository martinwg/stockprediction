#Initialization*****************************************************************
rm(list = ls()) # clear environment
cat("\014") # clear console
setwd("/Users/bin/Desktop/Paper/Paper 3/Code")

#Load library
library(plyr)

start_date = "2013-01-01"
end_date = "2017-01-01"
ticker = "C"
index = "^NYA"  #DJI, NYA, IXIC, GSPC
search_term = "Citigroup"

source('func_dataAcquire.R')
df_market_withDate = get_stockMarket(start_date,end_date,ticker)
df_market = df_market_withDate$df_stockMarket
market_date = df_market_withDate$market_date
df_index = get_index(start_date,end_date,index)
df_indicator = get_indicator(df_market, market_date)
df_news =get_news(start_date, end_date, ticker, market_date)
df_newsCount = df_news$df_newsCount
df_newsSentiment = df_news$df_newsSentiment
df_wiki = get_wiki(start_date, end_date, search_term,market_date)
df_gTrends = get_gTrends(start_date, end_date, search_term, market_date)


source('func_varsGenerate.R')
df_newsCount_new = generate_vars(df_newsCount)
df_gTrends_new = generate_vars(df_gTrends)
df_wiki_new = generate_vars(df_wiki)
df_target = generate_targets(df_market)
df_return = generate_returns_all(start_date,end_date, ticker, market_date)

df_full = join_all(list(df_market,df_index,df_indicator,df_newsSentiment,
                        df_newsCount_new, df_gTrends_new,df_wiki_new,
                        df_target,df_return), by = 'Date', type = 'full')

# df_full = join_all(list(df_market,df_index,df_indicator,
#                         df_gTrends_new,df_wiki_new,
#                         df_target,df_return), by = 'Date', type = 'full')
df_final = finalize_df(df_full)
X = df_final[,2:42]
Y = df_final[,43:55]

# Target = Y[,1]
# df_try = cbind.data.frame(X, Target)



source('func_dimReduction.R')
correlation = getCorrelation(df_final)
target_index = 11
df_pca = dimReduction(X,Y,target_index)


source('func_predictModel.R')
boostTreeModel = boostTree(df_pca, control)
#svmModel = svmModel(df_pca,control)
#nnetModel = neuralNet(df_pca, control)



#########testing model##############
df_pca2 = df_pca
df_pca2$Target = log(df_pca2$Target)
plot(df_pca2$Target,type = 'l', col = 2)
modelnn = train(Target ~., df_try, method ='pcaNNet')

modelnn = train(Target~.+.^2, df_pca, method = 'bagEarth',trControl = control)
pp = predict(modelnn, df_pca)
plot(df_pca$Target, type = 'l')
lines(pp, col=2)

see = cbind.data.frame(df_pca$Target, pp)

see_result = function(model,testdata){
     pp = predict(model, testdata)
     plot(testdata[,length(testdata)],type = 'l')
     lines(pp,col = 2)
     see = cbind.data.frame(testdata[,length(testdata)], pp)
     return(see)
}

########step1##############
target = Y[,11]
try_data_return = cbind.data.frame(X,target)
targetP = Y[,1]
try_data_price = cbind.data.frame(X, targetP)
linear_model_return = lm(target ~., try_data_return)
linear_model_price = lm(target ~., try_data_price)

# result_return = see_result(linear_model_return, try_data_return)
# result_price = see_result(linear_model_price, try_data_price)

write.csv(df_final,'data.csv')


