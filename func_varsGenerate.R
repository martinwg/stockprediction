#_______________________________________________________________________________
#Name the column automaticly pVar
pVar = function(oriName,varName){
     name = paste0(varName,'_',oriName) 
     return(name)
}

generate_vars = function(data_frame){
     varName = colnames(data_frame)[2]
     Date = data_frame[,1]
     oriData = data_frame[,2]
     RSI = RSI(na.locf(data_frame[,2],na.rm = FALSE))
     CMO = CMO(na.locf(data_frame[,2],na.rm = FALSE))
     MACD = MACD(na.locf(data_frame[,2],na.rm = FALSE))[,1]
     MA5 = SMA(na.locf(data_frame[,2],na.rm = FALSE),5)
     MA10 = SMA(na.locf(data_frame[,2],na.rm = FALSE),10)
     ROC = ROC(na.locf(data_frame[,2],na.rm = FALSE))
     OSCP = MA5- MA5/MA10
     
     df_generate = data.frame(Date = Date, oriData = oriData, RSI = RSI, 
                              CMO = CMO, MACD = MACD,MA5 = MA5, MA10 = MA10,
                              ROC = ROC, OSCP = OSCP)
     colnames(df_generate) = c('Date', varName, pVar('RSI',varName), 
                               pVar('CMO',varName), pVar('MACD',varName), 
                               pVar('MA5',varName), pVar('MA10',varName),
                               pVar('ROC',varName),  pVar('OSCP',varName))
     return(df_generate)
}
#_______________________________________________________________________________

generate_targets = function(df_market){
     df_target = list()
     for(period in 1:10){
          close = tail(df_market$Close, -period)
          close[length(close) + period] = NA
          df_target[[period]] = close
     }
     date = df_market$Date
     df_target_all = cbind.data.frame(date,df_target)
     colnames(df_target_all) = c('Date', 'Lag1','Lag2','Lag3','Lag4','Lag5',
                             'Lag6','Lag7','Lag8','Lag9','Lag10')
     return(df_target_all)
}

generate_returns = function(start_date, end_date, ticker, market_date,period){
     if(period == 'daily'){
          end_date_new = as.Date(end_date) + 5
          df_stockMarket = getSymbols(ticker, src = 'yahoo', from = start_date,
                                      to = end_date_new, auto.assign = FALSE)
          date = as.data.frame(index(df_stockMarket))[,1]
          numRow = nrow(df_stockMarket)-1
          df_return = data.frame(matrix(ncol = 2, nrow = numRow))
          colnames(df_return) = c('Date', 'daily.returns')
          for(i in 1:numRow){
               df_return[i,1] = date[i]
               current = as.numeric(df_stockMarket[i,6])
               future = as.numeric(df_stockMarket[i+1,6])
               df_return[i,2] = ((future - current)/current)*100
          }
          df_return$Date = as.Date(df_return$Date)
          
     }else if(period == 'weekly'){
          end_date_new = as.Date(end_date) + 10
          df_stockMarket = getSymbols(ticker, src = 'yahoo', from = start_date,
                                      to = end_date_new, auto.assign = FALSE)
          date = as.data.frame(index(df_stockMarket))[,1]
          numRow = nrow(df_stockMarket)-5
          df_return = data.frame(matrix(ncol = 2, nrow = numRow))
          colnames(df_return) = c('Date', 'weekly.returns')
          for(i in 1:numRow){
               df_return[i,1] = date[i]
               current = as.numeric(df_stockMarket[i,6])
               future = as.numeric(df_stockMarket[i+5,6])
               df_return[i,2] = ((future - current)/current)*100
          }
          df_return$Date = as.Date(df_return$Date)
     }else if(period == 'monthly'){
          end_date_new = as.Date(end_date) + 50
          df_stockMarket = getSymbols(ticker, src = 'yahoo', from = start_date,
                                      to = end_date_new, auto.assign = FALSE)
          date = as.data.frame(index(df_stockMarket))[,1]
          numRow = nrow(df_stockMarket)-30
          df_return = data.frame(matrix(ncol = 2, nrow = numRow))
          colnames(df_return) = c('Date', 'monthly.returns')
          for(i in 1:numRow){
               df_return[i,1] = date[i]
               current = as.numeric(df_stockMarket[i,6])
               future = as.numeric(df_stockMarket[i+30,6])
               df_return[i,2] = ((future - current)/current)*100
          }
          df_return$Date = as.Date(df_return$Date)
     }
     
     df_return_done = merge(market_date, df_return, by = 'Date', all.x = TRUE)
     return(df_return_done)
}


generate_returns_all = function(start_date, end_date, ticker, market_date){
     periods = c('daily', 'weekly' , 'monthly')
     df_returns = data.frame(matrix(ncol = 4, nrow = nrow(market_date)))
     colnames(df_returns) = c("Date","dailyReturn","weeklyReturn","monthlyReturn")
     df_returns$Date = market_date$Date
     for(i in 1:3){
          period = periods[i]
          df_return = generate_returns(start_date,end_date,ticker,
                                       market_date,period)
          df_return = df_return[,!(names(df_return) %in% c('Date'))]
          df_returns[,i+1] = df_return
     }
     colnames(df_returns) = c("Date","dailyReturn","weeklyReturn","monthlyReturn")
     return(df_returns)
}

#_______________________________________________________________________________

finalize_df = function(df_full){
     #Remove NA
     df_noNA = df_full[complete.cases(df_full),]
     #Remvoe Inf
     for(i in 1:ncol(df_noNA)){
          df_noNA = df_noNA[is.finite(df_noNA[,i]),]
     }
     #Define the type of each column
     for(i in 1:ncol(df_noNA)){
          df_noNA[,i] = as.numeric(df_noNA[,i])
     } #Apply not work, don't know why
     df_noNA$Date = as.Date(df_noNA$Date)
     #df_noNA$newsSentiment = as.factor(df_noNA$newsSentiment)  
     return(df_noNA)
}












