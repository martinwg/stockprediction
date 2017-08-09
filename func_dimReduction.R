library(corrplot)
library(caret)

getCorrelation = function(data_frame){
     #Get correlation matrix
     numeric_index = sapply(data_frame, is.numeric)
     df_num = data_frame[,numeric_index]
     correlations = cor(df_num[,1:(ncol(df_num)-10)])
     pdf('corPlot.pdf')
     corrplot(correlations,order = "hclust", tl.cex = 0.6, tl.col = 'black',
              tl.srt = 90, mar = c(0,0,0,0))
     dev.off
     # highCorr = findCorrelation(correlations,cutoff = .75)
     return(correlations)
}

dimReduction = function(X,Y,target_index){
     X = sapply(X,as.numeric)
     trans = preProcess(X, method=c('center', 'scale', 'pca'))
     transformed = predict(trans, X)
     Target = Y[,target_index]
     df_use = cbind.data.frame(transformed,Target)
     return(df_use)
}

featureSelection = function(X, Y, target_index,method = 'gbm', numVars = 10){
     X = sapply(X,as.numeric)
     Target = Y[,target_index]
     data = cbind.data.frame(X,Target)
     control <- trainControl(method="cv",number=10)
     # control = trainControl(method = 'timeslice', 
     #                        initialWindow = as.integer(0.8*nrow(X)),
     #                        horizon = as.integer(0.05*nrow(X)), 
     #                        fixedWindow = TRUE,
     #                        allowParallel = TRUE, savePredictions = TRUE)
     model = train(Target~., data = data, method = method, 
                   preProcess = 'scale', trControl = control)
     importance = varImp(model, scale=TRUE)
     
     imp_temp = as.data.frame(importance$importance)
     var_temp = as.data.frame(row.names(imp_temp))
     importance_new <- cbind(var_temp,imp_temp)
     row.names(importance_new) <- NULL
     
     order = rev(order(importance_new$Overall, importance_new$`row.names(imp_temp)`))
     importance_new$ranking = NA
     importance_new$ranking[order] = 1:nrow(importance_new)
     
     keep = which(importance_new$ranking <= numVars)
     features = as.character(importance_new$`row.names(imp_temp)`[keep])
     return(features)
}