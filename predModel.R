# X$newsSentiment = as.numeric(X$newsSentiment)
# 
# trans = preProcess(X, method=c('center', 'scale'))
# transformed = predict(trans, X)
# 
# 
# target = Y[,11]
# try_data_return = cbind.data.frame(transformed,target)
# targetP = Y[,5]
# try_data_price = cbind.data.frame(transformed, targetP)


# 
# linear_model_return = lm(target ~., try_data_return)
# linear_model_price = lm(targetP ~., try_data_price)
# 
# 
# predict_return = predict(linear_model_return, try_data_return)
# plot(try_data_return$target,type = 'l')
# lines(predict_return,col=2)
# 
# predict_price = predict(linear_model_price,try_data_price)
# plot(try_data_price$targetP, type = 'l')
# lines(predict_price,col=2)
# 
# 
# model_return = train(Target~., try_data_return, method = 'rf',trControl = control)
# pp = predict(model_return, try_data_return)
# plot(pp)
# 
# ########neural network############
# 
# nnModel = train(target~., data = try_data_return, method = 'neuralnet', trControl = control)
# 
# svrModel = train(train(df_train[,1:length(df_train)-1], df_train$Target,
#                        method = 'rvmPoly',
#                        trControl = control))





X$newsSentiment = as.numeric(X$newsSentiment)

trans = preProcess(X, method=c('center', 'scale'))
transformed = predict(trans, X)


target = Y[,11]
try_data_return = cbind.data.frame(target,transformed)
targetP = Y[,5]
try_data_price = cbind.data.frame(targetP,transformed)

########train model using full features############

nnModel = train(target~., data = try_data_return, method = 'nnet', trControl = control)

svrModel = train(target~., data = try_data_return, method = 'rvmRadial', trControl = control)

gbmModel = train(target~., data = try_data_return, method = 'gbm', trControl = control)

nnModel_price = train(targetP~., data = try_data_price, method = 'nnet', trControl = control)

svrModel_price = train(targetP~., data = try_data_price, method = 'rvmRadial', trControl = control)

gbmModel_price = train(targetP~., data = try_data_price, method = 'bstTree', trControl = control)

########train model with feature selection###################

features = featureSelection(transformed,Y,1, method = 'bstTree')
df_features = try_data_return[,which(colnames(try_data_return) %in% features)]
df_new = cbind.data.frame(targetP,df_features)

validate = as.integer(0.2*nrow(df_new))
df_train = head(df_new, -validate)
df_test = tail(df_new, validate)

library(nnet)
ff_nn = featureSelection(transformed,Y,2, method = 'nnet')
modelNN = nnet(df_new[,2:length(df_new)], df_new[,1], size =5, 
               linout = TRUE, maxit = 10000)
modelTest = predict(modelNN, df_new[,2:length(df_new)])
plot(modelTest, df_new$targetP)

nnModel = train(targetP~., data = df_new, method = 'mlpSGD', trControl = control)
nnRestuts = see_result(nnModel, df_new)

library(earth)
mm = earth(targetP~., data = df_train,degree = 6)
plot(mm$fitted.values, type = 'l')
lines(df_train$targetP)

#Tree
control <- trainControl(method="cv",number=5, savePredictions = TRUE)

treeModel_1 = train(target~., data = df_new, method = 'blackboost', trControl = control)
treeModel_2 = train(targetP~., data = df_new, method = 'bstTree', tuneGrid = expand.grid(.mstop=c(50,100,200),
                                                                                        .maxdepth = c(1,5,10),
                                                                                        .nu = c(0.1, 0.5, 0.01)),
                    trControl = control)  #selected
treeModel_3 = train(targetP~., data = df_new, method = 'gbm', trControl = control)

#SVM

svmModel_1 = train(targetP~., data = df_new, method = 'svmLinear2', trControl = control) #linear kernel
svmModel_2 = train(targetP~., data = df_new, method = 'svmRadial', trControl = control) #RBF
svmModel_3 = train(targetP~., data = df_new, method = 'rvmPoly', trControl = control)
svmModel_4 = train(targetP~., data = df_new, method = 'rvmRadial', trControl = control)









