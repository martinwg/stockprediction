library(caret)
library(doMC)
registerDoMC(cores = 3)

control = trainControl(method = 'timeslice', initialWindow = 445,
                         horizon = 89, fixedWindow = TRUE,
                         allowParallel = TRUE, savePredictions = TRUE)


boostTree = function(df_train,trControl){
     set.seed(666)
     gbmGrid = expand.grid(.interaction.depth = seq(1,7,by=2),
                           .n.trees = seq(100,1000,by=50),
                           .shrinkage = c(0.01,0.1),
                           .n.minobsinnode = 10)
     gbmModel = train(Target~., data = df_train, method = 'bstTree',
                      tuneGrid = gbmGrid, verbose = FALSE,
                      trControl = control)
     return(gbmModel)
}


bagTree = function(df_train, trControl){
     set.seed(666)
     treebagModel = train(Target~., data = df_train, method = 'treebag',
                          trControl = control)
}

svmModel = function(df_train, trControl){
     set.seed(666)
     #svmGrid = expand.grid(.sigma )
     svmRTuned = train(df_train[,1:length(df_train)-1], df_train$Target,
                       method = 'rvmPoly',
                       trControl = control)
     return(svmRTuned)
}

neuralNet = function(df_train, trControl){
     nnetModel = train(Target~., data=df_train, method = 'nnet',
                       linout = TRUE, trControl = control)
     return(nnetModel)
}