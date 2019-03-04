library(tidyverse)
library(caret)
library(mlbench)


traindata <- readRDS("AllData_Mon_Feb_04_12_50_06_2019.rds")

path <- "results_models_Mon_Feb_04_12_50_06_2019/"

allModels <- dir(path)
rwModels <- grep("_RW.rds",allModels,value = TRUE )

models <- list()
confMatrix <- list()
byClass <- list()
overall <- list()
evalTime <- list()
predictions <- list()


for(file in rwModels){
  print(file)
  rfFit <- readRDS(file.path(path, file))
  start_time <- Sys.time()
  predtest <- predict(rfFit,dplyr::select(traindata$data_RW_test, -quality))
  end_time <- Sys.time()
  evaltime <- end_time - start_time
  dd <- confusionMatrix(predtest, traindata$data_RW_test$quality)
  models[[file]]<-rfFit
  predictions[[file]] <- predtest
  evalTime[[file]] <- evaltime
  confMatrix[[file]] <- dd
  byClass[[file]] <- dd$byClass
  overall[[file]] <- dd$overall
}



designpoints <- sapply(models, function(x){nrow(x$results)})
hyperparamters <- sapply(models, function(x){ncol(x$results)-7})

hyperparamters <- data.frame(ML = names(hyperparamters),
                              nr_hyperparameters = hyperparamters)
designpoints <- data.frame(ML = names(designpoints),nr_designpoints=designpoints)

bclass <- lapply( byClass, function(x){as.data.frame(t(x))} )


bclass <-data.frame( ML=names(bclass) , bind_rows(bclass) )

overall <- lapply(overall, function(x){as.data.frame(t(x))})
overall <-data.frame(ML=names(overall),bind_rows(overall))

evalTime <- data.frame(ML = names(evalTime), eval_time = unlist(evalTime))
modelEval <- inner_join(inner_join(evalTime, overall), bclass)

modelEval <- inner_join(modelEval, hyperparamters)

bb<-c(
  "mod_fit_ada_RW.rds",  "Boosted Classification Trees" ,"Boosting",
"mod_fit_avNNet_RW.rds", "Model Averaged Neural Network", "Bagging NN",
"mod_fit_gbm_RW.rds", "Stochastic Gradient Boosting", "Boosting",
"mod_fit_glm_RW.rds", "Generalized Linear Model", "GLM",
"mod_fit_glmnet_RW.rds", "Generalized Linear Model - Lasso", "GLM",
"mod_fit_lda_RW.rds", "Linear Discr. Analysis", "Discr. analysis",
"mod_fit_lda2_RW.rds", "Linear Discr. Analysis", "Discr. analysis",
"mod_fit_LogitBoost_RW.rds", "Boosted Logistic Regression", "Boosting",
"mod_fit_mlp_RW.rds", "Multi-Layer Perceptron", "NN",
"mod_fit_mlpKerasDecay_RW.rds", "Multil. Perc. Net. with Weight Decay", "NN",
"mod_fit_mlpKerasDropout_RW.rds", "Multil. Perc. Network with Dropout", "NN",
"mod_fit_mlpKerasDropoutCost_RW.rds", "Multil. Perc. Network with Dropout", "NN",
"mod_fit_mlpML_RW.rds", "Multil. Perc. Network", "NN",
"mod_fit_mlpWeightDecayML_RW.rds", "Multil. Perc. Network", "NN",
"mod_fit_qda_RW.rds", "Quadratic Discr. Analysis", "Discr. analysis",
"mod_fit_rf_RW.rds","Random Forest","Bagging",
"mod_fit_ranger_RW.rds","Random Forest","Bagging",
"mod_fit_rpart_RW.rds", "CART","Tree",
"mod_fit_svmLinear_RW.rds", "SVM","SVM",
"mod_fit_svmRadial_RW.rds", "SVM","SVM",
"mod_fit_svmRadialSigma_RW.rds", "SVM","SVM",
"mod_fit_xgbDART_RW.rds", "Boosted Trees","Boosting",
"mod_fit_xgbLinear_RW.rds","Boosted Trees","Boosting",
"mod_fit_xgbTree_RW.rds", "Boosted Trees","Boosting",
"mod_fit_bagFDA_RW.rds","Bagged Flexible Discr. Analysis","Discr. analysis",
"mod_fit_C5.0_RW.rds","Tree","Tree"
)



bb <- data.frame(matrix(bb, ncol=3, byrow = T))
colnames(bb) <- c("ML", "name", "category")

modelEval <- full_join(bb, modelEval)
modelEval$ML2 <- gsub("mod_fit_","",gsub("_RW.rds","",modelEval$ML))

modelSummary <- modelEval %>% select(ML2, name, category, nr_hyp = nr_hyperparameters, predict_time = eval_time, Kappa, Sensitivity, Specificity, Balanced.Accuracy)

write_csv(modelSummary,path = "modelSummary.txt")
View(modelSummary)

