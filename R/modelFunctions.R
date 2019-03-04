run_train <- function(data, trControl, dataLabel = "RW", method = "xgbDART", tuneLength = tuneLength, metric = "ROC", path="."){
  modelFile <- file.path(path,paste0("mod_fit_",method,"_",dataLabel,".rds"))
  if(!file.exists(modelFile)){
    print(paste0("create:", modelFile))
    set.seed(1234)
    mod_fit <- caret::train(quality ~ .,  data=data, method=method, tuneLength = tuneLength, trControl = trControl , metric = metric)
    saveRDS(mod_fit, file=modelFile)
    return(mod_fit)
  }
  return(NULL)
}


runModelsClassification <- function(data_RW_train,data_W_train,data_R_train,ctrl,path, tuneLength = 5) {
  if(TRUE){
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "glm", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "glm", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "glm", tuneLength = tuneLength, path=path)
  
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "glmnet", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "glmnet", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "glmnet", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "lda", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "lda", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "lda", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "lda2", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "lda2", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "lda2", tuneLength = tuneLength, path=path)
  
  if(Sys.info()["nodename"] != "DESKTOP-45T6438"){
    mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "bagFDA", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "bagFDA", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "bagFDA", tuneLength = tuneLength, path=path)
  }
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "qda", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "qda", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "qda", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "rpart", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "rpart", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "rpart", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "ranger", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "ranger", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "ranger", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "C5.0", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "C5.0", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "C5.0", tuneLength = tuneLength, path=path)
  
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "rf", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "rf", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "rf", tuneLength = tuneLength, path=path)
  
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "gbm", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "gbm", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "gbm", tuneLength = tuneLength, path=path)
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "xgbLinear", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "xgbLinear", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "xgbLinear", tuneLength = tuneLength, path=path)

  # xgbDART ----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "xgbDART", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "xgbDART", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "xgbDART", tuneLength = tuneLength, path=path)
  
  # xgbTree ----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "xgbTree", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "xgbTree", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "xgbTree", tuneLength = tuneLength, path=path)
  
  # LogitBoost ----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "LogitBoost", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "LogitBoost", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "LogitBoost", tuneLength = tuneLength, path=path)
  
  # ADA -----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "ada", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "ada", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "ada", tuneLength = tuneLength, path=path)
  
  
  # SVM ----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "svmLinear", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "svmLinear", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "svmLinear", tuneLength = tuneLength, path=path)
  
  
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "svmRadial", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "svmRadial", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "svmRadial", tuneLength = tuneLength, path=path)
  # SVMRadialSigma ----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  
  
  # Model Averaged Neural Network ----
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "avNNet", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "avNNet", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "avNNet", tuneLength = tuneLength, path=path)

  # Multi-Layer Perceptron ----

  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlp", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlp", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlp", tuneLength = tuneLength, path=path)
  
  # Multi-Layer Perceptron, multiple layers ----
  
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  
  
  # Multi-Layer Perceptron, with multiple layers -----
  mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlpML", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlpML", tuneLength = tuneLength, path=path)
  #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlpML", tuneLength = tuneLength, path=path)
  }
  if(Sys.info()["nodename"] == "DESKTOP-45T6438"){
    mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)
    
    mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDropoutCost", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlpKerasDropoutCost", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlpKerasDropoutCost", tuneLength = tuneLength, path=path)
    
    mod_fig <- run_train(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_W_train, ctrl, dataLabel = "W", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
    #mod_fig <- run_train(data_R_train, ctrl, dataLabel = "R", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
  }
}