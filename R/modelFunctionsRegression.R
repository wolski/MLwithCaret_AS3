run_train_reg <- function(data,
                          trControl,
                          dataLabel = "RW",
                          method = "xgbDART",
                          tuneLength = 5,
                          metric = "RMSE",
                          path="."){
  modelFile <- file.path(path,paste0("mod_fit_",method,"_",dataLabel,".rds"))
  if(!file.exists(modelFile)){
    mod_fit <- caret::train(quality ~ .,
                            data=data,
                            method=method,
                            tuneLength = tuneLength, 
                            trControl = trControl ,
                            metric = metric)
    saveRDS(mod_fit, file=modelFile)
    return(mod_fit) 
    
  }
  return(NULL)
}


runModelsRegression <- function(data_RW_train,data_W_train,data_R_train,ctrl,path, tuneLength = 5){
  
  message("lm")
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "lm", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "lm", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "lm", tuneLength = tuneLength, path=path)
  
  message("glmnet")
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "glmnet", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "glmnet", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "glmnet", tuneLength = tuneLength, path=path)
  
  
  message("rpart")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "rpart", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "rpart", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "rpart", tuneLength = tuneLength, path=path)

  message("rf")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "rf", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "rf", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "rf", tuneLength = tuneLength, path=path)
  
  message("gbm")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "gbm", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "gbm", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "gbm", tuneLength = tuneLength, path=path)
  
  message("xgbLinear")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "xgbLinear", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "xgbLinear", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "xgbLinear", tuneLength = tuneLength, path=path)
  
  message("xgbDART")
  
  # xgbDART ----
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "xgbDART", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "xgbDART", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "xgbDART", tuneLength = tuneLength, path=path)

  message("xgbTree")
  
  # xgbTree ----
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "xgbTree", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "xgbTree", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "xgbTree", tuneLength = tuneLength, path=path)

  
  message("SVM")
  
  # SVM ----
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "svmLinear", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "svmLinear", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "svmLinear", tuneLength = tuneLength, path=path)
  
  message("svmRadial")
  
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "svmRadial", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "svmRadial", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "svmRadial", tuneLength = tuneLength, path=path)
  # SVMRadialSigma ----
  message("svmRadialSigma")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "svmRadialSigma", tuneLength = tuneLength, path=path)
  
  
  # Model Averaged Neural Network ----
  
  message("avNNet")
  
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "avNNet", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "avNNet", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "avNNet", tuneLength = tuneLength, path=path)
  
  # Multi-Layer Perceptron ----
  message("mlp")
  
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlp", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "mlp", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "mlp", tuneLength = tuneLength, path=path)
  
  # Multi-Layer Perceptron, multiple layers ----
  message("mlpWeightDecayML")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "mlpWeightDecayML", tuneLength = tuneLength, path=path)
  
  message("mlpML")
  
  # Multi-Layer Perceptron, with multiple layers -----
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlpML", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "mlpML", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "mlpML", tuneLength = tuneLength, path=path)
  
  message("mlpKerasDropout")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "mlpKerasDropout", tuneLength = tuneLength, path=path)

  message("mlpKerasDecay")
  
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDecay", tuneLength = 5, path="")
  mod_fig <- run_train_reg(data_RW_train, ctrl, dataLabel = "RW", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_W_train, ctrl, dataLabel = "W", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
  mod_fig <- run_train_reg(data_R_train, ctrl, dataLabel = "R", method = "mlpKerasDecay", tuneLength = tuneLength, path=path)
  
}