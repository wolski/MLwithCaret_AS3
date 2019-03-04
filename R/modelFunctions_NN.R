run_train <- function(data, trControl, dataLabel = "RW", method = "xgbDART", tuneLength = tuneLength, path="."){
  modelFile <- file.path(path,paste0("mod_fit_",method,"_",dataLabel,".rds"))
  if(!file.exists(modelFile)){
    print(paste0("create:", modelFile))
    set.seed(1234)
    mod_fit <- caret::train(quality ~ .,  data=data, method=method, tuneLength = tuneLength, trControl = trControl)
    saveRDS(mod_fit, file=modelFile)
    return(mod_fit) 
  }
  return(NULL)
}


runModelsClassificationNN <- function(data_RW_train,data_W_train,data_R_train,ctrl,path, tuneLength = 3) {
  # Multi-Layer Perceptron ----
  
  if(0){ 
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