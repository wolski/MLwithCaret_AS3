# deployment 

make_should_I_drink_these_wines <- function(){
  prProc <- readRDS(prProc, file="preProcess_BoxCoxModel.rds")
  bestModel <- readRDS("results_models_Mon_Feb_04_12_50_06_2019/mod_fit_xgbTree_RW.rds")
  
  preprocessdata <- function(data_RW){
    data_RW$colour <- as.factor(ifelse(data_RW$colour==0,"white","red"))
    xx <- data_RW %>% select_if(is.numeric)
    bb <- as.matrix(xx)
    gg <- predict( prProc , bb)
    gg <- data.frame(gg)
    
    data_RW_t <- data.frame(colour = data_RW$colour,  gg)
    return( as_tibble(data_RW_t) )
  }
  
  predict_mm <- function(data_RW){
    xx <- preprocessdata(data_RW)
    bb <- predict(bestModel,xx)
    bb <- as.factor(ifelse(bb == "good" ,1, 0))
    return(bb)
  }
  return(predict_mm)
}



should_I_drink_these_wines<-make_should_I_drink_these_wines()
saveRDS(should_I_drink_these_wines, file = "should_I_drink_these_wines.rds")
