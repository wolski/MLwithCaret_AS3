rm(list=ls())
library(caret)
source("modelFunctionsRegression.R")

path <- "results_models_regression"
dir.create(path)

traindata <- readRDS("AllData_ORIG.rds")

data_RW_train <- traindata$data_RW_train
data_W_train <- traindata$data_W_train
data_R_train <- traindata$data_R_train


ctrl <- trainControl(method = "repeatedcv",
                     number=10,
                     repeats = 3,
                     allowParallel = TRUE
)

runModelsRegression(data_RW_train,
                    data_W_train,
                    data_R_train,
                    ctrl,
                    path,
                    tuneLength = 5)
