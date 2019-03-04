rm(list=ls())
library(caret)
source("modelFunctions.R")
#source("modelFunctions_NN.R")

traindata <- readRDS("AllData_Mon_Feb_04_12_50_06_2019.rds")

path <- "results_models_Mon_Feb_04_12_50_06_2019"
dir.create(path)


data_RW_train <- traindata$data_RW_train
data_W_train <- traindata$data_W_train
data_R_train <- traindata$data_R_train


ctrl <- trainControl(method = "repeatedcv",
                     number=10,
                     repeats = 3,
                     allowParallel = TRUE,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = twoClassSummary
)

runModelsClassification(data_RW_train,
                          data_W_train,
                          data_R_train,
                          ctrl,
                          path,
                          tuneLength = 5)

