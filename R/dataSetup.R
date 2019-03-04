rm(list=ls())
library(tidyverse)
library(readr)
library(factoextra)
library(GGally)
library(caret)

data_RW <- read_csv("winetrain.csv") %>% dplyr::select(-X1)

data_RW$quality <- as.factor(ifelse(data_RW$quality==0,"poor","good"))
data_RW$colour <- as.factor(ifelse(data_RW$colour==0,"white","red"))

xx <- data_RW %>% select_if(is.numeric)
bb <- as.matrix(xx)
prProc <- preProcess( bb , method="BoxCox" )

saveRDS(prProc, file="preProcess_BoxCoxModel.rds")

gg <- predict( prProc , bb)
gg <- data.frame(gg)

data_RW_t <- data.frame(colour = data_RW$colour, quality = data_RW$quality, gg)
data_RW <- data_RW_t
data_RW <- as_tibble(data_RW)


data_W <- data_RW %>% filter(colour=="white") %>% select(-colour)
data_R <- data_RW %>% filter(colour=="red") %>% select(-colour)


trainIndex_RW <- createDataPartition(data_RW$quality , p = .8, 
                                     list = FALSE, 
                                     times = 1)
trainIndex_W <- createDataPartition(data_W$quality , p = .8, 
                                    list = FALSE, 
                                    times = 1)
trainIndex_R <- createDataPartition(data_R$quality , p = .8, 
                                    list = FALSE, 
                                    times = 1)

data_RW_train <- data_RW[trainIndex_RW,]
data_RW_test <- data_RW[-trainIndex_RW,]
data_W_train <- data_W[trainIndex_W,]
data_W_test <- data_W[-trainIndex_W,]
data_R_train <- data_R[trainIndex_R,]
data_R_test <- data_R[-trainIndex_R,]

reslist <- list(data_RW_train = data_RW_train, data_RW_test = data_RW_test, 
                data_W_train = data_W_train,
                data_W_test = data_W_test,
                data_R_train = data_R_train,
                data_R_test = data_R_test)
saveRDS(reslist, file=paste0("AllData_",gsub("[ :]", "_",date()) , ".rds"))
