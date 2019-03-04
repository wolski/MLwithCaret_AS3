rm(list=ls())
library(tidyverse)
library(readr)
library(factoextra)
library(GGally)
library(caret)

data_R <- read.csv("OriginalData/winequality-red.csv",sep=";")
data_W <- read.csv("OriginalData/winequality-white.csv", sep=";")

data_R$colour <- 1
data_W$colour <- 0
data_RW <- bind_rows(data_R, data_W)
data_RW <- as_tibble(data_RW)
data_RW

str(data_RW)

xx <- data_RW %>% select(-quality)
bb <- as.matrix(xx)
prProc <- preProcess( bb , method="BoxCox" )
gg <- predict( prProc , bb)
gg <- data.frame(gg)
data_RW_t <- data.frame( quality = data_RW$quality, gg)
data_RW <- data_RW_t

data_W <- data_RW %>% filter(colour==0) %>% select(-colour)
data_R <- data_RW %>% filter(colour==1) %>% select(-colour)

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
saveRDS(reslist, file="AllData_ORIG.rds")
