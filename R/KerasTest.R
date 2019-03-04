rm(list=ls())
library(caret)
library(keras)
library(dplyr)
#source("modelFunctions.R")

traindata <- readRDS("AllData_Mon_Feb_04_12_50_06_2019.rds")

data_RW_train <- traindata$data_RW_train
data_RW_train <-(data_RW_train %>% mutate(colour = ifelse(colour == "white", 1, 0)))
data_RW_train <-(data_RW_train %>% mutate(quality = ifelse(quality == "good", 1, 0)))

training <- data_RW_train

data_RW_test <- traindata$data_RW_test
data_RW_test <-(data_RW_test %>% mutate(colour = ifelse(colour == "white", 1, 0)))
data_RW_test <-(data_RW_test %>% mutate(quality = ifelse(quality == "good", 1, 0)))
testing <-data_RW_test 

x_train <- training %>% dplyr::select( -quality)%>% as.matrix()
y_train <- to_categorical(training$quality, 2)

x_test <- testing %>% dplyr::select( -quality) %>% as.matrix()
y_test <- to_categorical(testing$quality, 2)


model <- keras_model_sequential() 

model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = ncol(x_train)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'sigmoid')

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

model %>% fit(
  x_train, y_train, 
  epochs = 50, 
  batch_size = 64,
  validation_split = 0.2
)


keraspred <- model %>% predict_classes(x_test)
confusionMatrix(as.factor(keraspred), as.factor(testing$quality))

