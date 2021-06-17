###############
# IOM PhD days
# swag random forest
###############

# clean ws
rm(list=ls())

# load libraries
library(swag)
library(caret)
library(magrittr)

# load dataset
load("data/df_telco_final.rda")

# launch swag
y = df_telco_final$churn
x = df_telco_final %>% dplyr::select(-c(churn, customer_id))

# Training and test set
set.seed(180) # for replication
ind <- sample(1:dim(x)[1],dim(x)[1]*0.15)  
y_test <- y[ind]
y_train <- y[-ind]
x_test <- x[ind,]
x_train <-x[-ind,]

# Meta-parameters chosen for swag
swagcon_2 <- swagControl(pmax = 15L, 
                         alpha = 0.3, 
                         m = 20L,
                         seed = 163L, #for replicability
                         verbose = T #keeps track of completed dimensions
)

train_swag_rf <- swag(
  # arguments for swag
  x = x_train, 
  y = y_train, 
  control = swagcon_2,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "rf"
)

save(train_swag_rf, file = "train_swag_rf")
