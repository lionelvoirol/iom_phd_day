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
# load("data/df_telco_final.rda")
load("df_telco_final.rda")

# launch swag
y = df_telco_final$churn
x = df_telco_final %>% dplyr::select(-c(churn, customer_id))

# Meta-parameters chosen for swag
swagcon_2 <- swagControl(pmax = 20L, 
                         alpha = 0.3, 
                         m = 200L,
                         seed = 163L, #for replicability
                         verbose = T #keeps track of completed dimensions
)

train_swag_rf <- swag(
  # arguments for swag
  x = x, 
  y = y, 
  control = swagcon_2,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 5, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "rf"
)

save(train_swag_rf, file = "train_swag_rf.rda")
