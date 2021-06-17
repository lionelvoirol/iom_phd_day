#######################
# check results swag
#######################

rm(list=ls())

library(MASS)

# load telco
df_telco <- read_csv("data/customer_churn.csv")

#remove observations with missing
df_telco <- df_telco[complete.cases(df_telco),]

# transform to factor
df_telco$SeniorCitizen <- as.factor(ifelse(df_telco$SeniorCitizen==1, 'YES', 'NO'))
# change no phone and no internet to NO
df_telco <- data.frame(lapply(df_telco, function(x) {
  gsub("No internet service", "No", x)}))

df_telco <- data.frame(lapply(df_telco, function(x) {
  gsub("No phone service", "No", x)}))

# extract categorical variables
library(dplyr)
df_telco_cat = df_telco %>% dplyr::select(-c(customerID, tenure, MonthlyCharges, TotalCharges))
dummy<- data.frame(sapply(df_telco_cat,function(x) data.frame(model.matrix(~x-1,data =df_telco_cat))[,-1]))

# bind with numeric values
df_telco_2 = cbind(df_telco %>% dplyr::select(customerID, tenure, MonthlyCharges, TotalCharges), 
                   dummy)
df_telco_3 = df_telco_2

# change the three numeric values to be recognized as such
df_telco_3$tenure = as.numeric(df_telco_3$tenure)
df_telco_3$MonthlyCharges = as.numeric(df_telco_3$MonthlyCharges)
df_telco_3$TotalCharges = as.numeric(df_telco_3$TotalCharges)
df_telco_3 = janitor::clean_names(df_telco_3)

null_model = glm(churn~1, data = df_telco_3 %>% dplyr::select(-customer_id), family = "binomial")
full_model = glm(churn~., data = df_telco_3 %>% dplyr::select(-customer_id), family = "binomial")
selected_model = step(null_model, scope = list(lower = formula(null_model),
                                                         upper = formula(full_model)), 
                                            direction = "forward", trace = FALSE)


length(selected_model$coefficients)     
summary(selected_model)

library(boot)
cost = function(resp, pred){
  mean(resp == (pred > 0.5))
}
out_accuracy = cv.glm(df_telco_3, selected_model, cost, K = 10)$delta[2]
out_accuracy





# Meta-parameters chosen for swag
swagcon <- swagControl(pmax = 15L, 
                       alpha = 0.3, 
                       m = 90L,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
)

df_telco_3 = na.omit(df_telco_3)
x = df_telco_3 %>% dplyr::select(-c(customer_id, churn))
y = df_telco_3 %>% dplyr::select(churn)
y = as.factor(y)

library(swag)
train_swag_glm_sub <- swag(
  # arguments for swag
  x = x, 
  y = y, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "glm",
  family = "binomial"
)










load("data/df_telco_final.rda")
load("data/train_swag_glm.rda")

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

# load swag results
table(y_train) / length(y_train)

# check results
train_swag_glm$CVs

