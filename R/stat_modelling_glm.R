###############
# IOM PhD days
###############

# clean ws
rm(list=ls())

# load libraries
library(swag)
library(janitor)
library(caret)
library(dplyr)
library(ggplot2)
library(naniar)
library(cowplot)
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)

# load data
df_telco <- read_csv("data/customer_churn.csv")

# check NA
naniar::gg_miss_var(df_telco)

#remove observations with missing
df_telco <- df_telco[complete.cases(df_telco),]

# check summry
summary(df_telco)
colnames(df_telco)

# transform to factor
df_telco$SeniorCitizen <- as.factor(ifelse(df_telco$SeniorCitizen==1, 'YES', 'NO'))

# exploratory

# dependant variable
ggplot(df_telco, aes(y= Churn))+ geom_bar() 


# independant variables
options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(df_telco, aes(x=gender,fill=Churn))+ geom_bar(), 
          ggplot(df_telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(df_telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(df_telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(df_telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(df_telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")



options(repr.plot.width = 12, repr.plot.height = 8)
plot_grid(ggplot(df_telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(df_telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(df_telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(df_telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(df_telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(df_telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")



options(repr.plot.width =6, repr.plot.height = 2)
ggplot(df_telco, aes(y= tenure, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

ggplot(df_telco, aes(y= MonthlyCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")


ggplot(df_telco, aes(y= TotalCharges, x = "", fill = Churn)) + 
  geom_boxplot()+ 
  theme_bw()+
  xlab(" ")

# correlation between numeric
options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(df_telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

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
df_telco_3 = df_telco_2 %>% dplyr::select(-c(customerID, Churn))

# change the three numeric values to be recognized as such
df_telco_3$tenure = as.numeric(df_telco_3$tenure)
df_telco_3$MonthlyCharges = as.numeric(df_telco_3$MonthlyCharges)
df_telco_3$TotalCharges = as.numeric(df_telco_3$TotalCharges)



# create second interactions
df_telco_4 = model.matrix(~.^2, df_telco_3)
colnames(df_telco_4)

# rebind customer id and churn
df_telco_final = cbind(df_telco_2 %>% dplyr::select(customerID, Churn), 
                       df_telco_4[,-1])



# clean column name
colnames(df_telco_final)
df_telco_final = clean_names(df_telco_final)
colnames(df_telco_final)


# churn as factor
df_telco_final$churn = as.factor(df_telco_final$churn)
save(df_telco_final, file = "data/df_telco_final.rda")

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
swagcon <- swagControl(pmax = 15L, 
                       alpha = 0.3, 
                       m = 90L,
                       seed = 163L, #for replicability
                       verbose = T #keeps track of completed dimensions
)

train_swag_glm <- swag(
  # arguments for swag
  x = x_train, 
  y = y_train, 
  control = swagcon,
  auto_control = FALSE,
  # arguments for caret
  trControl = caret::trainControl(method = "repeatedcv", number = 10, repeats = 1, allowParallel = F),
  metric = "Accuracy",
  method = "glm",
  family = "binomial"
)

save(train_swag_glm, file ="data/train_swag_glm.rda")
