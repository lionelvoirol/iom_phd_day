#############
# check swag_results
#############

# clean ws
rm(list=ls())

# load libraries
library(swag)
library(boot)
library(dplyr)
library(ggplot2)

# source functions
source("R/return_glm.R")

# load results from swag
load("data/train_swag_glm_no_int.rda")
load("data/df_telco_no_int.rda")
load("data/selected_model_aic.rda")

###########################################
# graph to show the cv error
###########################################

# select best models
summary_swag_glm_no_int = summary(train_swag_glm_sub,min_dim_method = "min", min_dim_min_cv_error_quantile = .25)

#return cv of selected models

dimension_cv = extract_cv_best_model(train_swag_glm_sub,min_dim_method = "min",
                                     min_dim_min_cv_error_quantile = .25)
dim_mdl_aic = length(coefficients(selected_model_aic))

cost = function(resp, pred){
  mean(resp == (pred > 0.5))
}
out_accuracy = cv.glm(df_telco_no_int, selected_model_aic, cost, K = 10)$delta[2]
error_aic_selected = 1- out_accuracy
plot(dimension_cv$dimension, dimension_cv$cv)
g1 = ggplot(dimension_cv, aes(x=dimension,y=cv, colour =  best_model))+
  geom_point(position = "jitter") + xlab("Model dimension")+
  scale_color_manual(values = c("#00008B80", "#FF8C0080")) +
  ylab("Cross-Validation Error") +
  theme_minimal(base_size = 20) + 
  geom_point(aes(x=dim_mdl_aic, y=error_aic_selected), 
             colour="red", size =5, alpha =.5)+
  annotate("segment", x = dim_mdl_aic-1, xend = dim_mdl_aic , 
           y = error_aic_selected+.01, yend = error_aic_selected, 
           colour = "red", size=2, alpha=0.5, arrow=arrow()) +
  annotate("text", x = dim_mdl_aic-1, y = error_aic_selected+.013,
           label = "stepwise AIC \n selected model") + 
  xlim(5,20)


plotly::ggplotly(g1)


# density plot error
dimension_cv_sub 
ggplot(dimension_cv) +
  aes(x = cv) +
  geom_density(adjust = 1L, fill = "#cae2f9") +
  theme_minimal(base_size = 22) + ylab("Density")+
  xlab("Cross-Validation Error") + geom_vline(xintercept =  error_aic_selected)+

  annotate("text", x = .212, y = 70,
           label = "Cross-Validation Error \n of AICselected model") +
  ggtitle("Cross Validation Error of all models")
  

# lolipop chart pvalue
load("data/selected_model_aic.rda")
df_1 = summary(selected_model_aic)
df_1$coefficients[,4]
x = names(df_1$coefficients[,4])
y = df_1$coefficients[,4]
df_pval = data.frame("pvalue" = y, "variable" = x)
df_pval = df_pval%>% arrange(pvalue)
par(mar=c(6,18,6,1))
plot(df_pval$pvalue,seq(length(df_pval$variable)),
     ylab = "",pch=16 ,xlab="p-value",
     main = "Variable significance",
     cex=1.5, yaxt='n', cex.axis=1.5, cex.lab = 2)
vec_name = df_pval$variable
vec_name_2 = str_replace(vec_name, pattern = "_", replacement = " ")
vec_name_3 = str_replace(vec_name_2, pattern = "_x_", replacement = " ")
vec_name_4 = str_replace(vec_name_3, pattern = "x_", replacement = " ")
vec_name_5 = str_replace(vec_name_4, pattern = "_", replacement = " ")
axis(side = 2, at = seq(length(df_pval$pvalue)), 
     labels = vec_name_5, las = 2, cex.axis = 1.2)
for(i in seq(length(vec_name_5))){
  segments(x0 = 0, y0 = i, x1 = df_pval[i, "pvalue"], y1 = i)
}
abline(v = .05)





# Plot
p <- ggplot(df_pval, aes(x=pvalue, y=variable)) +
  geom_segment(
    aes(x=x, xend=x, y=0, yend=y))
  ) +
  geom_point(
    color=ifelse(data$x %in% c("A","D"), "orange", "grey"), 
    size=ifelse(data$x %in% c("A","D"), 5, 2)
  ) +
  theme_ipsum() +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("Value of Y") +
  ggtitle("How did groups A and D perform?")

####
# graph polygon with error
####

# CVs = train_swag_glm_sub$CVs
# varmat = train_swag_glm_sub$VarMat
# 
# dmax=length(CVs)
# 
# # x=rnorm(10)
# # summary(x)
# m_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[4])
# l_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[1])
# u_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[6])
# 
# plot(1: length(m_vector), m_vector , type ="b", ylab = "Cross-validation Error",
#      xlab= "Model dimension", col = "blue4", ylim =c(0.19, 0.28), xlim=c(1, 20))
# polygon(x = c(1:length(m_vector), rev(1:length(m_vector))), 
#         y = c(l_vector, rev(u_vector)),  
#         border = NA, col =  "#FF8C0019")
# lines(x=1: length(m_vector), y = m_vector , col ="blue4")
# points(x=1: length(m_vector), y = m_vector , col ="blue4", pch =16)
# 
# # add points aic
# dim_mdl_aic = length(coefficients(selected_model_aic))
# library(boot)
# cost = function(resp, pred){
#   mean(resp == (pred > 0.5))
# }
# out_accuracy = cv.glm(df_telco_no_int, selected_model_aic, cost, K = 10)$delta[2]
# error_aic_selected = 1- out_accuracy
# 
# points(dim_mdl_aic, error_aic_selected, col ="black", pch = 15)
# # text(dim_mdl_aic-.3, y = error_aic_selected +.005, labels = "stepwise AIC selected model")
# legend("topright", pch = c(16,15), col = c("blue4", "black"), bty = "n",
#        legend = c("SWAG best model","stepwise AIC selected model"))
# 
# 






###########################################
# network graph
###########################################

# select best model
summary_swag_glm_no_int = summary(train_swag_glm_sub,min_dim_method = "min", min_dim_min_cv_error_quantile = .25)
estimated_models = return_glm_beta_selected_models(summary_swag_glm_no_int)

df_model = estimated_models$beta_models_df
df_1 = as.data.frame(t(combn(colnames(df_model), 2)))
colnames(df_1) = c("from", "to")
for(i_model in seq(dim(df_model)[1])){
  # i_model =2
  non_null_coef = colnames(df_model)[which(!is.na(df_model[i_model , ]))]
  combi_mdl_i  = cbind(t(combn(non_null_coef, 2)), 1)
  colnames(combi_mdl_i) = c("from", "to", paste("mdl", i_model, sep="_"))
  combi_mdl_i = as.data.frame(combi_mdl_i)
  df_1 = full_join(df_1, combi_mdl_i, by =c("from", "to"))
  
}


df_1[3:dim(df_1)[2]] <- lapply(df_1[3:dim(df_1)[2]], function(x) as.integer(x))
# count number of non na
df_1$sum_non_na_coef_in_best_mdl = rowSums(df_1[,3:dim(df_1)[2]], na.rm = T)

# select onyl sum and from to
colnames(df_1)
df_1 = df_1 %>% dplyr::select(from, to, sum_non_na_coef_in_best_mdl)
library(tidyr)
df_2 = pivot_wider(df_1, id_cols = from, names_from = to, values_from = sum_non_na_coef_in_best_mdl)
mat_1 = as.matrix(df_2)
rownames(mat_1) = mat_1[,1]
vec_name =  mat_1[,1]
mat_2 = mat_1[,-1]
mat_3 = apply(mat_2, 2, as.numeric)
vec_name_2 = str_replace(vec_name, pattern = "_", replacement = " ")
vec_name_3 = str_replace(vec_name_2, pattern = "_x_", replacement = " ")
vec_name_4 = str_replace(vec_name_3, pattern = "x_", replacement = " ")
vec_name_5 = str_replace(vec_name_4, pattern = "_", replacement = " ")
rownames(mat_3) = vec_name_5
colnames(mat_3) = vec_name_5
# dimnames(mat_3)[2] = vec_name_5
# fill matrix
# mat_3[lower.tri(mat_3)] = mat_3[upper.tri(mat_3)]


library(chorddiag)
load("data/mat_3")
chorddiag(mat_3, groupnamePadding = 10)

save(mat_3, file="data/mat_3.rda")
load("data/mat_3")
chorddiag(mat_3, groupnamePadding = 10)

# Libraries
# install.packages("ggraph")
library(ggraph)
library(igraph)

all_leaves <- paste("subgroup", seq(1,100), sep="_")
connect <- rbind( 
  data.frame( from=sample(all_leaves, 100, replace=T) , to=sample(all_leaves, 100, replace=T)), 
  data.frame( from=sample(head(all_leaves), 30, replace=T) , to=sample( tail(all_leaves), 30, replace=T)), 
  data.frame( from=sample(all_leaves[25:30], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)), 
  data.frame( from=sample(all_leaves[75:80], 30, replace=T) , to=sample( all_leaves[55:60], 30, replace=T)) 
)

# The connection object must refer to the ids of the leaves:
from <- match( connect$from, vertices$name)
to <- match( connect$to, vertices$name)
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices )
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.2, colour="skyblue", tension = 0) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
  theme_void()
