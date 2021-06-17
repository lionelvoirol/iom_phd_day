#############
# check swag_results
#############

# clean ws
rm(list=ls())

# load libraries
library(swag)

# load results from swag
load("~/github_repo/iom_phd_day/data/train_swag_glm_no_int.rda")
load("data/df_telco_no_int.rda")
load("data/selected_model_aic.rda")



###########################################
# graph to show the cv error
###########################################

# select best models
summary_swag_glm_no_int = summary(train_swag_glm_sub,min_dim_method = "min", min_dim_min_cv_error_quantile = .25)


# extract CV from best models




#return beta models
source("R/return_glm.R")
summary_swag_glm_no_int$n_models_selected
estimated_beta_glm = return_glm_beta_selected_models(summary_swag_glm_no_int)


CVs = train_swag_glm_sub$CVs
varmat = train_swag_glm_sub$VarMat

dmax=length(CVs)

# x=rnorm(10)
# summary(x)
m_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[4])
l_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[1])
u_vector <- sapply(CVs[c(1:dmax)], function(x) summary(x)[6])

plot(1: length(m_vector), m_vector , type ="b", ylab = "Cross-validation Error",
     xlab= "Model dimension", col = "blue4", ylim =c(0.19, 0.28), xlim=c(1, 20))
polygon(x = c(1:length(m_vector), rev(1:length(m_vector))), 
        y = c(l_vector, rev(u_vector)),  
        border = NA, col =  "#FF8C0019")
lines(x=1: length(m_vector), y = m_vector , col ="blue4")
points(x=1: length(m_vector), y = m_vector , col ="blue4", pch =16)

# add points aic
dim_mdl_aic = length(coefficients(selected_model_aic))
library(boot)
cost = function(resp, pred){
  mean(resp == (pred > 0.5))
}
out_accuracy = cv.glm(df_telco_no_int, selected_model_aic, cost, K = 10)$delta[2]
error_aic_selected = 1- out_accuracy

points(dim_mdl_aic, error_aic_selected, col ="black", pch = 15)
# text(dim_mdl_aic-.3, y = error_aic_selected +.005, labels = "stepwise AIC selected model")
legend("topright", pch = c(16,15), col = c("blue4", "black"), bty = "n",
       legend = c("SWAG best model","stepwise AIC selected model"))








###########################################
# network graph
###########################################

# select best model




m <- matrix(c(11975,  5871, 8916, 2868,
              1951, 10048, 2060, 6171,
              8010, 16145, 8090, 8045,
              1013,   990,  940, 6907),
            byrow = TRUE,
            nrow = 4, ncol = 4)
haircolors <- c("black", "blonde", "brown", "red")
dimnames(m) <- list(have = haircolors,
                    prefer = haircolors)

library(chorddiag)
groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")
chorddiag(m, groupColors = groupColors, groupnamePadding = 20)

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
