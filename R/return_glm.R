return_glm_beta_selected_models <- function(swag_summary) {
  
  # stop if not object summary.swag
  if (class(swag_summary) != "summary.swag") {
    stop("Please provide a summary.swag object")
  }
  
  # define max dim explored
  max_dim_explored <- length(swag_summary$model_select)
  
  # create matrix to store betas
  beta_models_df <- as.data.frame(matrix(nrow = swag_summary$n_models_selected, ncol = length(swag_summary$variable_name) + 1))
  colnames(beta_models_df) <- c("(Intercept)", swag_summary$variable_name)
  
  # fill up matrix
  model_index <- 1
  # iterate over all dimensions
  for (dim_i in seq(max_dim_explored)) {
    # if selected model in this dimension
    if (length(swag_summary$model_select[[dim_i]]) != 0) {
      
      # define nbr of model differently if only one or multiple selected models for that dimension
      if (is.vector(swag_summary$model_select[[dim_i]])) {
        nbr_model <- 1
      } else if (is.matrix(swag_summary$model_select[[dim_i]])) {
        nbr_model <- dim(swag_summary$model_select[[dim_i]])[2]
      }
      # iterate over selected model in this dimension
      for (model_i_dim_i in seq(nbr_model)) {
        
        # define selected variables differently if only one or multiple selected models for that dimension
        if (nbr_model == 1) {
          selected_variables <- swag_summary$model_select[[dim_i]]
        } else {
          selected_variables <- swag_summary$model_select[[dim_i]][, model_i_dim_i]
        }
        # get selected variables in the model
        x_mat <- as.matrix(swag_summary$x[, selected_variables])
        df_model_i <- as.data.frame(x_mat)
        df_model_i$y = swag_summary$y
        
        # fit
        fit <- glm(y ~ ., data = df_model_i, family = binomial)
        coeff_lm <- fit$coefficients
        
        # save fit coefficients
        beta_models_df[model_index, names(fit$coefficients)] <- coeff_lm
        
        # update model index
        model_index <- model_index + 1
      }
    }
  }
  
  # reorder based on table of model
  beta_models_df <- beta_models_df[, names(swag_summary$variable_table_prop)]
  
  out <- structure(list(
    beta_models_df = beta_models_df,
    swag_summary = swag_summary
  ))
  return(out)
}




# extract CV from best models
extract_cv_best_model = function(object,
                                 min_dim_method = "median",
                                 min_dim_min_cv_error_quantile = 0.01){
  
  object = train_swag_glm_sub
  min_dim_method = "min"
  min_dim_min_cv_error_quantile = 0.25
  
  # define CV and varmat
  CVs <- object$CVs
  VarMat <- object$VarMat
  
  # compute maximum dimension explored
  dmax <- length(object$CVs)
  dim_model <- seq_len(dmax)
  
  # find_dimension with lowest (min, mean, median)
  if (!min_dim_method %in% c("mean", "median", "min")) {
    stop("Please provided a supported method for selecting the minimum dimension for selecting models. \n Supported functions are min, median, mean")
  }
  
  if (min_dim_method == "min") {
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], min)))
  } else if (min_dim_method == "median") {
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], median)))
  } else if (min_dim_method == "mean") {
    mod_size_min <- which.min(unlist(lapply(CVs[1:dmax], mean)))
  }
  
  # get the quantile in  dimensions with the lowest (min, median, mean)
  quantile_value <- quantile(CVs[[mod_size_min]], min_dim_min_cv_error_quantile, na.rm = TRUE)
  
  # save all models in all dimensions for which the cv error is below the selected quantile
  df_cv_dim = data.frame("dimension" =NA, "cv" =NA, "best_model"=NA)
  index_model_select <- vector("list", dmax)
  for (i in dim_model) {

    # save in these dimensions
    index_model_select[[i]] <- which(((CVs[[dim_model[i]]] <= quantile_value)))
    # if(length(index_model_select[[i]] ) != 0){
    
      mat_cv_dim_i = cbind(i, CVs[[dim_model[i]]], 0)
      colnames(mat_cv_dim_i) = c("dimension", "cv", "best_model")
      mat_cv_dim_i[index_model_select[[i]], "best_model"] =T
      df_cv_dim = rbind(df_cv_dim, mat_cv_dim_i)
    # }
    
  }
  df_cv_dim = na.omit(df_cv_dim)
  df_cv_dim$best_model = as.factor(df_cv_dim$best_model)
  
  return(df_cv_dim)
  
}
