library(tidymodels)
library(DALEXtra)
library(forcats)
{
  ####This code sets up a variety of regression models with parameters that can be tuned during the model tuning process. 
  #The `tune()` function is a placeholder for actual parameter values or a strategy for selecting parameter values, such as using cross-validation. 
  #Each model is associated with a specific engine that provides the underlying implementation for the model. 
  #The `set_mode("regression")` call specifies that the models are intended for regression tasks.
  
#Define an XGBoost model with tunable parameters for tree depth, learning rate, loss reduction, minimum node size, and sample size.
xgb_mod <- boost_tree(
  tree_depth = tune(),
  learn_rate = tune(), 
  loss_reduction = tune(), 
  min_n = tune(), 
  sample_size = tune(),
  trees = 200L
) %>%
  set_engine("xgboost") %>%          # Set the engine to 'xgboost'
  set_mode("regression")            # Set the model mode to 'regression'

# Define a decision tree model with tunable parameters for cost complexity and minimum node size.
dt_mod <- decision_tree(
  cost_complexity = tune(), 
  min_n = tune()
) %>%
  set_engine("rpart") %>%           # Set the engine to 'rpart'
  set_mode("regression")            # Set the model mode to 'regression'

# Commented out logistic regression model as it's not being used in this snippet.

# Define a linear regression model using the 'glmnet' engine with tunable penalty and mixture parameters.
glmn_mod <- 
  linear_reg(
    penalty = tune(), 
    mixture = tune()
  ) %>%
  set_engine("glmnet") %>%           # Set the engine to 'glmnet'
  
  # Define a neural network model using the 'nnet' engine with tunable hidden units, penalty, and epochs.
  nnet_mod <- 
  mlp(
    hidden_units = tune(), 
    penalty = tune(), 
    epochs = tune()
  ) %>%
  set_engine('nnet') %>%            # Set the engine to 'nnet'
  set_mode('regression')            # Set the model mode to 'regression'

# Commented out naive Bayes model as it's not being used in this snippet.

# Define a k-nearest neighbors (KNN) model with tunable parameters for neighbors, distance power, and weight function.
kknn_mod <- 
  nearest_neighbor(
    neighbors = tune(), 
    dist_power = tune(), 
    weight_func = tune()
  ) %>%
  set_engine('kknn') %>%           # Set the engine to 'kknn'
  set_mode('regression')            # Set the model mode to 'regression'

# Define a random forest model with tunable parameters for the number of trees, minimum node size, and number of predictors per split.
rf_mod <- 
  rand_forest(
    mtry = tune(), 
    min_n = tune(), 
    trees = 200L
  ) %>%
  set_engine('ranger', importance = "impurity") %>%  # Set the engine to 'ranger' with 'impurity' for variable importance
  set_mode('regression')                                # Set the model mode to 'regression'

# Define a support vector machine (SVM) model using the 'kernlab' engine with tunable cost and RBF sigma parameters.
svm_mod <-
  svm_rbf(
    cost = tune(), 
    rbf_sigma = tune()
  ) %>%
  set_engine('kernlab') %>%        # Set the engine to 'kernlab'
  set_mode('regression')            # Set the model mode to 'regression'

# Load the 'plsmod' library for partial least squares (PLS) models.
library(plsmod)

# Define a PLS model with a tunable number of components using the 'mixOmics' engine.
pls_mod <-
  pls(
    num_comp = tune()
  ) %>%
  set_engine('mixOmics') %>%        # Set the engine to 'mixOmics'
  set_mode('regression')            # Set the model mode to 'regression'

# Commented out an alternative PLS model definition as it's not being used in this snippet.

# Define a lasso regression model using the 'glmnet' engine with a fixed mixture parameter of 1 (indicating L1 penalty).
lasso_mod <- 
  linear_reg(
    penalty = tune(), 
    mixture = 1
  ) %>%
  set_engine("glmnet") %>%          # Set the engine to 'glmnet'
  set_mode("regression")            # Set the model mode to 'regression'

# Load the 'bonsai' library for lightGBM models.
library(bonsai)

# Define a lightGBM model with tunable parameters for the number of trees, minimum node size, and tree depth.
lgb_mod <- boost_tree(
  trees = tune(),
  min_n = tune(),
  tree_depth = tune()
) %>%
  set_engine("lightgbm") %>%         # Set the engine to 'lightgbm'
  set_mode("regression")            # Set the model mode to 'regression'
}


{
  ##This code defines models for classification using various algorithms, including XGBoost, decision trees, logistic regression, neural networks, naive Bayes, KNN, random forest, SVM, PLS, lasso, and lightGBM. 
  #Each model has its parameters set up for tuning, which is a process to find the best hyperparameters for model performance. 
  #The tune() function is a placeholder that would be replaced with actual values or a tuning method (like grid search or random search) during the model tuning process. 
  #The set_engine() function specifies the underlying implementation to be used for each model.
  
  # XGBoost model for classification with tunable parameters
  xgb_mod <- boost_tree(
    tree_depth = tune(),      # Tunable tree depth
    learn_rate = tune(),      # Tunable learning rate
    loss_reduction = tune(),  # Tunable loss reduction parameter
    min_n = tune(),           # Tunable minimum node size
    sample_size = tune(),     # Tunable sample size
    trees = 200L             # Fixed number of trees
  ) %>%
    set_engine("xgboost") %>%  # Set the engine to 'xgboost'
    set_mode("classification") # Set the model mode to 'classification'
  
  # Decision tree model for classification with tunable parameters
  dt_mod <- decision_tree(
    cost_complexity = tune(), # Tunable cost complexity
    min_n = tune()            # Tunable minimum node size
  ) %>%
    set_engine("rpart") %>%    # Set the engine to 'rpart'
    set_mode("classification") # Set the model mode to 'classification'
  
  # Logistic regression model with tunable penalty and mixture parameters
  logistic_mod <-
    logistic_reg(
      penalty = tune(), # Tunable penalty for regularization
      mixture = tune()  # Tunable mixture parameter for elastic-net
    ) %>%
    set_engine('glmnet') # Set the engine to 'glmnet'
  
  # Neural network (multi-layer perceptron) model for classification
  nnet_mod <- 
    mlp(
      hidden_units = tune(), # Tunable number of hidden units
      penalty = tune(),      # Tunable penalty for regularization
      epochs = tune()        # Tunable number of training epochs
    ) %>%
    set_engine('nnet') %>%    # Set the engine to 'nnet'
    set_mode('classification') # Set the model mode to 'classification'
  
  # Naive Bayes model for classification
  naivebayes_mod <-
    naive_Bayes() %>%        # Naive Bayes model
    set_engine('naivebayes') # Set the engine to 'naivebayes'
  
  # k-Nearest Neighbors (KNN) model for classification with tunable parameters
  kknn_mod <- 
    nearest_neighbor(
      neighbors = tune(),    # Tunable number of neighbors
      dist_power = tune(),   # Tunable distance power
      weight_func = tune()   # Tunable weight function
    ) %>%
    set_engine('kknn') %>%   # Set the engine to 'kknn'
    set_mode('classification') # Set the model mode to 'classification'
  
  # Random forest model for classification with tunable parameters
  rf_mod <- 
    rand_forest(
      mtry = tune(),         # Tunable number of predictors for each split
      min_n = tune(),       # Tunable minimum node size
      trees = 1000          # Fixed number of trees
    ) %>%
    set_engine('ranger', importance = "impurity") %>% # Set the engine to 'ranger' with impurity importance
    set_mode('classification')                         # Set the model mode to 'classification'
  
  # Support Vector Machine (SVM) with Radial Basis Function (RBF) kernel for classification
  svm_mod <-
    svm_rbf(
      cost = tune(),        # Tunable cost parameter
      rbf_sigma = tune()    # Tunable RBF kernel sigma
    ) %>%
    set_engine('kernlab') %>% # Set the engine to 'kernlab'
    set_mode('classification') # Set the model mode to 'classification'
  
  # Partial Least Squares (PLS) model for classification with tunable number of components
  library(plsmod) # Load the 'plsmod' library for PLS models
  pls_mod <-
    pls(
      num_comp = tune()     # Tunable number of components
    ) %>%
    set_engine('mixOmics') %>% # Set the engine to 'mixOmics'
    set_mode('classification') # Set the model mode to 'classification'
  
  # Lasso regression model (L1 penalty) for classification
  lasso_mod <- 
    logistic_reg(
      penalty = tune(), # Tunable penalty for lasso (L1 regularization)
      mixture = 1       # Fixed mixture parameter for lasso
    ) %>%
    set_engine("glmnet") %>% # Set the engine to 'glmnet'
    set_mode("classification") # Set the model mode to 'classification'
  
  # LightGBM model for classification with tunable parameters
  library(bonsai) # Load the 'bonsai' library for lightGBM models
  lgb_mod <- boost_tree(
    trees = tune(),       # Tunable number of trees
    min_n = tune(),       # Tunable minimum node size
    tree_depth = tune()   # Tunable tree depth
  ) %>%
    set_engine("lightgbm") %>% # Set the engine to 'lightgbm'
    set_mode("classification") # Set the model mode to 'classification'
}

# Define a function to visualize model importance using ggplot2
ggplot_imp <- function(...) {
  # Capture all arguments in a list
  obj <- list(...)
  
  # Extract the metric name from the first element's attribute
  metric_name <- attr(obj[[1]], "loss_name")
  
  # Create a label for the x-axis using the metric name
  metric_lab <- paste(metric_name, 
                      "after permutations\n(higher indicates more important)")
  
  # Combine all elements of the list into a single data frame and filter out the baseline
  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")
  
  # Filter the combined data for the "_full_model_" variable to get permutation values
  perm_vals <- full_vip %>%
    filter(variable == "_full_model_") %>%
    group_by(label) %>%
    # Calculate the mean dropout loss for each label
    aggregate(dropout_loss ~ label, FUN = mean)
  
  # Start building the ggplot object
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% # Exclude the full model from the plot
    mutate(variable = fct_reorder(variable, dropout_loss)) %>% # Reorder variables by dropout loss
    ggplot(aes(dropout_loss, variable)) # Initialize the plot with aesthetic mappings
  
  # If there's more than one model in the input, use facets and boxplots
  if(length(obj) > 1) {
    p <- p +
      facet_wrap(vars(label)) + # Create facets based on the label variable
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 linewidth = 1.4, lty = 2, alpha = 0.7) + # Add a vertical line for each mean dropout loss
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2) # Add a semi-transparent boxplot
  } else {
    # If there's only one model, just add a vertical line and boxplot without facets
    p <- p +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
  }
  
  # Finalize the plot with no legend and custom axis labels
  p +
    theme(legend.position = "none") + # Hide the legend
    labs(x = metric_lab, # Set the x-axis label
         y = NULL, # No y-axis label
         fill = NULL, # No fill label
         color = NULL) # No color label
}

#This function takes in rank data and three model specifications. 
#It performs an initial data split, fits the models to the test data, explains the models, calculates variable importance, visualizes the importance, and summarizes the results. 
expmod <- function(rankdata, mod1, mod2, mod3) {
  # Set a seed for reproducibility
  set.seed(123)
  
  # Perform an initial split of the data into training and testing sets, stratified by a variable
  split_pbp <- initial_split(merge_common, 0.7, strata = name2)
  
  # Obtain the training and testing datasets
  train_data <- training(split_pbp)
  test_data <- testing(split_pbp)
  
  # Define three workflows with different models and a common formula, then fit them to the test data
  wf_1 <- workflow() %>% add_model(mod1) %>% add_formula(formula_string) %>% fit(data = test_data)
  wf_2 <- workflow() %>% add_model(mod2) %>% add_formula(formula_string) %>% fit(data = test_data)
  wf_3 <- workflow() %>% add_model(mod3) %>% add_formula(formula_string) %>% fit(data = test_data)
  
  # Explain the models using the test data and specific labels and outcomes
  explainer_1 <- explain_tidymodels(wf_1, data = test_data[, -1], label = mod[rankdata$rmse_wflow_id[1]], y = as.numeric(test_data[, 1]))
  explainer_2 <- explain_tidymodels(wf_2, data = test_data[, -1], label = mod[rankdata$rmse_wflow_id[2]], y = as.numeric(test_data[, 1]))
  explainer_3 <- explain_tidymodels(wf_3, data = test_data[, -1], label = mod[rankdata$rmse_wflow_id[3]], y = as.numeric(test_data[, 1]))
  
  # Set seeds for model interpretation and calculate variable importance for each model
  set.seed(1803)
  vip_1 <- model_parts(explainer_1, loss_function = loss_root_mean_square)
  set.seed(1804)
  vip_2 <- model_parts(explainer_2, loss_function = loss_root_mean_square)
  set.seed(1805)
  vip_3 <- model_parts(explainer_3, loss_function = loss_root_mean_square)
  
  # Create a plot of the variable importance using a custom function 'ggplot_imp'
  p <- ggplot_imp(vip_1, vip_2, vip_3) + theme_bw()
  # Save the plot to a PDF file with a specific naming convention
  ggsave("./vip3-tune.pdf",p, height = 15, width = 15)
  
  # Aggregate the variable importance measures and rename the columns to include model identifiers
  summary_1 <- aggregate(dropout_loss ~ variable, data = vip_1, FUN = sum)
  colnames(summary_1)[2] <- paste(mod[rankdata$rmse_wflow_id[1]], colnames(summary_1)[2], sep = "-")
  summary_2 <- aggregate(dropout_loss ~ variable, data = vip_2, FUN = sum)
  colnames(summary_2)[2] <- paste(mod[rankdata$rmse_wflow_id[2]], colnames(summary_2)[2], sep = "-")
  summary_3 <- aggregate(dropout_loss ~ variable, data = vip_3, FUN = sum)
  colnames(summary_3)[2] <- paste(mod[rankdata$rmse_wflow_id[3]], colnames(summary_3)[2], sep = "-")
  
  # Merge the summaries and calculate the total sum of importance measures
  merged_df_vip <- merge(merge(summary_1, summary_2, by = "variable"), summary_3, by = "variable")
  merged_df_vip$sum <- rowSums(merged_df_vip[, 2:4])
  # Sort the merged data frame by the total sum in descending order
  sorted_vip <- merged_df_vip[order(merged_df_vip$sum, decreasing = T), ]
  # Output the top rows of the sorted variable importance data
  head(sorted_vip)
  
  # Write the sorted variable importance data to a CSV file with a specific naming convention
  write.csv(sorted_vip, file = "vip3-tune.csv")
  
  # Comparing models using a funnel plot
  plot_data <- funnel_measure(explainer_1, explainer_2,
                              partition_data = test_data[,-1],
                              nbins = 5, measure_function = DALEX::loss_root_mean_square, show_info = FALSE)

  p <- plot(plot_data)
  ggsave("funnelmod.pdf",p,height = 20,width = 15)
}

