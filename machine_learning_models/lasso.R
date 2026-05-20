library(tidytext)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(naivebayes)
library(discrim) 
library(glmnet)

# DATA 
urlfile ="https://raw.githubusercontent.com/MaRo406/EDS-231-text-sentiment/main/data/climbing_reports_model_dat.csv"
incidents_df<-readr::read_csv(url(urlfile))

set.seed(321)

# ------------------------------------------------------------------------------
#                       Training Split 
# ------------------------------------------------------------------------------

# Split the data
incident2class <- incidents_df %>% 
  mutate(deadly = factor(if_else(
    is.na(Deadly), "nondeadly", "deadly"
  )))
# 20/80 split 
incidents_split <- rsample::initial_split(incident2class, prop = 0.8, strata = deadly)
incidents_train <- training(incidents_split)
incidents_test <- testing(incidents_split)

# Pre processing ----------------------------------------------------------------
# Specify data's predictor & outcome variables
incidents_rec <- recipe(deadly ~ Text, data = incidents_train)

# Pre-processing
recipe <- incidents_rec %>% 
  step_tokenize(Text) %>%  # character predictor --> token variable
  step_tokenfilter(Text,  # token variable --> filtered based on frequency
                   max_tokens = 1000) %>% # filter for 1,000 most common words 
  step_tfidf() # token --> multi variable containing term freq inverse doc freq of tokens

# Bundle  everything into a single object
incidents_wf <- workflow %>% 
  add_recipe(recipe)

# ------------------------------------------------------------------------------
#                      Lasso
# ------------------------------------------------------------------------------

# Define the recipe
incidents_wf <- workflow %>% 
  add_recipe(recipe)

# Penalty-tuning-specification 
tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# Hyper parameter - lambda 
lambda_grid <- grid_regular(penalty(), # Create 30 possible values for the regularization penalty. 
                            levels = 30) # 

# Create a workflow to bundle recipe and model - updated workflow 
lasso_wf <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(tune_spec)

# TUNE - Fit the model 
set.seed(2023)
tune_rs <- tune_grid( # tune_grid() fits a model at each of the lambda values 
  lasso_wf,
  incidents_folds,
  grid = lambda_grid,
  control = control_resamples(save_pred = T))

# Evaluate & Predictions
collect_metrics(tune_rs)
autoplot(tune_rs)
tune_rs %>%show_best(metric = "roc_auc")
tune_rs %>% show_best(metric = "accuracy")
chosen_acc <- tune_rs %>% select_by_one_std_err(metric = "accuracy", -penalty)

# Finalize our workflow with best regularization penalty.
final_lasso <- finalize_workflow(lasso_wf, chosen_acc) 

# Fit to the training data.\
fitted_lasso <- fit(final_lasso, incidents_train)

# Fit to the test data and see how we did.
last_fit(final_lasso, incidents_split) %>%
  collect_metrics()




