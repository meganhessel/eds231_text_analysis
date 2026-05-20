library(tidytext)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(naivebayes)
library(discrim) 
library(glmnet)
library(kknn)



# DATA 
urlfile ="https://raw.githubusercontent.com/MaRo406/EDS-231-text-sentiment/main/data/climbing_reports_model_dat.csv"
incidents_df<-readr::read_csv(url(urlfile))

set.seed(321)

# ------------------------------------------------------------------------------
#                       Split 
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
recipe <- incidents_rec |> 
  step_tokenize(Text) |> # character predictor --> token variable
  step_tokenfilter(Text,  # token variable --> filtered based on frequency
                   max_tokens = 1000) |> # filter for 1,000 most common words 
  step_tfidf(Text) # token --> multi variable containing term freq inverse doc freq of tokens

# ------------------------------------------------------------------------------
#                       K Nearest Neighbor 
# ------------------------------------------------------------------------------
# Penalty-tuning-specification 
knn_spec <- nearest_neighbor(neighbors = tune(), ) %>% 
  set_engine("kknn") %>% 
  set_mode("classification")

# Create Cross-Validation Folds
cv_folds <- vfold_cv(incidents_train, v = 10, strata = deadly)

# Hyper parameter - K neighbors 
k_grid <- grid_regular(neighbors(range = c(1, 50)), levels = 20)

# Create a workflow to bundle recipe and model - updated workflow 
knn_wf <- workflow() %>% 
  add_recipe(recipe) %>% 
  add_model(knn_spec)

# TUNE - Fit the model 
set.seed(321)
tune_knn <- tune_grid( # tune_grid() fits a model at each of the HP values 
  knn_wf,
  cv_folds,
  grid = k_grid,
  control = control_resamples(save_pred = T))

# Evaluate & Predictions
chosen_acc <- tune_knn %>% select_by_one_std_err(metric = "accuracy", neighbors)

# Finalize our workflow with best regularization penalty.
final_knn  <- finalize_workflow(knn_wf, chosen_acc)

# Fit to the training data.
fitted_knn <- fit(final_knn, incidents_train)

# Fit to the test data and see how we did.
last_fit(final_knn, incidents_split) %>%
  collect_metrics()

predictions <- last_fit(final_knn, incidents_split) %>% 
  collect_predictions()

# ------------------------------------------------------------------------------
#                                   ROC AUC
# ------------------------------------------------------------------------------
# ROC 
predictions %>%
  group_by(id) %>%
  roc_curve(truth = deadly, .pred_deadly) %>%
  autoplot() +
  labs(
    color = "Resamples",
    title = "ROC curve for Climbing Incident Reports"
  )

# Confusion Matrix 
tune_knn %>%
  conf_mat_resampled(parameters = chosen_acc, # pass the best parameters (AI told me this)
                     tidy = FALSE) %>%
  autoplot(type = "heatmap") + 
  scale_fill_gradient(low = "#f7fbff", high = "#08306b")
