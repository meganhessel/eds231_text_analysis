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
# Create the Recipe
knn_recipe <- recipe(deadly ~ ., data = incidents_train) %>%
  step_normalize(all_numeric_predictors()) 

# Create Cross-Validation Folds
cv_folds <- vfold_cv(incidents_train, v = 10, strata = deadly)

# Specify the KNN Model
knn_spec <- nearest_neighbor(neighbors = 5) %>%
  set_engine("kknn") %>%
  set_mode("classification")

# Combine into a workflow
knn_workflow <- workflow() %>%
  add_recipe(knn_recipe) %>%
  add_model(knn_spec)

# TUNE K using cross-validation
knn_results <- tune_grid(
  knn_workflow,
  resamples = cv_folds,
  grid = grid_regular(neighbors(range = c(1, 20)), levels = 10)
)

#FINALIZE AND EVALUATE 
# Select best parameters
best_knn <- select_best(knn_results, metric = "accuracy")

# Finalize workflow and fit to the full training set
final_fit <- knn_workflow %>%
  finalize_workflow(best_knn) %>%
  last_fit(incidents_split)

# View performance metrics
collect_metrics(final_fit)

# predict with test data 
predictions <- predict(rf_fit, new_data = incidents_test)


# ------------------------------------------------------------------------------

# Bundle into a Workflow
knn_workflow <- workflow() %>%
  add_recipe(knn_recipe) %>%
  add_model(knn_spec)

# Fit the Model
knn_fitted <- knn_workflow %>%
  fit(data = incidents_train)

# Make predictions with test data 
predictions <- predict(rf_fit, new_data = incidents_test)


