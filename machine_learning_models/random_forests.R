library(tidytext)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(naivebayes)
library(discrim) 
library(glmnet)
library(ranger)


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
recipe <- incidents_rec |> 
  step_tokenize(Text) |> # character predictor --> token variable
  step_tokenfilter(Text,  # token variable --> filtered based on frequency
                   max_tokens = 1000) |> # filter for 1,000 most common words 
  step_tfidf(Text) # token --> multi variable containing term freq inverse doc freq of tokens


# ------------------------------------------------------------------------------
#                      Random Forests 
# ------------------------------------------------------------------------------

# Define the model specification
rf <- rand_forest(trees = 500, mtry = 2) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# Cross Validation Folds
set.seed(123)
incidents_folds <- vfold_cv(incidents_train)
incidents_folds

# Create a workflow to bundle recipe and model
rf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf)

# Fit the model to the data
rf_fit <- rf_workflow %>% 
  fit(data = incidents_train)

# Predictions with test data 
predictions <- predict(rf_fit, new_data = iris)






