rm(list = ls())
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
library(tidyverse)
library(tidymodels)
library(furrr)
library(rsample)
library(ids)
library(broom)
library(glue)
library(MASS)
library(tictoc)
library(logistf)

model_type <- 'glmnet'

mpg_binary <- mpg %>% 
  mutate(hwy = as_factor(as.character(hwy > 20)))

trained_recipe <- 
  recipe(hwy ~ ., data = mpg) %>% 
  step_dummy(
    all_predictors(), 
    -all_numeric()
  ) 

trained_recipe <- 
  recipe(hwy ~ ., data = mpg_binary) %>% 
  step_dummy(
    all_predictors(), 
    -all_numeric()
  ) %>% 
  step_corr(all_predictors(), threshold = 10)

trained_recipe <- prep(trained_recipe, retain = TRUE)

train_test_split <- bootstraps(mpg_binary, times = 1000, strata = 'hwy')

build_model <- function(split, penalty = 1, mixture = .1) {
  
  train <- training(split)
  test <- testing(split)
  baked_train <- bake(trained_recipe, new_data = train)
  baked_test  <- bake(trained_recipe, new_data = test)
  
  model_results <-
    logistic_reg(penalty = penalty, mixture = mixture) %>%
    set_engine(model_type) %>%
    fit(hwy ~ ., data = baked_train)
  
  list(model = model_results, 
       train = baked_train, 
       test = baked_test, 
       parameters = list(penalty = penalty, mixture = mixture),
       time = Sys.time())
}

plan(multiprocess)


clean_model <- 
  function(model_iteration) {
    
    model <- model_iteration$model
    train <- model_iteration$train
    test <- model_iteration$test
    train$type = 'train'
    test$type = 'test' 
    test$predictions <- model %>% predict(test) %>% pull(.pred_class)
    data_df <- bind_rows(train, test)
    
    tidied_model <- tidy(model$fit)
    glanced_model <- glance(model$fit)
    
    data_df <- data_df %>%  group_by(type) %>% nest()
    tidied_model <- tidied_model %>% mutate(type = 'coeff') %>% group_by(type) %>% nest()
    glanced_model <- glanced_model %>% mutate(type = 'glance') %>% group_by(type) %>% nest()
    parameters <- 
      tibble(penalty  = model_iteration$parameters$penalty, mixture = model_iteration$parameters$mixture)
    parameters <- parameters %>% mutate(type = 'parameters') %>% group_by(type) %>% nest()
    
    data_df <- 
      data_df %>% 
      bind_rows(tidied_model) %>% 
      bind_rows(glanced_model) %>% 
      bind_rows(parameters)
    
    data_df$id = ids::random_id()
    
    list(data_df = data_df, model = model)
    
  }

model_data <- 
  map(
    c(
      future_map(train_test_split$splits, ~ build_model(., penalty = 0, mixture = .1)),
      future_map(train_test_split$splits, ~ build_model(., penalty = 0, mixture = .3)),
      future_map(train_test_split$splits, ~ build_model(., penalty = 0, mixture = .4)),
      future_map(train_test_split$splits, ~ build_model(., penalty = 0, mixture = 1))
    ),
    clean_model
  )


acc <- 
  model_data %>% 
  map_df(~ .$data_df) %>% 
  filter(type == 'test') %>% 
  unnest(data) %>% 
  group_by(id) %>% 
  accuracy(hwy, predictions)

params <- 
  model_data %>% 
  map_df(~ .$data_df) %>% 
  filter(type == 'parameters') %>% 
  unnest(data) %>% 
  right_join(acc)


params %>% 
  pivot_longer(
    c('.estimate')
  ) %>% 
  ggplot() +
  aes(x = value, fill = as_factor(mixture)) +
  geom_histogram(bins = 10) +
  facet_grid(penalty ~ .)
  

  
