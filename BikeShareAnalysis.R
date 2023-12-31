library(vroom)
library(tidyverse)
library(tidymodels)
library(dplyr)

bike_data <- vroom("train.csv")
new_bike_data <- vroom("test.csv")

# dyplyr Cleaning Step
bike_data <- bike_data %>%
  select(-casual, -registered)


# Feature Engineering Step
my_bike_recipe <- recipe(count~., data = bike_data) %>%
  step_num2factor(weather, levels = c("Clear", "Cloudy/Misty", "Light Snow/Rain")) %>%
  step_date(datetime, features="dow") %>%
  step_num2factor(season, levels = c("Spring", "Summer", "Fall", "Winter")) %>%
  step_mutate(weather, levels = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(holiday = ifelse(holiday == 0, "No", "Yes")) %>%
  step_mutate(workingday = ifelse(workingday == 0, "No", "Yes")) %>%
  step_time(datetime, features = "hour")
  
prepped_data <- prep(my_bike_recipe)
bake(prepped_data, new_data = bike_data)
bake(prepped_data, new_data = new_bike_data)


# Linear Regression Section
my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(my_bike_recipe) %>%
  add_model(my_mod) %>%
  fit(data = bike_data)

bike_predictions <- predict(bike_workflow,
                            new_data = new_bike_data) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))
vroom_write(x = bike_predictions, file = "TestPreds.csv", delim = ",")


# Poisson Regression Section
library(poissonreg)

pois_mod <- poisson_reg() %>%
  set_engine("glm")

bike_pois_workflow <- workflow() %>%
  add_recipe(my_bike_recipe) %>%
  add_model(pois_mod) %>%
  fit(data = bike_data)

bike_pois_predictions <- predict(bike_pois_workflow,
                                 new_data = new_bike_data) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(datetime = as.character(format(datetime)))
vroom_write(x = bike_pois_predictions, file = "TestPredsPois.csv", delim = ",")


# Penalized Regression Section
library(glmnet)

my_bike_recipe <- recipe(count~., data = bike_data) %>%
  step_mutate(weather, levels = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(hour = datetime_hour) %>%
  step_rm(datetime_hour) %>%
  step_mutate(daytime = ifelse(hour > 6 & hour < 19, "1", "0")) %>%
  step_mutate(daytime = as.factor(daytime)) %>%
  step_mutate(workingday = as.factor(workingday)) %>%
  step_mutate(holiday = as.factor(holiday)) %>%
  step_mutate(season = as.factor(season)) %>% 
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

log_bike_data <- bike_data %>%
  mutate(count = log(count))

preg_model <- poisson_reg(penalty = 1, mixture = 0) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(my_bike_recipe) %>%
  add_model(preg_model) %>%
  fit(data = log_bike_data)

log_preds <- predict(preg_wf, 
                     new_data = new_bike_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(log_preds, "Penalized_Log_Pois_Preds_Lam1_Mixzero.csv", delim = ",")


# Penalized Regression (Log Model)
bike_data <- vroom("train.csv")
new_bike_data <- vroom("test.csv")

bike_data <- bike_data %>%
  select(-casual, -registered)

log_bike_data <- bike_data %>%
  mutate(count = log(count))

my_bike_recipe <- recipe(count~., data = bike_data) %>%
  step_mutate(weather, levels = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(hour = datetime_hour) %>%
  step_rm(datetime_hour) %>%
  step_mutate(daytime = ifelse(hour > 6 & hour < 19, "1", "0")) %>%
  step_mutate(daytime = as.factor(daytime)) %>%
  step_mutate(workingday = as.factor(workingday)) %>%
  step_mutate(holiday = as.factor(holiday)) %>%
  step_mutate(season = as.factor(season)) %>% 
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

pregression_model <- linear_reg(penalty = tune(),
                                mixture = tune()) %>%
  set_engine("glmnet")

pregression_wf <- workflow() %>%
  add_recipe(my_bike_recipe) %>%
  add_model(pregression_model)

tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

folds <- vfold_cv(log_bike_data, v = 10, repeats = 1)

CV_results <- pregression_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq))

collect_metrics(CV_results) %>%
  filter(.metric == "rmse") %>%
  ggplot(data =., aes(x = penalty, y = mean, color = factor(mixture))) +
  geom_line()

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- pregression_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = log_bike_data)

preds <- predict(final_wf, 
                 new_data = new_bike_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(preds, "PenalizedPreds.csv", delim = ",")


# Regression Tree Model
library(rpart)

bike_data <- vroom("train.csv")
new_bike_data <- vroom("test.csv")

bike_data <- bike_data %>%
  select(-casual, -registered)

log_bike_data <- bike_data %>%
  mutate(count = log(count))

my_recipe <- recipe(count~., data = bike_data) %>%
  step_mutate(weather, levels = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(hour = datetime_hour) %>%
  step_rm(datetime_hour) %>%
  step_mutate(daytime = ifelse(hour > 6 & hour < 19, "1", "0")) %>%
  step_mutate(daytime = as.factor(daytime)) %>%
  step_mutate(workingday = as.factor(workingday)) %>%
  step_mutate(holiday = as.factor(holiday)) %>%
  step_mutate(season = as.factor(season)) %>% 
  step_rm(datetime)

my_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(log_bike_data, v = 5, repeats = 1)

CV_results <- tree_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq))

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- tree_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = log_bike_data)

preds <- predict(final_wf, 
                 new_data = new_bike_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(preds, "TreePreds.csv", delim = ",")


# Random Forests Modification
library(rpart)
library(ranger)

bike_data <- vroom("train.csv")
new_bike_data <- vroom("test.csv")

bike_data <- bike_data %>%
  select(-casual, -registered)

log_bike_data <- bike_data %>%
  mutate(count = log(count))

my_recipe <- recipe(count~., data = bike_data) %>%
  step_mutate(weather, levels = ifelse(weather == 4, 3, weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(hour = datetime_hour) %>%
  step_rm(datetime_hour) %>%
  step_mutate(daytime = ifelse(hour > 6 & hour < 19, "1", "0")) %>%
  step_mutate(daytime = as.factor(daytime)) %>%
  step_mutate(workingday = as.factor(workingday)) %>%
  step_mutate(holiday = as.factor(holiday)) %>%
  step_mutate(season = as.factor(season)) %>% 
  step_rm(datetime)

my_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_mod)

tuning_grid <- grid_regular(mtry(range = c(1,10)),
                            min_n(),
                            levels = 5)

folds <- vfold_cv(log_bike_data, v = 5, repeats = 1)

CV_results <- forest_wf %>%
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae, rsq))

bestTune <- CV_results %>%
  select_best("rmse")

final_wf <- forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = log_bike_data)

preds <- predict(final_wf, 
                 new_data = new_bike_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(preds, "ForestPreds.csv", delim = ",")


# Model Stacking Modification
library(stacks)

bike_data <- vroom("train.csv")
test_bike_data <- vroom("test.csv")

bike_data <- bike_data %>%
  select(-casual, -registered)

log_bike_data <- bike_data %>%
  mutate(count = log(count))

# Split data for cross validation
folds <- vfold_cv(bike_data, v = 5, repeats = 1)

# Create control grid
untunedmodel <- control_stack_grid() 
tunedmodel <- control_stack_resamples()

# Set up recipe
stack_recipe <- recipe(count~., data = bike_data) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = factor(weather,levels = 1:3, labels = c("Sunny", "Cloudy", "Rain/Snow"))) %>%
  step_mutate(season = factor(season, levels = 1:4, labels = c("Spring", "Summer", "Fall", "Winter"))) %>%
  step_mutate(holiday = factor(holiday, levels = c(0,1), labels = c("No", "Yes"))) %>%
  step_mutate(workingday = factor(workingday, levels = c(0,1), labels = c("No", "Yes"))) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(hour = datetime_hour) %>%
  step_rm(datetime_hour) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

prepped_recipe <- prep(stack_recipe)
bake(prepped_recipe, new_data = bike_data)
bake(prepped_recipe, new_data = test_bike_data)

# Linear Regression Model
lin_mod <- linear_reg() %>%
  set_engine("lm")

linreg_wf <- workflow() %>%
  add_recipe(stack_recipe) %>%
  add_model(lin_mod)

linreg_model <- linreg_wf %>%
  fit_resamples(resamples = folds,
                control = tunedmodel)

# Poisson Regression Model
pois_mod <- poisson_reg() %>%
  set_engine("glm")

pois_workflow <- workflow() %>%
  add_recipe(stack_recipe) %>%
  add_model(pois_mod)

pois_reg_model <- fit_resamples(pois_workflow,
                                resamples = folds,
                                control = tunedmodel)

# Penalized Regression Model
preg_model <- linear_reg(penalty = tune(),
                         mixture = tune()) %>%
  set_engine("glmnet")

preg_wf <- workflow() %>%
  add_recipe(stack_recipe) %>%
  add_model(preg_model)

preg_tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 5)

preg_models <- preg_wf %>%
  tune_grid(resamples = folds,
            grid = preg_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedmodel)

# Set up Decision Tree
tree_mod <- decision_tree(tree_depth = tune(),
                        cost_complexity = tune(),
                        min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")

tree_wf <- workflow() %>%
  add_recipe(stack_recipe) %>%
  add_model(tree_mod)

tree_tuning_grid <- grid_regular(tree_depth(),
                            cost_complexity(),
                            min_n(),
                            levels = 5)

tree_models <- tree_wf %>%
  tune_grid(resamples = folds,
            grid = tree_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedmodel)

# Random Forest Set Up
forest_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 500) %>%
  set_engine("ranger") %>%
  set_mode("regression")

forest_wf <- workflow() %>%
  add_recipe(stack_recipe) %>%
  add_model(forest_mod)

forest_tuning_grid <- grid_regular(mtry(range = c(1,10)),
                            min_n(),
                            levels = 5)

forest_models <- forest_wf %>%
  tune_grid(resamples = folds,
            grid = forest_tuning_grid,
            metrics = metric_set(rmse),
            control = untunedmodel)

# STACK MODELS
# Specify Models
my_stack <- stacks() %>%
  add_candidates(linreg_model) %>%
  add_candidates(pois_reg_model) %>%
  add_candidates(preg_models) %>%
  add_candidates(tree_models) %>%
  add_candidates(forest_models)

stack_mods <- my_stack %>%
  blend_predictions() %>%
  fit_members()

stack_preds <- predict(stack_mods, 
                       new_data = new_bike_data) %>%
  bind_cols(., new_bike_data) %>%
  select(datetime, .pred) %>%
  rename(count = .pred) %>%
  mutate(count = pmax(0, count)) %>%
  mutate(datetime = as.character(format(datetime)))

vroom_write(stack_preds, "StackedPreds.csv", delim = ",")
