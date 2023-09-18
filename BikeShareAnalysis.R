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


