library(vroom)
library(tidyverse)
library(tidymodels)
library(dplyr)

bike_data <- vroom("train.csv")
view(bike_data)

# dyplyr Cleaning Step
bike_data_clean <- bike_data %>%
  mutate(weather = ifelse(weather == 4, 3, weather))

# Feature Engineering Step
my_bike_data <- recipe(count~., data = bike_data_clean) %>%
  step_num2factor(weather, levels = c("Clear", "Cloudy/Misty", "Light Snow/Rain")) %>%
  step_date(datetime, features="dow") %>%
  step_rm(casual) %>%
  step_rm(registered) %>%
  step_num2factor(season, levels = c("Spring", "Summer", "Fall", "Winter")) %>%
  step_mutate(holiday = ifelse(holiday == 0, "No", "Yes")) %>%
  step_mutate(workingday = ifelse(workingday == 0, "No", "Yes"))
  
prepped_data <- prep(my_bike_data)
bike_data_wrangle <- bake(prepped_data, new_data = bike_data)
view(bike_data_wrangle)
