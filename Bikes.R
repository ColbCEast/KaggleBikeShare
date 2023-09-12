library(vroom)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(GGally)
library(patchwork)

bikes <- vroom("train.csv")
view(bikes)
bikes$holiday <- as.factor(bikes$holiday)
bikes$season <- as.factor(bikes$season)
bikes$workingday <- as.factor(bikes$workingday)
bikes$weather <- as.factor(bikes$weather)

glimpse(bikes)
plot_intro(bikes)
plot_correlation(bikes)
plot_bar(bikes)
plot_histogram(bikes)
plot_missing(bikes)
ggpairs(bikes)

temp_plot <- ggplot(bikes, 
                    aes(x = temp)) +
  geom_histogram(binwidth = 1) + 
  labs(title = "Temp",
       x = "Temperature",
       y = "Count")
temp_plot

atemp_plot <- ggplot(bikes,
                     aes(x = atemp)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Atemp",
       x = "Effective Temp",
       y = "Count")
atemp_plot

register_plot <- ggplot(bikes,
                        aes(x = registered,
                            y = count)) +
  geom_point() +
  labs(title = "Registered Bikers",
       x = "Number Registered",
       y = "Total Count")
register_plot

temp_casual_plot <- ggplot(bikes,
                           aes(x = temp,
                               y = casual)) +
  geom_point()
temp_casual_plot

patchwork_plot <- (temp_plot + atemp_plot) / (register_plot + temp_casual_plot)
patchwork_plot

ggsave("STAT_348_PLOT.pdf", plot = patchwork_plot)
