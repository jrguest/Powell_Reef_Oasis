################################################################################
##' @title Simulate time series of coral cover
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####

source("scripts/elahi/coral_sims/sim_functions.R")

library(ggplot2)
library(dplyr)
library(tidyr)

# How many simulations per scenario?
n_scenarios = 100
sim_df <- data.frame(sim = seq(1:n_scenarios))

##### SIMULATE STABLE COVER #####

# Run simulations
# Then replace negative values with 0
sim_df2 <- sim_df %>% group_by(sim) %>% 
  do(stable_simF()) %>% ungroup() %>% 
  mutate(y = ifelse(y < 0, 0, y))

# Rename dataset and add column for scenario
sim_df_stable <- sim_df2 %>% 
  mutate(scenario = "Stable")

sim_df_stable %>% 
  slice(1:270) %>% 
  ggplot(aes(year, y)) + 
  geom_line(alpha = 0.5) + geom_point() + 
  facet_wrap(~ sim)

##### SIMULATE LINEAR TRENDS #####

# Need to specify slope - I want mostly negative trends
# Linear trend
linear_trend = -0.5
# Standard deviation of trend
linear_trend_sd = 0.5

# Run simulations
# Then replace negative values with 0
sim_df2 <- sim_df %>% group_by(sim) %>% 
  do(linear_simF()) %>% ungroup() %>% 
  mutate(y = ifelse(y < 0, 0, y))

# Rename dataset and add column for scenario
sim_df_linear <- sim_df2 %>% 
  mutate(scenario = "Linear")

##### SIMULATE OSCILLATIONS #####

## Need to specify slope 
## Default to zero and let the oscillations drive the patterns
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 0

## Choose periodicity
# Periodicity (years) [for a 30 yr time series, a period of 30 results in a U]
period_yrs = 40
# SD of periodicity (years) 
period_yrs_sd = 10

# Run simulations
# Then replace negative values with 0
sim_df2 <- sim_df %>% group_by(sim) %>% 
  do(non_linear_simF()) %>% ungroup() %>% 
  mutate(y = ifelse(y < 0, 0, y))

# Rename dataset and add column for scenario
sim_df_osc <- sim_df2 %>% 
  mutate(scenario = "Oscillations")

##### COMBINE DATASETS AND SAVE #####
sim_df2 %>% slice(1:300) %>% 
  ggplot(aes(year, y)) + 
  geom_point() + geom_line() + 
  theme(legend.position = "none") + 
  facet_wrap(~ sim)

sim_df <- rbind(sim_df_stable, sim_df_linear, sim_df_osc)
write.csv(sim_df, "scripts/elahi/coral_sims/output_sims/sim_df.csv")

