################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(broom)

sim_df <- read.csv("scripts/elahi/coral_sims/output_sims/sim_df.csv") %>% 
  tbl_df() %>% select(-X)
str(sim_df)
sim_df
levels(sim_df$scenario)

##### TAKE RANDOM SUBSAMPLES OF DATA TO TEST THE EFFECT OF SAMPLE SIZE #####

# How many simulations total?
n_sims = 400
i = 1
i_vec <- rep(i,30)
for(i in 2:n_sims){
  i_new <- rep(i, 30)
  i_vec <- c(i_vec, i_new)
}

sim_df$sim_total <- i_vec
sim_df

# Get random samples
n_random = 100
set.seed(203)
random_sims <- sample(x = 1:n_sims, size = n_random, replace = FALSE)

# Now subset the data
sim_df_sub <- sim_df[sim_df$sim_total %in% random_sims, ]
sim_df_sub %>% group_by(scenario) %>% tally()

##### CALCULATE SUMMARY STATISTICS #####

# What am I going to summarise?

grand_means <- sim_df %>% 
  group_by(sim_total, scenario, sim) %>% 
  summarise(mean = mean(y), 
            median = median(y), 
            sd = sd(y), 
            n = n(), 
            cv = sd/mean * 100) %>% ungroup()
summary(grand_means)

grand_means2 <- grand_means %>% 
  mutate(mean_z = scale(mean)[,1], 
         med_z = scale(median)[,1], 
         cv_z = scale(cv)[,1])

##### GET LONG-TERM TRENDS #####
head(sim_df)
summary(sim_df)

# Function to calculate linear trend
get_lin_fit <- function(dat, offset = 0) {
  the_fit <- lm(y ~ I(year - offset), dat)
  the_fit_slope <- tidy(the_fit)[2, c(2,5)]
  return(the_fit_slope)
}

# Run per site
site_lm_fits <- sim_df %>%
  group_by(sim, scenario) %>% 
  do(get_lin_fit(.))

# Attach to means

grand_means3 <- left_join(grand_means2, site_lm_fits, by = c("sim", "scenario")) %>%
  mutate(lm_sig = ifelse(p.value < 0.05, "sig", "ns"), 
         lm_dir = ifelse(estimate < 0, "negative", "positive"), 
         oasis = ifelse(cv_z > 1 & mean_z > 0, "oasis_variable", 
                        ifelse(cv_z < 1 & mean_z > 1, "oasis_stable", 
                               "not_oasis")))

##### GET PERCENTAGE OF TIME ABOVE MEAN #####
head(sim_df)
summary(sim_df)

## I need to know the mean coral cover for each time-location
yr_means <- sim_df %>% group_by(year, scenario) %>% 
  summarise(yr_mean = mean(y)) %>% ungroup()
  
# Attach to raw data
sim_df2 <- left_join(sim_df, yr_means, by = c("year", "scenario"))

sim_df2 <- sim_df2 %>% 
  mutate(above_mean = ifelse(y > yr_mean, 1, 0))

#write.csv(sim_df2, "scripts/elahi/coral_sims/output_sims/sim_df2.csv")

above_mean_df <- sim_df2 %>% 
  group_by(sim, scenario) %>% 
  summarise(percent_above = sum(above_mean)/30) %>% 
  ungroup()

## attach to grand_means3
grand_means4 <- left_join(grand_means3, above_mean_df, by = c("sim", "scenario")) %>% 
  mutate(sim_total = seq(from = 1, to = 400, 1))
  
## Attach oasis results to raw data
sim_df3 <- grand_means4 %>% 
  select(sim, scenario, cv, med_z, mean_z, lm_sig:oasis) %>% 
  left_join(sim_df2, ., c("sim", "scenario")) %>% 
  mutate(sim2 = reorder(sim, desc(mean_z)))

mean_cover_df <- sim_df3 %>% 
  group_by(year) %>%
  summarise(y_mean = mean(y))


