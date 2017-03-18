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

sim_df <- read.csv("scripts/elahi/coral_sims/output_sims/sim_df.csv") %>% tbl_df() %>%
  select(-X)
str(sim_df)
sim_df

n_sims = length(unique(sim_df$sim))

##### TAKE RANDOM SUBSAMPLES OF DATA TO TEST THE EFFECT OF SAMPLE SIZE #####

samp1 <- sim_df %>% 
  group_by(sim, scenario) %>% 
  sample_n(1) 

sample_size = 10 # of each scenario
random_vector <- sample(x = seq(1,n_sims,1), size = sample_size, replace = FALSE)
sort(random_vector)

#sim_df <- sim_df[sim_df$sim %in% random_vector, ]

##### CALCULATE SUMMARY STATISTICS #####

grand_means <- sim_df %>% 
  #filter(scenario != "Stable") %>% 
  group_by(sim, scenario) %>% 
  summarise(mean = mean(y), 
            sd = sd(y), 
            n = n(), 
            cv = sd/mean) %>% ungroup()
summary(grand_means)

grand_means2 <- grand_means %>% 
  mutate(mean_z = scale(mean)[,1], 
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

## Attach oasis results to raw data

sim_df2 <- grand_means3 %>% select(sim, scenario, mean_z, lm_sig:oasis) %>% 
  left_join(sim_df, ., c("sim", "scenario")) %>% 
  mutate(sim2 = reorder(sim, desc(mean_z)))

mean_cover = mean(sim_df2$y)




