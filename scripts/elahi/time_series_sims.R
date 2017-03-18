################################################################################
##' @title Time-series simulations
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-12-01
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(tidyr)

##' I want to simulate the following types of trends:
##' stable (no trend), with variation Y fluctuating about a mean X
##' positive trend with a mean of X
##' negative trend with a mean of X
##' oscillating trend with a mean of X

##' What am I calculating from a population of trends?
##' average coral cover
##' coefficient of variation
##' slope
##' intercept at time X (e.g., 1985?)
##' 

##### EXAMPLE FOR CORAL COVER TIME SERIES #####

##' cc = coral cover
##' cc_sd = sd of cover
##' yrs = number of years
##' w = white noise (sampled from a normal distribution)
##' slope = slope of temporal trend
##' intercept = intercept of temporal trend

###### STABLE ######

## White noise around a mean of coral cover
cc = 30
cc_sd = 3
yrs = 30
w = rnorm(yrs, mean = cc, sd = cc_sd) 
w
plot.ts(w)

## Function to return a time series with random variation about a mean coral cover
stable_simF <- function(cc = 30, cc_sd = 3, yrs = 30){
  w = rnorm(yrs, mean = cc, sd = cc_sd)
}

stable_df <- as.data.frame(replicate(5, stable_simF())) %>%
  mutate(year = seq(1:yrs)) %>% 
  gather(key = "ID", value = "cover", V1:V5)

stable_df %>% 
  ggplot(aes(year, cover, color = ID)) + 
  geom_point() + geom_line()

###### LINEAR TREND ######

## Assign state variables
# Mean coral cover
cc = 30
# Standard deviation of coral cover
cc_sd = 3
# Number of years
yrs = 30
# Linear trend
trend = 0
# Standard deviation of trend
trend_sd = 1

## Random variation for each time point
w = rnorm(yrs, mean = 0, sd = cc_sd) 

## Random value for slope (mean = 0, sd = 1)
slope = rnorm(1, mean = trend, sd = trend_sd)

## Random value for intercept
intercept = rnorm(1, cc, sd = cc_sd)

x = seq(1:yrs)
y = slope * x + intercept + w
plot.ts(y)
mean(y)
summary(lm(y ~ x))

##' For each simulation, need to:
##' 1) assign 1 slope (at random)
##' 2) assign 1 intercept (at random)
##' 3) assign N yrs of random variation at each time point
##' 4) calculate cover using slope, intercept, and random variation


# Mean coral cover
coral_cover = 30
# Standard deviation of coral cover
coral_cover_sd = 3
# Number of years
number_yrs = 30
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 0.5

## Function to return a time series with a trend

x = seq(1:yrs)

linear_simF <- function(cc = coral_cover, cc_sd = coral_cover_sd, yrs = number_yrs, 
                        trend = linear_trend, trend_sd = linear_trend_sd){
  
  ## Random variation for each time point
  w <- rnorm(yrs, mean = 0, sd = cc_sd) 
  
  ## Random value for slope (mean = 0, sd = 1)
  slope <- rnorm(1, mean = trend, sd = trend_sd)
  
  ## Random value for intercept
  intercept <- rnorm(1, cc, sd = cc_sd)
  
  ## Get time series
  y <- slope * x + intercept + w
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = w, slope = slope, intercept = intercept, 
                       y = y)
  
  sim_df
  
  return(sim_df)
}

linear_simF()


sim_df <- data.frame(sim = as.character(seq(1:100)))

sim_df2 <- sim_df %>% group_by(sim) %>% 
  do(linear_simF()) %>% ungroup()

sim_df2 %>% 
  ggplot(aes(year, y, color = sim)) + 
  geom_hline(yintercept = 0) + 
  geom_point() + geom_line() + 
  theme(legend.position = "none")


###### CYCLES ######

##' For the following cosine curve:
##' y = a * cos(bx + c)
##' a = amplitude (height)
##' b = period (length of one cycle of the curve)
##' if b = 1, we get the natural cycle of the cos curve, i.e., 2*pi
##' c = phase shift
##' if c = 0, the cosine curve starts at 'a'

## Assign variables

# Time
x = seq(1:yrs)
# Mean coral cover
coral_cover = 40
# Standard deviation of coral cover
coral_cover_sd = 3
# Number of years
number_yrs = 30
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 1
# Periodicity (years)
period = 15
# Intercept
intercept = 30
# Amplitude 
amp = 20

# Phase shift
phase_shift = 0 * pi # starting at top (cos curve)
#phase_shift = 0.5 * pi # sin curve, starting at middle
#phase_shift = 1 * pi # cos curve, starting at bottom

## Catch the wave
cs = amp * cos(2*pi*1:yrs/period + phase_shift)
plot.ts(cs)

## Get random variation
w = rnorm(yrs, mean = 0, sd = coral_cover_sd) 

## Simulate cover
y = linear_trend * x + coral_cover + w + cs
plot.ts(y)
mean(y)

###### TOY EXAMPLE ######

## Values were chosen to achieve similar mean coral cover

## Get random variation
set.seed(41)
w = rnorm(yrs, mean = 0, sd = coral_cover_sd) 

## High cover, stable
coral_cover = 30
y = coral_cover + w
plot.ts(y)
mean(y)
a_stable <- y

## High cover, linear decline
coral_cover = 40
linear_trend = -0.75
y = linear_trend * x + coral_cover + w
plot.ts(y)
mean(y)
b_linear <- y

## High cover, cyclical
coral_cover = 30
linear_trend = 0
period = 15
cs = amp * cos(2*pi*1:yrs/period + phase_shift)
y = linear_trend * x + coral_cover + cs + w
plot.ts(y)
mean(y)
c_cycles <- y

## High cover, decline then stasis
coral_cover = 34.6
period = 45
cs = amp * cos(2*pi*1:yrs/period + phase_shift)
y = linear_trend * x + coral_cover + cs + w
plot.ts(y)
mean(y)
d_nonlinear <- y

## Put in dataframe
toy_sims <- data_frame(year = seq(1:yrs), a_stable, b_linear, 
                       c_cycles, d_nonlinear)
tsl <- toy_sims %>% gather(key = scenario, value = cover, a_stable:d_nonlinear)
tsl

tsl %>% 
  ggplot(aes(year, cover, color = scenario)) + 
  geom_point(alpha = 0.7) + 
  geom_smooth() + 
  facet_wrap(~ scenario, nrow = 1) + 
  ggtitle("Starting with high percent coral cover (mean = 30%)") + 
  theme(legend.position = "none")

#ggsave("figures/trend_scenarios.png", height = 3.5, width = 7)

##' NEXT STEPS
##' (1) NEED TO BOUND CORAL COVER AT ZERO PERCENT - ALLOW RECRUITMENT FROM OUTSIDE?
##' (2) VARY THE STARTING CORAL COVER


##' James wants these scenarios
##' 1. Stable
##' 2. U-shaped recovery
##' 3. Linear decline
##' 4. Abrupt decline/phase shift




