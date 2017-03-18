################################################################################
##' @title Functions to simulate time series of coral cover
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

library(dplyr)

##### Assign variables #####

# Mean coral cover
coral_cover = 40
# Standard deviation of coral cover
coral_cover_sd = 3
# Number of years
number_yrs = 30
# Linear trend
linear_trend = 0
# Standard deviation of trend
linear_trend_sd = 0
# Periodicity (years) [for a 30 yr time series, a period of 30 results in a U]
period_yrs = 35
# SD of periodicity (years) 
period_yrs_sd = 20
# Amplitude 
amp = 15
# Time
x = seq(1:number_yrs)
# Phase shift
phase_shift = 0 * pi # starting at top (cos curve)
#phase_shift = 0.5 * pi # sin curve, starting at middle
#phase_shift = 1 * pi # cos curve, starting at bottom

##### STABLE #####

# stable_simF <- function(cc = coral_cover, cc_sd = coral_cover_sd, 
#                         yrs = number_yrs, randomize_cover = TRUE){
#   
#   # If I am randomizing starting values:
#   if(randomize_cover == TRUE){
#     # Choose the starting coral cover from a normal distribution
#     cc = rnorm(1, mean = cc, sd = 10)
#     # Choose the starting coral cover sd from a normal distribution
#     cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)}
# 
#   ## Get time series
#   y = rnorm(number_yrs, mean = cc, sd = cc_sd)
#   
#   ## Assemble data frame
#   sim_df <- data.frame(year = 1:number_yrs, 
#                        w = cc - y, slope = 0, intercept = cc, 
#                        y = y)
#   
#   return(sim_df)
# }

# Starting coral cover with random samples between 5 and select upper bound
# Flip a coin - sample from a normal distribution OR sample from a uniform distribution
stable_simF <- function(cc = coral_cover, yrs = number_yrs, rnorm_theta = 0.5){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Get time series
  y = rnorm(number_yrs, mean = cc, sd = cc_sd)
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = cc - y, slope = 0, intercept = cc, 
                       y = y)
  
  return(sim_df)
}

stable_simF()

###### NEGATIVE LINEAR TREND ######

linear_simF <- function(cc = coral_cover, yrs = number_yrs, 
                        trend = linear_trend, trend_sd = linear_trend_sd, rnorm_theta = 0.5){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Random variation for each time point
  w <- rnorm(number_yrs, mean = 0, sd = cc_sd) 
  ## Random value for slope
  slope <- rnorm(1, mean = trend, sd = trend_sd)
  ## Random value for intercept (i.e., the starting coral cover)
  intercept <- rnorm(1, cc, sd = cc_sd)
  
  ## Get time series
  y <- slope * x + intercept + w
  
  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = w, slope = slope, intercept = intercept, 
                       y = y)
  
  return(sim_df)
}

linear_simF()

###### CYCLES ######

##' For the following cosine curve:
##' y = a * cos(bx + c)
##' a = amplitude (height)
##' b = period (length of one cycle of the curve)
##' if b = 1, we get the natural cycle of the cos curve, i.e., 2*pi
##' c = phase shift
##' if c = 0, the cosine curve starts at 'a'

non_linear_simF <- function(cc = coral_cover, yrs = number_yrs, 
                        trend = linear_trend, trend_sd = linear_trend_sd, 
                        rnorm_theta = 0.5){
  
  coin_flip <- sample(c(0, 1), size = 1, prob = c(rnorm_theta, 1 - rnorm_theta))
  
  # If coin_flip is 0, then I sample from normal distribution:
  if(coin_flip == 0){
    # Choose the starting coral cover from a normal distribution
    cc = rnorm(1, mean = cc, sd = 5)}
  
  # Otherwise, I sample from a uniform distribution
  if(coin_flip == 1){
    # Choose the starting coral cover from a uniform distribution
    cc = runif(1, max = cc, min = 5)}
  
  # Choose the starting coral cover sd from a normal distribution
  cc_sd = 0.1 * cc # make standard deviation 10% of starting coral cover
  cc_sd = rnorm(1, mean = cc_sd, sd = 0.5)
  
  # Change cc_sd if below some arbitrary threshold
  cc_sd = ifelse(cc_sd < 0.1, 0.1, cc_sd)
  
  ## Random variation for each time point
  w <- rnorm(number_yrs, mean = 0, sd = cc_sd) 
  ## Random value for slope
  slope <- rnorm(1, mean = trend, sd = trend_sd)
  ## Random value for intercept (i.e., the starting coral cover)
  intercept <- rnorm(1, cc, sd = cc_sd)
  
  # Phase shift
  phase_shift = runif(1, 0, 0.25) * pi
  
  ## Catch the wave
  period = rnorm(1, mean = period_yrs, sd = period_yrs_sd)
  cs = amp * cos(2*pi*1:number_yrs/period + phase_shift)

  ## Get time series
  y <- slope * x + intercept + w + cs

  ## Assemble data frame
  sim_df <- data.frame(year = 1:number_yrs, 
                       w = w, slope = slope, intercept = intercept, 
                       y = y)
  
  return(sim_df)
}

non_linear_simF()
