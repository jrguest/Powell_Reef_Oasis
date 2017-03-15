################################################################################
##' @title Inspecting z-scores
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2017-03-14
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

dat <- read_csv("data_derived/Long_term_z_CV.csv")
dat

dat %>% group_by(location) %>% 
  summarise(n_rows = n())

### Scale coral cover CV
dat2 <- dat %>% 
  group_by(location) %>% 
  mutate(cv_z = scale(CV_coral_cover)) %>% 
  ungroup()



dat %>% 
  ggplot(aes(Z_mean_coral_cover, CV_coral_cover, color = island)) + 
  geom_point() + 
  facet_wrap(~ location) + 
  geom_vline(xintercept = c(1, 2), color = "gray20")

dat %>% 
  ggplot(aes(Z_mean_coral_cover, cv_z, color = island)) + 
  geom_point() + 
  facet_wrap(~ location) + 
  geom_vline(xintercept = c(0, 1, 2), color = "gray20", linetype = "dashed") + 
  geom_hline(yintercept = 0, linetype = "dashed")

##### FREQUENCY PLOTS -  #####

dat %>% 
  ggplot(aes(Z_mean_coral_cover)) + 
  geom_histogram(binwidth = 0.1) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray") + 
  xlab("Coral cover (z-score)")

ggsave("figures/histo_coral_cover_Z.png", height = 3.5, width = 3.5)


cv_sd = sd(dat$CV_coral_cover)
cv_mean = mean(dat$CV_coral_cover)


dat %>% 
  ggplot(aes(CV_coral_cover)) + 
  geom_histogram(binwidth = 5) + 
  geom_vline(xintercept = cv_sd + cv_mean, linetype = "dashed", color = "darkgray") + 
  xlab("CV of coral cover")
ggsave("figures/histo_coral_cover_CV.png", height = 3.5, width = 3.5)


##### DENSITY PLOTS -  #####

quantile(dat$Z_mean_coral_cover, na.rm = TRUE, probs = c(seq(0,1, 0.05)))

dat %>% 
  ggplot(aes(Z_mean_coral_cover)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
  geom_density(alpha = 0.2, aes(fill = NULL), color = "red", size = 1) + 
  geom_vline(xintercept = 1, linetype = "dashed")


dat %>% 
  ggplot(aes(CV_coral_cover)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) + 
  geom_density(alpha = 0.2, aes(fill = NULL), color = "red", size = 1) + 
  geom_vline(xintercept = 50, linetype = "dashed")


dat %>% 
  ggplot(aes(Z_mean_coral_cover, fill = location)) + 
  geom_histogram(binwidth = 0.2) + 
  facet_wrap(~ location) 

dat %>% 
  ggplot(aes(Z_mean_coral_cover, fill = location)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ location) + 
  geom_vline(xintercept = 1, linetype = "dashed")




dat %>% 
  ggplot(aes(CV_coral_cover, fill = location)) + 
  geom_histogram(binwidth = 10) + 
  facet_wrap(~ location)

dat %>% 
  ggplot(aes(CV_coral_cover, fill = location)) + 
  geom_histogram(aes(y = ..density..), binwidth = 10) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ location)

dat %>% 
  ggplot(aes(cv_z, fill = location)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.25) + 
  geom_density(alpha = 0.2) + 
  facet_wrap(~ location)

dat %>% 
  ggplot(aes(cv_z)) + 
  geom_histogram(binwidth = 0.25) 


