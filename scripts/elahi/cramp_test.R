################################################################################
##' @title Testing CRAMP
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-11-14
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

dat <- read.csv("data_derived/CRAMP_MHI_corcov.csv", 
                stringsAsFactors = FALSE)
head(dat)
summary(dat)

##### GET LONG-TERM SITE LEVEL MEANS #####

grand_means <- dat %>% group_by(Island, Site) %>% 
  summarise(mean = mean(Stony_coral), 
            sd = sd(Stony_coral), 
            n = n(), 
            cv = sd/mean, 
            stability = 1/cv) %>% ungroup()

grand_means2 <- grand_means %>% 
  mutate(mean_z = scale(mean), 
         cv_z = scale(cv))

summary(grand_means2)

grand_means2 %>% 
  ggplot(aes(mean, cv)) + 
  geom_point()

grand_means2 %>% 
  ggplot(aes(mean_z, cv_z, color = Island)) + 
  geom_smooth(method = "lm", aes(color = NULL), color = "black") + 
  geom_hline(yintercept = 2, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 2, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = -2, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -2, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Temporal variability in coral cover (z-score)") + 
  scale_x_continuous(limits = c(-4, 4)) + 
  scale_y_continuous(limits = c(-4, 4)) + 
  geom_point() +
  annotate("text", x = 3, y = -3, label = "Stable\noasis") + 
  annotate("text", x = 3, y = 3, label = "Variable\noasis") + 
  annotate("text", x = -3, y = -3, label = "Stable\ndump") + 
  annotate("text", x = -3, y = 3, label = "Variable\ndump") + 
  ggtitle("Main Hawaiian Islands") 

ggsave("figures/cramp_var_vs_mean.png", height = 5, width = 5)
  
  
  