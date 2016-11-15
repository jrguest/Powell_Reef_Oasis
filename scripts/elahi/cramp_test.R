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
  
  
  

##### MUMBY TEST #####

##' At the regional scale
##' For each reef, get z-score
datR <- dat %>% group_by(REGION_PS) %>%
  mutate(year_mean = mean(HARD_CORAL, na.rm = TRUE), 
         year_sd = sd(HARD_CORAL, na.rm = TRUE), 
         zed = (HARD_CORAL - year_mean)/year_sd) %>% ungroup()

datR %>%
  ggplot(aes(zed)) + geom_histogram(binwidth = 0.2) + 
  facet_wrap(~ REGION_PS)


##' GLOBAL
##' For each reef, get z-score
datG <- dat %>%
  mutate(year_mean = mean(HARD_CORAL, na.rm = TRUE), 
         year_sd = sd(HARD_CORAL, na.rm = TRUE), 
         zed = (HARD_CORAL - year_mean)/year_sd) %>% ungroup()

datG %>%
  ggplot(aes(zed)) + geom_histogram(binwidth = 0.2) + 
  facet_wrap(~ REGION_PS)

ggsave("proj/pwg/pwg_figs/bruno1.png")

datG %>% filter(zed >= 2)

names(datG)
source("R/get_base_map.R")

map1_data <- data.frame(map('world', plot = FALSE)[c('x', 'y')])
head(map1_data)
map1_data <- map1_data %>% rename(LON = x, LAT = y)
head(map1_data)

map1 <- ggplot(map1_data, aes(LON, LAT)) + 
  geom_path() + coord_fixed() + 
  labs(x = "Longitude", y = "Latitude") 

datFilt <- datG %>% filter(zed >= 2)

map1 + 
  geom_point(aes(LON, LAT), data = datFilt, color = "red", size = 4, 
             alpha = 0.7)

ggsave("proj/pwg/pwg_figs/bruno2.png")

##### LM TEST #####
datG <- datG %>% mutate(reef = as.character(REEF_ID))
library(broom)

mod1 <- lm(HARD_CORAL ~ reef, dat = datG)
summary(mod1)
mod1$coefficients

datG$coefs <- mod1$coefficients

datG %>%
  ggplot(aes(coefs)) + geom_histogram(binwidth = 5) + 
  facet_wrap(~ REGION_PS) 






