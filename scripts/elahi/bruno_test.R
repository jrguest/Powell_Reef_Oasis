################################################################################
##' @title Testing BRUNO
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-19
##' 
##' @description 
##' 
##' @note 
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE))

##### LOAD PACKAGES, DATA #####
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)

dat <- read.csv("data/data_bruno/World.csv", 
                stringsAsFactors = FALSE)
head(dat)
glimpse(dat)
names(dat)
unique(dat$REEF_ID)

dat %>% filter(REEF_ID == "3")

dat %>% group_by(REEF_ID) %>% tally()


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
  





