################################################################################
##' @title Testing USVI data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-21
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

dat <- read.csv("data/data_usvi/CSUN_USVI_random_taxa_long_20160106.csv", 
                stringsAsFactors = FALSE, na.strings = c("nd", "NA"))
glimpse(dat)
names(dat)
unique(dat$taxa)
unique(dat$year)
unique(dat$site)
summary(dat)
unique(dat$percent_cover)
summary(dat$percent_cover)

##### SUM CORAL COVER FOR EACH QUADRAT #####
names(dat)
summary(dat)

dat %>% group_by(year, site, quadrat) %>% tally() %>% 
  filter(n > 35) %>% View()

quadSum <- dat %>% 
  group_by(year, site, quadrat) %>%
  summarise(coralPer = sum(percent_cover, na.rm = TRUE)) %>% ungroup()
quadSum

quadSum %>% group_by(year, site, quadrat) %>% tally() %>%
  filter(n > 1)


summary(quadSum$coralPer)

glimpse(quadSum)

quadSum %>% group_by(year, site) %>% tally() %>%
  ggplot(aes(year, n)) + geom_point()

##### GET MEAN CORAL COVER FOR EACH SITE-YEAR #####

siteMeans <- quadSum %>% group_by(year, site) %>%
  summarise(cover_mean = mean(coralPer, na.rm = TRUE), 
            cover_sd = sd(coralPer, na.rm = TRUE)) %>% ungroup() %>%
  arrange(year, site)

siteMeans %>% group_by(year, site) %>% tally()

siteMeans %>% 
  ggplot(aes(year, cover_mean)) + 
  geom_line() + facet_wrap(~ site)
