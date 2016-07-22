################################################################################
##' @title Processing raw coral data from Moorea LTER
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @contributor 
##' 
##' @date 2016-07-22
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

dat <- read.csv("raw_data_process/raw_data/knb-lter-mcr.4_1_20151209.csv", 
                stringsAsFactors = FALSE)
names(dat)
names(dat)[7] <- "Taxon"

unique(dat$Date)
unique(dat$Taxon)

taxa <- unique(dat$Taxon)
notCoral <- taxa[c(2,3,4,5,22,31,32,38)]

dat %>% filter(Taxon == "")

dat2 <- dat[!dat$Taxon %in% notCoral, ]

datTest <- dat %>% 
  mutate(category = ifelse(Taxon == "Sand", "sand", 
                           ifelse(Taxon == "Crustose Coralline Algae / Bare Space", 
                                  "openSpace", 
                                  ifelse(Taxon == "Turf", "turf",
                                         ifelse(Taxon == "Macroalgae", "macroalgae", 
                                                ifelse(Taxon == "Soft Coral", "softCoral", 
                                                       ifelse(Taxon == "Non-coralline Crustose Algae", "openSpace", 
                                                              ifelse(Taxon == "Unknown or Other", "other", "hardCoral"))))))))


dat3 <- dat2 %>% filter(Date != "(195058 rows)")

dat4 <- dat3 %>% mutate(dateR = paste(Date, "-01", sep = ""), 
                        dateR = ymd(dateR), 
                        year = year(dateR))

names(dat4)
unique(dat4$Location)
unique(dat4$Site)
unique(dat4$Habitat)
unique(dat4$Transect)
unique(dat4$Quadrat)
head(dat4)

##### CREATE NEW COLUMNS FOR SYNTHESIS ######

dat5 <- dat4 %>%
  mutate(derived_site = paste(Site, Habitat, sep = "_"), 
         nest1 = paste("transect", Transect, sep = "_"), 
         nest2 = paste("quadrat", Quadrat, sep = "_"), 
         nest3 = NA, 
         nest4 = NA)
head(dat5)

##### FORMAT TABLE FOR META-ANALYSIS #####
names(dat4)

df_final <- dat5 %>% select(-c(Date, Location))
  
dfMeta <- data.frame(
  study_id = "lter_mcr", 
  derived_site = df_final$derived_site, 
  nest1 = df_final$nest1, 
  nest2 = df_final$nest2, 
  nest3 = df_final$nest3,
  date = df_final$dateR, 
  year = df_final$year, 
  state = df_final$Taxon, 
  percent_cover = df_final$Percent.Cover
  )
  

write.csv(dfMeta, "raw_data_process/processed_data/proc_lter_mcr_coral.csv")



##### quick test #####
head(dfMeta)

# Need to get coral cover per quad
quadSum <- dfMeta %>% 
  group_by(year, date, derived_site, nest1, nest2) %>%
  summarise(percent_cover = sum(percent_cover)) %>% ungroup()

##### GET MEAN CORAL COVER FOR EACH SITE-HABITAT-YEAR #####

siteMeans_moorea <- quadSum %>% group_by(year, derived_site) %>%
  summarise(percent_cover = mean(percent_cover, na.rm = TRUE)) %>% ungroup() %>%
  arrange(year, derived_site)




# get USVI data - already coral cover per quadrat
datUSVI <- read.csv("raw_data_process/processed_data/proc_ltreb_usvi.csv", 
                    stringsAsFactors = FALSE, na.strings = "nd")
glimpse(datUSVI)
unique(datUSVI$derived_site)

siteMeans_usvi <- datUSVI %>% group_by(year, derived_site) %>%
  summarise(percent_cover = mean(percent_cover, na.rm = TRUE)) %>% ungroup() %>%
  arrange(year, derived_site)

unique(siteMeans_usvi$derived_site)


siteMeans_usvi$study_id <- "usvi"
siteMeans_moorea$study_id <- "moorea"

smdf <- rbind(siteMeans_usvi, siteMeans_moorea)

head(smdf)
unique(smdf$derived_site)

smdf %>% group_by(study_id) %>% 
  summarise(startyr = min(year), 
            endyr = max(year))

##' Calculate z-scores
zedDF <- smdf %>% filter(year > 2004) %>%
  group_by(year) %>%
  mutate(year_mean = mean(percent_cover, na.rm = TRUE), 
         year_sd = sd(percent_cover, na.rm = TRUE), 
         zed = (percent_cover - year_mean)/year_sd) %>% ungroup()

head(zedDF)


