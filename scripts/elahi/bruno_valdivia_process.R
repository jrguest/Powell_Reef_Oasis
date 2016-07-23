################################################################################
##' @title Processing raw coral data from Bruno and Valdivia
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

dat <- read.csv("raw_data_process/raw_data/World+human.csv", 
                stringsAsFactors = FALSE)

range(dat$YEAR)
median(dat$YEAR)


head(dat)
glimpse(dat)
names(dat)
unique(dat$REEF_ID)

dat %>% filter(REEF_ID == "3")

dat %>% group_by(REEF_ID) %>% tally()

##### CREATE NEW COLUMNS FOR SYNTHESIS ######

dat2 <- dat %>%
  mutate(derived_site = paste("reef", REEF_ID, sep = "_"),  
         nest1 = NA, 
         nest2 = NA,  
         nest3 = NA, 
         nest4 = NA)
head(dat2)

##### FORMAT TABLE FOR META-ANALYSIS #####
df_final <- dat2 %>% select(-c(X, REEF_ID, ))

dfMeta <- data.frame(
  study = "lter_mcr", 
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
