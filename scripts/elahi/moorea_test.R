################################################################################
##' @title Testing Moorea data
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

dat <- read.csv("data/data_moorea/knb-lter-mcr.4_1_20151209.csv", 
                stringsAsFactors = FALSE)
names(dat)[7] <- "Taxon"

unique(dat$Date)
unique(dat$Taxon)

taxa <- unique(dat$Taxon)
notCoral <- taxa[c(2,3,4,5,6,22,31,32,34,38)]

dat2 <- dat[!dat$Taxon %in% notCoral, ]

dat3 <- dat2 %>% filter(Date != "(195058 rows)")

dat4 <- dat3 %>% mutate(dateR = paste(Date, "-01", sep = ""), 
                        dateR = ymd(dateR), 
                        year = year(dateR))

dat4 %>% group_by(Site, Habitat) %>% tally()

names(dat4)
unique(dat4$Location)
unique(dat4$Site)
unique(dat4$Habitat)
unique(dat4$Transect)
unique(dat4$Quadrat)

##### SUM CORAL COVER FOR EACH QUADRAT #####
names(dat4)

quadSum <- dat4 %>% 
  group_by(year, dateR, Site, Habitat, Transect, Quadrat) %>%
  summarise(coralPer = sum(Percent.Cover)) %>% ungroup()

glimpse(quadSum)

quadSum %>% group_by(year, Site, Habitat) %>% tally()

quadSum %>% 
  ggplot(aes(dateR, coralPer, color = Habitat)) + 
  geom_point() + geom_smooth() + 
  facet_wrap(~ Site)

##### GET MEAN CORAL COVER FOR EACH SITE-HABITAT-YEAR #####

siteHabMeans <- quadSum %>% group_by(year, dateR, Site, Habitat) %>%
  summarise(cover_mean = mean(coralPer, na.rm = TRUE), 
            cover_sd = sd(coralPer, na.rm = TRUE)) %>% ungroup() %>%
  arrange(year, Habitat, Site)

siteHabMeans %>% group_by(year, Site) %>% tally()

siteHabMeans %>% 
  ggplot(aes(year, cover_mean, color = Habitat)) + 
  geom_line() + facet_wrap(~ Site)

siteHabMeans %>% 
  ggplot(aes(cover_mean)) + geom_histogram(binwidth = 5)

siteHabMeans %>% 
  ggplot(aes(cover_mean)) + geom_histogram(binwidth = 5) + 
  facet_wrap(~ dateR)

### Define oasis, based on 95%ile mean cover
cover95 <- quantile(siteHabMeans$cover_mean, 0.95, na.rm = TRUE)
cover05 <- quantile(siteHabMeans$cover_mean, 0.05, na.rm = TRUE)
coverBright <- quantile(siteHabMeans$cover_mean, 0.9, na.rm = TRUE)
coverDark <- quantile(siteHabMeans$cover_mean, 0.1, na.rm = TRUE)

shm2 <- siteHabMeans %>% 
  mutate(oasis_cutoff = quantile(cover_mean, 0.9, na.rm = TRUE), 
         oasis = ifelse(cover_mean > oasis_cutoff, "oasis", "not_oasis"), 
         dark_cutoff = quantile(cover_mean, 0.1, na.rm = TRUE), 
         darkSpot = ifelse(cover_mean < dark_cutoff, "dark spot", "not_dark"))

shm2 %>% 
  ggplot(aes(year, cover_mean, color = Habitat)) + 
  geom_line() + facet_wrap(~ Site) + 
  geom_point(aes(shape = oasis, size = oasis)) + 
  geom_hline(yintercept = coverBright, linetype = "dashed") + 
  geom_hline(yintercept = coverDark, linetype = "dashed")

shm2 %>% 
  ggplot(aes(year, cover_mean, color = Habitat)) + 
  geom_line() + facet_wrap(~ Site)
ggsave("proj/pwg/pwg_figs/lterPlot1.png", height = 5, width = 7)

##### GET MEAN CORAL COVER FOR EACH SITE-HABITAT-YEAR #####

names(shm2)
shm3 <- shm2 %>% select(-c(oasis_cutoff:darkSpot))

shm3 <- shm3 %>% 
  mutate(cover_grand_mean = mean(cover_mean, na.rm = TRUE), 
         cover_grand_sd = sd(cover_mean, na.rm = TRUE))

##### MUMBY TEST #####

##' For each year, get z-score

shm3 <- shm2 %>% select(-c(oasis_cutoff:darkSpot))

shm4 <- shm3 %>% group_by(year) %>%
  mutate(year_mean = mean(cover_mean, na.rm = TRUE), 
         year_sd = mean(cover_sd, na.rm = TRUE), 
         zed = (cover_mean - year_mean)/year_sd) %>% ungroup()

shm4 <- shm3 %>% # group_by(year) %>% 
  mutate(zed = scale(cover_mean)) # %>% ungroup()
  
shm4 %>% 
  ggplot(aes(year, zed, color = Habitat)) + 
  geom_line() + facet_wrap(~ Site)
ggsave("proj/pwg/pwg_figs/lterPlot2.png", height = 5, width = 7)

##' What is the median z-score per site?
##' 

zDF <- shm4 %>% group_by(Site, Habitat) %>%
  summarise(medianZed = median(zed, na.rm = TRUE))

zDF %>% 
  ggplot(aes(Site, medianZed, color = Habitat)) + geom_point() + 
  ggtitle("Oasis index (median zed)")
ggsave("proj/pwg/pwg_figs/lterPlot3.png", height = 3.5, width = 5)

##### MUMBY TEST - WITH BRUNO #####

# get Bruno data
datBruno <- read.csv("data/data_bruno/World.csv", 
                     stringsAsFactors = FALSE)
head(datBruno)
unique(datBruno$SUBREGION)
unique(datBruno$REGION_PS)

datBruno2 <- datBruno %>%
  select(-c(SUBREGION, MACROALGAE, PSI, LON, LAT)) %>%
  rename(Site = REEF_ID, 
         year = YEAR, 
         region = REGION_PS, 
         cover_mean = HARD_CORAL)

shm3 <- shm2 %>% select(-c(cover_sd:darkSpot)) %>%
  mutate(region = "Indo-Pacific")



head(datBruno2)
head(shm3)

# get USVI data
datUSVI <- read.csv("proc_ltreb_usvi.csv", 
                     stringsAsFactors = FALSE)





##' Calculate z-scores
shm4 <- shm3 %>% group_by(year) %>%
  mutate(year_mean = mean(cover_mean, na.rm = TRUE), 
         year_sd = mean(cover_sd, na.rm = TRUE), 
         zed = (cover_mean - year_mean)/year_sd) %>% ungroup()






