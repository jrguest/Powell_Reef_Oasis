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
library(tidyr)
library(broom)

dat <- read.csv("data_derived/CRAMP_MHI_corcov.csv", 
                stringsAsFactors = FALSE)
head(dat)
summary(dat)
str(dat)
glimpse(dat)

dat %>% 
  ggplot(aes(Year, Stony_coral, color = Site)) + 
  geom_line(alpha = 0.5) + 
  facet_wrap(~ Island) + 
  theme(legend.position = "none") + 
  geom_smooth(aes(color = NULL))

##### GET LONG-TERM SITE LEVEL MEANS #####

grand_means <- dat %>% group_by(Island, Site) %>% 
  summarise(mean = mean(Stony_coral), 
            sd = sd(Stony_coral), 
            n = n(), 
            cv = sd/mean, 
            stability = 1/cv) %>% ungroup()
summary(grand_means)

grand_means2 <- grand_means %>% 
  mutate(mean_z = scale(mean)[,1], 
         cv_z = scale(cv)[,1])
glimpse(grand_means2)
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

##### GET LONG-TERM TRENDS #####
head(dat)
summary(dat)

# Function to calculate linear trend
get_lin_fit <- function(dat, offset = 0) {
  the_fit <- lm(Stony_coral ~ I(Year - offset), dat)
  the_fit_slope <- tidy(the_fit)[2, c(2,5)]
  return(the_fit_slope)
}

get_lin_fit(dat)

# Run per site
site_lm_fits <- dat %>%
  group_by(Site) %>% 
  do(get_lin_fit(.))
site_lm_fits
summary(site_lm_fits)

# Attach to means

grand_means3 <- left_join(grand_means2, site_lm_fits, by = "Site") %>%
  mutate(lm_sig = ifelse(p.value < 0.05, "sig", "ns"), 
         lm_dir = ifelse(estimate < 0, "negative", "positive"), 
         oasis = ifelse(cv_z > 1 & mean_z > 0, "oasis_variable", 
                        ifelse(cv_z < 1 & mean_z > 1, "oasis_stable", 
                               "not_oasis")))

grand_means3
summary(grand_means2)
glimpse(grand_means3)


## Attach oasis results to raw data

dat2 <- grand_means3 %>% select(Site, mean_z, lm_sig:oasis) %>% 
  left_join(dat, ., by = "Site") %>% 
  mutate(Site2 = reorder(Site, desc(mean_z)))
names(dat2)
glimpse(dat2)
summary(dat2)
str(dat2)

mean_cover = mean(dat2$Stony_coral)

dat2 %>% 
  filter(oasis != "not_oasis") %>% 
  ggplot(aes(Year, Stony_coral, color = lm_sig)) + 
  geom_line(alpha = 0.5) + geom_point() + 
  facet_wrap(oasis ~ Site2) + 
  geom_hline(yintercept = mean_cover, color = "black", linetype = "dashed")

ggsave("figures/cramp_var_vs_mean_2ts.png")

##### GET A SURFACE TO DEFINE OASES #####
grand_means2


df_surface <- data.frame(mean_z = c(seq(0, 1, by = 0.1)), 
                         cv_z = c(seq(1, 0, by = -0.1)))


grand_means2 %>% 
  ggplot(aes(mean_z, cv_z, color = Island)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") + 
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Temporal variability in coral cover (z-score)") + 
  scale_x_continuous(limits = c(-4, 4)) + 
  scale_y_continuous(limits = c(-4, 4)) + 
  geom_point() + 
  geom_line(aes(color = NULL), data = df_surface) + 
  geom_smooth(aes(color = NULL), data = df_surface, method = "lm", fullrange = TRUE)


grand_means3 %>% 
  ggplot(aes(mean_z, cv_z, color = lm_sig, shape = lm_dir)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "gray") + 
  geom_vline(xintercept = 0, linetype = "solid", color = "gray") + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Temporal variability in coral cover (z-score)") + 
  annotate("rect", color = "red", fill = "red", alpha = 0.1, 
                        xmin = 0, xmax = Inf,
                        ymin = 1, ymax = Inf) + 
  annotate("rect", color = "blue", fill = "blue", alpha = 0.1, 
           xmin = 1, xmax = Inf,
           ymin = -Inf, ymax = 1) +
  annotate("text", x = 3, y = -0.5, label = "Stable\noasis") + 
  annotate("text", x = 3, y = 1.5, label = "Variable\noasis") + 
  geom_point()

ggsave("figures/cramp_var_vs_mean_2.png", height = 5, width = 5)
