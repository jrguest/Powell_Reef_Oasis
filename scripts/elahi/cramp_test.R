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

##### GET LONG-TERM TRENDS #####
head(dat)
summary(dat)

library(tidyr)
library(broom)

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

# Attach to means

grand_means3 <- left_join(grand_means2, site_lm_fits, by = "Site") %>%
  mutate(lm_sig = ifelse(p.value < 0.05, "sig", "ns"), 
         lm_dir = ifelse(estimate < 0, "negative", "positive"))

grand_means3 %>% 
  ggplot(aes(estimate, fill = lm_sig)) + 
  geom_histogram(bins = 30)



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
