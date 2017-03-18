################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

#rm(list=ls(all=TRUE))

source("scripts/elahi/coral_sims/2_summarise_data.R")

sim_df2
grand_means3

##### BOX PLOTS #####

grand_means3 %>% 
  ggplot(aes(scenario, mean)) + 
  geom_boxplot()

grand_means3 %>% 
  ggplot(aes(scenario, cv)) + 
  geom_boxplot()

##### FREQUENCY PLOTS #####

grand_means3 %>% 
  ggplot(aes(mean_z)) + 
  geom_histogram(binwidth = 0.1) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray") + 
  xlab("Coral cover (z-score)")

grand_means3 %>% 
  ggplot(aes(cv_z)) + 
  geom_histogram(binwidth = 0.1) + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray") + 
  xlab("Coral cover variability (z-score)")

##### OASIS Z PLOTS #####

grand_means3 %>% 
  #filter(cv_z < 8) %>% 
  ggplot(aes(mean_z, cv_z, color = scenario)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black") + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Temporal variability in coral cover (z-score)") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") + 
  geom_point(alpha = 0.7) 

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_z_plot.png")


grand_means3 %>% 
  ggplot(aes(mean_z, cv_z, color = lm_sig, shape = lm_dir)) + 
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + 
  geom_vline(xintercept = 0, linetype = "solid", color = "black") + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Temporal variability in coral cover (z-score)") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") + 
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") + 
  geom_point() + 
  facet_wrap(~ scenario)

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_z_plot_facet.png")

##### TIME SERIES PLOTS #####

sim_df3 <- sim_df2 %>% arrange(sim, scenario, year)

sim_df3 %>% 
  slice(1:1200) %>% 
  ggplot(aes(year, y, color = oasis)) + 
  geom_line(alpha = 1) + #geom_point(alpha = 0.5) + 
  geom_hline(yintercept = mean_cover, color = "black", linetype = "dashed") + 
  facet_grid(scenario ~ sim) + 
  ylab("Coral cover (%)") + 
  theme(legend.position = "top")

ggsave("scripts/elahi/coral_sims/figs_sims/scenario_time_series.png")


