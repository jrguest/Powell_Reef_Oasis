################################################################################
##' @title Summarise simulated time series data
##'
##' @author Robin Elahi
##' @contact elahi.robin@gmail.com
##' @date 2017-03-17
##' 
##' @log Add a log here
################################################################################

rm(list=ls(all=TRUE))

source("scripts/elahi/coral_sims/2_summarise_data_original.R")
library(RColorBrewer)
library(gridExtra)
library(grid)
theme_set(theme_bw(base_size = 18))

plot_dat <- grand_means4
levels(plot_dat$scenario)

## Get colors for scenarios
scenarios <- unique(plot_dat$scenario)
#cb_pal_scenario <- c("#D55E00", "#E69F00", "#56B4E9", "#009E73")
cb_pal_scenario <- c("#009E73", "#56B4E9", "#D55E00", "#E69F00")

names(cb_pal_scenario) <- scenarios
col_scale_scenario <- scale_colour_manual(name = "Model Scenario", values = cb_pal_scenario)
fill_scale_scenario <- scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario)

##### OASIS Z PLOT #####

plot_dat %>% 
  ggplot(aes(med_z, cv, fill = scenario, shape = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray50", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray50") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray50") +
  geom_point(alpha = 0.75, size = 4) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25), 
        legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  scale_shape_manual(name = "Model Scenario", values = c(21,22,23,25))

#ggsave("scripts/elahi/coral_sims/figs_sims/oasis_z_plot.pdf")

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_z_plot.pdf", 
       height = 7, width = 10)

##### PICK REPRESENTATIVE SITES #####

##' Escape reef (stable positive)
##' Rebound reef (phase shift)
##' Crap reef (stable negative)
##' (non-linear decline)

plot_dat %>% 
  ggplot(aes(med_z, cv, color = scenario)) + 
  xlab("Median z-score of coral cover") + 
  ylab("Temporal variability in coral cover (CV)") + 
  annotate("rect", xmin = -2, xmax = -1, ymin = 100, ymax = 150,
           alpha = 0, color = "gray", linetype = "solid") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -2, xmax = -1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -0, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 50, ymax = 100,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 1, xmax = 2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = 0, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  annotate("rect", xmin = -1, xmax = -2, ymin = 0, ymax = 50,
           alpha = 0, color = "gray") +
  geom_point(alpha = 0.75) + 
  theme(legend.position = c(1,1), legend.justification = c(1.25, 1.25)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name = "Model Scenario", values = cb_pal_scenario) + 
  geom_text(data = plot_dat,  
            aes(label = sim_total), size = 2.5, check_overlap = FALSE, 
            vjust = 0, nudge_y = 3)

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_z_plot_boxes.png")

## Get time-series for each box
box1 = c(229, 121, 395)
box2 = c(226, 135, 384)
box3 = c(113, 373)
box4 = c(124, 302)
box5 = c(5, 246, 176, 326)
box6 = c(50, 201, 116, 342)
box7 = c(78, 237, 195, 324)
box8 = c(92, 216, 137, 314)

## Get needed items for the loop
box_list <- list(box1, box2, box3, box4, 
                 box5, box6, box7, box8)

box_names <- c("box1", "box2", "box3", "box4", 
               "box5", "box6", "box7", "box8")

box_names_length <- length(box_list)

i = 4
box_i <- box_list[[i]]
box_name_i <- box_names[i]
box_i_df <- sim_df3[sim_df3$sim_total %in% box_i, ]
box_i_df$box <- box_name_i

box_df <- box_i_df
box_results <- vector("list", box_names_length)

box_df %>% 
  ggplot(aes(year, y)) + 
  ylab("") + xlab("") + 
  scale_y_continuous(limits = c(0, 60)) + 
  geom_line(aes(year, y_mean), alpha = 0.75, 
            data = mean_cover_df, color = "black", linetype = "dashed") + 
  geom_line(alpha = 0.8, aes(group = sim2, color = scenario), 
            size = 1.25) + 
  col_scale_scenario + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  facet_wrap(~ sim_total)



for(i in 1:box_names_length){
  
  box_i <- box_list[[i]]
  box_name_i <- box_names[i]
  box_i_df <- sim_df3[sim_df3$sim_total %in% box_i, ]
  box_i_df$box <- box_name_i
  
  plot_box <- box_i_df %>% 
    ggplot(aes(year, y)) + 
    ylab("") + xlab("") + 
    scale_y_continuous(limits = c(0, 60)) + 
    theme(legend.position = "none") + 
    geom_line(aes(year, y_mean), alpha = 1, size = 1, 
              data = mean_cover_df, color = "black", linetype = "dashed") + 
    geom_line(alpha = 0.8, aes(group = sim2, color = scenario), 
              size = 1.25) + 
    col_scale_scenario + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  box_results[[i]] <- plot_box
}

box_results[[1]]

##### USE GRID ARRANGE TO CREATE PANEL B #####

hlay <- rbind(c(1,NA,NA,NA),
              c(2,3,4,NA),
              c(5,6,7,8))

pdf("scripts/elahi/coral_sims/figs_sims/box_time_series_panels.pdf", 
    height = 7, width = 10)
grid.arrange(grobs = box_results, layout_matrix = hlay)
dev.off()



##### ILLUSTRATING THE METHOD #####

sim_df3 <- sim_df3 %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("Stable", "Linear", 
                                      "Phase_shift", "Oscillations")))

sim_df3 %>% 
  filter(sim < 6) %>% 
  ggplot(aes(year, y, color = scenario)) + 
  theme_bw(base_size = 16) + 
  ylab("Coral cover (%)") + xlab("Year") + 
  scale_y_continuous(limits = c(0, 65)) + 
  theme(legend.position = "none") + 
  geom_line(alpha = 0.9, aes(group = sim, color = scenario), 
            size = 1.25) + 
  facet_wrap(~ scenario) + 
  col_scale_scenario + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(strip.background = element_blank())

ggsave("scripts/elahi/coral_sims/figs_sims/scenario_examples.png")


##### OLD CRAP #####
##### OLD CRAP #####
##### OLD CRAP #####

## DECLINE (LINEAR)
linear_decline <- sim_df3 %>% 
  filter(med_z < -1) %>% 
  filter(cv > 100 & cv < 150)
unique(linear_decline$sim2)

linear_decline %>% 
  ggplot(aes(year, y, color = sim2)) + 
  geom_hline(yintercept = mean_cover, color = "gray", linetype = "dashed") + 
  geom_line(alpha = 0.5) + 
  ylab("Coral cover (%)") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none")

## REBOUND
rebound <- sim_df3 %>% 
  filter(med_z > 0 & med_z < 1) %>% 
  filter(cv > 50 & cv < 100) %>% 
  filter(scenario == "Oscillations")

rebound %>% 
  ggplot(aes(year, y, color = sim2)) + 
  geom_hline(yintercept = mean_cover, color = "gray", linetype = "dashed") + 
  geom_line(alpha = 0.5) + 
  ylab("Coral cover (%)") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none")

## RESTORATION
restoration <- sim_df3 %>% 
  filter(med_z > 0 & med_z < 1) %>% 
  filter(cv > 50 & cv < 100) %>% 
  filter(scenario == "Phase_shift")

restoration %>% 
  ggplot(aes(year, y, color = sim2)) + 
  geom_hline(yintercept = mean_cover, color = "gray", linetype = "dashed") + 
  geom_line(alpha = 0.5) + 
  ylab("Coral cover (%)") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none") + 
  geom_point()

## STABLE HIGH
stable_high <- sim_df3 %>% 
  filter(med_z > 1 & med_z < 2) %>% 
  filter(cv > 0 & cv < 50) %>% 
  filter(scenario == "Stable")

stable_high %>% 
  ggplot(aes(year, y, color = sim2)) + 
  geom_hline(yintercept = mean_cover, color = "gray", linetype = "dashed") + 
  geom_line(alpha = 0.5) + 
  ylab("Coral cover (%)") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none")

## STABLE LOW
stable_low <- sim_df3 %>% 
  filter(med_z > -2 & med_z < -1) %>% 
  filter(cv > 0 & cv < 50) %>% 
  filter(scenario == "Stable")

stable_low %>% 
  ggplot(aes(year, y, color = sim2)) + 
  geom_hline(yintercept = mean_cover, color = "gray", linetype = "dashed") + 
  geom_line(alpha = 0.5) + 
  ylab("Coral cover (%)") + 
  scale_y_continuous(limits = c(0, 60)) + 
  theme(legend.position = "none")


##### OASIS % PLOTS #####

plot_dat %>% 
  ggplot(aes(mean_z, percent_above, color = scenario)) + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Proportion of years above mean") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(alpha = 0.5) 

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_percent_plot.png")

plot_dat %>% 
  ggplot(aes(mean_z, percent_above, color = scenario)) + 
  xlab("Long-term mean coral cover (z-score)") + 
  ylab("Proportion of years above mean") + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") + 
  geom_point(alpha = 0.5) + 
  facet_wrap(~ scenario)

ggsave("scripts/elahi/coral_sims/figs_sims/oasis_percent_plot_facet.png")

