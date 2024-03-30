# Created by MRU on Februrary 2024, based on SM
# Script to find the best model for the Mirova - South Pole ARR project in Colombia

library(data.table)
library(ggplot2)
library(tidyverse)
library(growthmodels)

# Import functions
sapply("./calc_biomass_growth_curve.R",
  FUN = source
)

# Basic Parameters:
iyears <- 0:40
root_to_shoot <- 0.42

# The real data:
agb_porce <- read.table("/Users/mariauribe/Desktop/safe_copies/GitHub/MirovaARRDD/AGB_data/agb_literature.txt",
                        header = TRUE) %>%
  rename("Year" = Age, "agb" = AGB) %>%
  mutate(totb = agb + agb*root_to_shoot)

test_lm <- lm(agb ~ Year, data = agb_porce)
summary(test_lm)
test_lm$coefficients

# Growth curve parameters:
agb_max_stand <- 240
agb_min_stand <- 4.47
yxmax <- 80
kk <- 0.128

# Aboveground biomass growth over time:
agb <- calc_biomass_growth_curve(
  x = 0:100,           # project years
  xmax = yxmax,         # year of leveling off
  ymin = agb_min_stand, # starting value of y (e.g. AGB baseline)
  ymax = agb_max_stand, # Maximum AGB
  k = kk                # curvature parameter, the higher the faster max is approached
)
# AGB+BGB:
biomass_stand_log <- data.table(
  Year = 0:100,
  agb = agb,
  bgb = agb * root_to_shoot,
  totb = agb + agb * root_to_shoot
)

# Exponential growth:
# agb_exp <- calc_biomass_expgrowth_curve(
#   x = iyears,
#   a = agb_min_stand,
#   r = 0.095
# )
# 
# biomass_stand_exp <- data.table(
#   Year = iyears,
#   agb_exp = agb_exp,
#   bgb_exp = agb_exp * root_to_shoot,
#   totb_exp = agb_exp + agb_exp * root_to_shoot
# )

agb_von <- calc_biomass_vonbert(
  x = 0:100, 
  y0 = agb_min_stand,
  ymax = agb_max_stand,
  b0 = 0,
  b1 = 0.04,
  b2 = 3.1)

biomass_stand_von <- data.table(
    Year = 0:100,
    agb_von = agb_von,
    bgb_von = agb_von * root_to_shoot,
    totb_von = agb_von + agb_von * root_to_shoot
  )

## parameter optimization:
params = c(lambda=1, k=1)

# Linear growth:
agb_lin <- calc_biomass_lineargrowth(
  x = iyears,
  a = agb_min_stand,
  m = 5.46
)
biomass_stand_lin <- data.table(
  Year = iyears,
  agb_lin = agb_lin,
  bgb_lin = agb_lin * root_to_shoot,
  totb_lin = agb_lin + agb_lin * root_to_shoot
)

# Plot biomass growth:
p <- ggplot() +
  theme_bw()
p <- p + geom_line(data = biomass_stand_log, aes(x = Year, y = agb), color = "darkgreen")
p <- p + geom_line(data = biomass_stand_von, aes(x = Year, y = agb_von), color = "darkred")
#p <- p + geom_line(data = biomass_stand_lin, aes(x = Year, y = agb_lin), color = "black", linetype = "longdash")
#p <- p + ylim(c(0, max(biomass_stand_df$totb * 1.1)))
p <- p + geom_point(data = agb_porce, aes(x = Year, y = agb), color = "black", shape = 18, size = 2)
#p <- p + geom_hline(yintercept = mean(biomass_stand_df$totb))
p <- p + xlab("Years from project start date") + ylab("AGB (tDM/ha)")
p
#ggsave(filename = "/Users/rosariouribe_climatefocus/Library/CloudStorage/OneDrive-ClimateFocus/Projects/2024/Mirova_ARR_DD/Analyses/AGB_byage_logmax_disct15.png",
#       scale = 0.9)

# Compare the models:
all_agb <- agb_porce %>%
  rename(agb_obs = agb) %>% 
  left_join(biomass_stand_log, by = "Year") %>%
  left_join(biomass_stand_von, by = "Year") %>%
  #left_join(biomass_stand_lin, by = "Year") %>%
  select(Year, agb_obs, agb, agb_von) %>%   # agb_exp, agb_lin
  rename(agb_log = agb)

# R-square:
agblog_lm <- lm(agb_obs ~ agb_log, data = all_agb)
summary(agblog_lm)$r.squared
agbexp_von <- lm(agb_obs ~ agb_von, data = all_agb)
summary(agbexp_von)$r.squared
# agblin_lm <- lm(agb_obs ~ agb_lin, data = all_agb)
# summary(agblin_lm)$r.squared

rmse <- function(actual, predicted){
  return(sqrt(mean((actual - predicted)^2)))}

rmse(all_agb$agb_obs, all_agb$agb_log)
rmse(all_agb$agb_obs, all_agb$agb_von)
#rmse(all_agb$agb_obs, all_agb$agb_lin)

# Export data from exponential model to use it to estimate ERR:
biomass_stand_log <- biomass_stand_log %>%
  mutate(totCO2ha = totb * 0.47 * (44/12)) %>%
  mutate(across(2:5, \(x) round(x, digits = 4)))
biomass_stand_log
write_csv(biomass_stand_log, 
          file = "/Users/rosariouribe_climatefocus/Library/CloudStorage/OneDrive-ClimateFocus/Projects/2024/Mirova_ARR_DD/Data/1_processeddata/predict_ctree_naturere_agbmax300_yxmax80_kk0128.csv")


