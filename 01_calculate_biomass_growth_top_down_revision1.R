# Quick calculation of ERR for Kenya Reforestation Project

rm(list = ls())

source("./git_23834-ReforestationKenya/master.R")

# Parameters

# Location: 0.8411,34.4867
#project_area <- 1 # ha
project_area <- 60 # ha
#project_area <- 569 # ha

# Biomass values based on new literature ----
#  agb_max_stand:
#  Reduced max by 20%, as we are not sure this will really end up looking like a
#  natural forest (given some expected NTFP use etc).

#  agb_min_stand:
#  Min AGB (starting biomass level in baseline-scenario)

#  root_to_shoot:
#  Root biomass: standard value

# MAIN ----
em_n2o <- 0 # n2o emissions
pb <- 0    # baseline

# Leakage and uncertainty
# ldf <- 0 # leakage
# unc <- 0 # uncertainty
ldf <- 0.1 # leakage
unc <- 0.2 # uncertainty


# First estimate
# agb_max_stand <- 231 * 0.8
# agb_min_stand <- 10
# root_to_shoot <- 0.2
# yxmax <- 100
# kk <- 0.15
# version <- 1

# Updated estimate
agb_max_stand <- 281 * 0.8
agb_min_stand <- 7.34
root_to_shoot <- 0.2
yxmax <- 100
kk <- 0.16
version <- 2

# Setting
setting <- paste(
  pb, em_n2o, ldf, unc, agb_max_stand, agb_min_stand, root_to_shoot, yxmax, kk, project_area, sep = "_")

# Aboveground biomass growth over time
agb <- calc_biomass_growth_curve(
  x = iyears,           # project years
  xmax = yxmax,         # year of leveling off
  ymin = agb_min_stand, # starting value of y (e.g. AGB baseline)
  ymax = agb_max_stand, # Maximum AGB
  k = kk                # curvature parameter, the higher the faster max is approached
)

biomass_stand_df <- data.table(
  Year = iyears,
  agb = agb,
  bgb = agb * root_to_shoot,
  totb = agb + agb * root_to_shoot
)


# Plot biomass growth
p <- ggplot(biomass_stand_df, aes(x = Year, y = totb))
p <- p + geom_line()
#p <- p + ylim(c(0, max(biomass_stand_df$totb * 1.1)))
p <- p + geom_line(y = agb, color = "green")
#p <- p + geom_hline(yintercept = mean(biomass_stand_df$totb))
p <- p + xlab("Years from project start date") + ylab("Woody Biomass (tDM/ha)")
p <- p + annotate("label", x = 45, y = 100, label = "Aboveground biomass", color = "green")
p <- p + annotate("label", x = 15, y = 150, label = "Total biomass")
# p <- p + geom_point(x = 0, y = 4.5 * (1 - root_to_shoot) / biomass_to_c, color = "green") # Reference point PDD PlanVivo
# p <- p + geom_point(x = 25, y = 172 * (1 - root_to_shoot) / c_to_co2 / biomass_to_c, color = "green") # Reference point PDD PlanVivo
# p <- p + geom_point(x = 0, y = 5.1, color = "green") # Reference point Wheeler et al. (2016)
# p <- p + geom_point(x = 10, y = 9.5, color = "green") # Reference point Wheeler et al. (2016)
# p <- p + geom_point(x = 18, y = 40.6, color = "green") # Reference point Wheeler et al. (2016)
p <- p + geom_point(x = 0, y = 9.57 * (1 - root_to_shoot), color = "green") # Reference point PDD PlanVivo
p <- p + geom_point(x = 25, y = 99.79 * (1 - root_to_shoot), color = "green") # Reference point PDD PlanVivo
p <- p + geom_point(x = 0, y = 5.10 * (1 - root_to_shoot), color = "green") # Reference point PDD PlanVivo
p <- p + geom_point(x = 10, y = 9.50 * (1 - root_to_shoot), color = "green") # Reference point PDD PlanVivo
p <- p + geom_point(x = 18, y = 40.6 * (1 - root_to_shoot), color = "green") # Reference point PDD PlanVivo
p
fname <- paste0(results_dir, "Biomass_stand_tDM_ha_set_", setting, ".png")
ggsave(fname, p, width = 7, height = 4, units = ("in"))



# ERR calculation

# biomass change and conversion to co2
er_biomass <- diff(biomass_stand_df$totb) * biomass_to_c * c_to_co2
# ERR minus baseline, leakage, uncertainty
er_tot <- (er_biomass - em_n2o) * (1 - pb) * (1 - ldf) * (1 - unc)

er_stand_df <- data.table(
  Year           = iyears[-1],
  er_biomass     = er_biomass,
  er_tot_ha      = er_tot,
  er_cumsum      = cumsum(er_tot),
  er_tot_area    = er_tot * project_area,
  er_cumsum_area = cumsum(er_tot) * project_area
)

fname <- paste0(results_dir, "ERR_set_", setting, ".csv")
write.csv(er_stand_df, file = fname, row.names = FALSE)

# Plot ER per ha / year
p <- ggplot(er_stand_df, aes(x = Year, y = er_tot_ha))
p <- p + geom_line()
#p <- p + geom_hline(yintercept = mean(biomass_stand_df$totb))
p <- p + xlab("Years from project start date") + ylab("Emission Removals (tCO2/ha)")
p
fname <- paste0(results_dir, "ERR_tCO2_ha_year_set_", setting, ".png")
ggsave(fname, p, width = 7, height = 4, units = ("in"))

# # Total 30 years
# er_stand_df$er_cumsum_area[30]
# # Per ha
# er_stand_df$er_cumsum_area[30] / project_area
# # Per ha / y
# er_stand_df$er_cumsum_area[30] / project_area / 30
# 
# # Total 60 years
# er_stand_df$er_cumsum_area[60]
# # Per ha
# er_stand_df$er_cumsum_area[60] / project_area
# # Per ha / y
# er_stand_df$er_cumsum_area[60] / project_area / 60

er_summary_df_30 <- data.table(
  project_duration = 30,
  er_total         = er_stand_df$er_cumsum_area[30],
  er_ha_avg        = er_stand_df$er_cumsum_area[30] / project_area,
  er_ha_y_avg      = er_stand_df$er_cumsum_area[30] / project_area / 30,
  project_area     = project_area
)
er_summary_df_60 <- data.table(
  project_duration = 60,
  er_total         = er_stand_df$er_cumsum_area[60],
  er_ha_avg        = er_stand_df$er_cumsum_area[60] / project_area,
  er_ha_y_avg      = er_stand_df$er_cumsum_area[60] / project_area / 60,
  project_area     = project_area
)

er_summary_df <- rbind(er_summary_df_30, er_summary_df_60)
View(er_summary_df)

fname <- paste0(results_dir, "ERR_summary_set_", setting, "ha", ".csv")
write.csv(er_summary_df, file = fname, row.names = FALSE)

