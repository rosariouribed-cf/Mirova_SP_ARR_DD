# To explore land use change and biomass around the Mirova ARR project

# The basics: --------------------------------------------------------------
library(sf)
library(terra)
library(tidyverse)

setwd("/Users/mariauribe/Library/CloudStorage/OneDrive-ClimateFocus/Projects/2024/Mirova_ARR_DD/Data/")
ddir0 <- "0_rawdata/"
ddir1<- "1_processeddata/"

pa_fi <- read_sf(paste0(ddir0, "NatureRe Tech DD - Climate Focus/Project Area/Project_Area.kml"))
pa_fi <- vect(paste0(ddir1, "Project_Area.shp"))
plot(pa_fi)      

subreg_col <- vect(paste0(ddir0, "Subregiones_Provincias_de_Colombia/Subregiones_-_Provincias_de_Colombia.shp"))
ea <- subreg_col[subreg_col$NOM_SUBREG %in% c("BAJO CAUCA", "MAGDALENA MEDIO", "NORDESTE"),]
plot(ea)

# Land use (mapbiomas): ---------------------------------------------------------------

mapbiomas2022 <- rast(paste0(ddir0, "MAPBIOMAS-EXPORT/mapbiomas-colombia-collection-10-antioquia-2022.tif"))
plot(mapbiomas2022)
mapbiomas2018 <- rast(paste0(ddir0, "MAPBIOMAS-EXPORT/mapbiomas-colombia-collection-10-antioquia-2018.tif"))
plot(mapbiomas2018)
mapbiomas2015 <- rast(paste0(ddir0, "MAPBIOMAS-EXPORT/mapbiomas-colombia-collection-10-antioquia-2015.tif"))
plot(mapbiomas2015)
mapbiomas1985 <- rast(paste0(ddir0, "MAPBIOMAS-EXPORT/mapbiomas-colombia-collection-10-antioquia-1985.tif"))
plot(mapbiomas1985)

pa_fi_lc1985 <- terra::crop(mapbiomas1985, pa_fi)
pa_fi_lc1985 <- terra::mask(pa_fi_lc1985, pa_fi)
plot(pa_fi_lc1985, col = c("darkgreen", "lightgreen", "yellow"))

pa_fi_lc2022 <- terra::crop(mapbiomas2022, pa_fi)
pa_fi_lc2022 <- terra::mask(pa_fi_lc2022, pa_fi)
plot(pa_fi_lc2022, col = c("darkgreen", "yellow", "blue"))

par(mfrow = c(1,2))
plot(pa_fi_lc1985, col = c("darkgreen", "yellow"))
plot(pa_fi_lc2022, col = c("darkgreen", "yellow"))

# Look at changes in LC:
pa_fi_lc1985[pa_fi_lc1985 == 13] <- NA
pa_fi_lc2022[pa_fi_lc2022 == 33] <- NA
pa_fi_change <- pa_fi_lc2022 - pa_fi_lc1985
pa_fi_change[pa_fi_change == -18] <- 1
pa_fi_change[pa_fi_change == 18] <- -1
plot(pa_fi_change, col = c("brown", "gray", "green"))

# Calculate how much of the area has been a pasture since 1985:
pa_fi_pasture2022 <- pa_fi_lc2022
pa_fi_pasture2022[pa_fi_pasture2022 != 21] <- NA
pa_fi_pasture2022[pa_fi_pasture2022 == 21] <- 2
plot(pa_fi_pasture2022, col = "gold")
pa_fi_pasture1985 <- pa_fi_lc1985
pa_fi_pasture1985[pa_fi_pasture1985 != 21] <- NA
pa_fi_pasture1985[pa_fi_pasture1985 == 21] <- 1
plot(pa_fi_pasture1985, col = "gold")
pa_fi_pasture8522 <- pa_fi_pasture2022 - pa_fi_pasture1985
plot(pa_fi_pasture8522, col = "gold")

pa_fi_area <- pa_fi_lc1985
pa_fi_area[!is.na(pa_fi_area)] <- 0
plot(pa_fi_area)
pa_fi_pasture8522 <- sum(pa_fi_pasture8522, pa_fi_area, na.rm = TRUE)
plot(pa_fi_pasture8522, col = c("gray", "gold"))
# the numbers:
pa_fi_pasture_freq <- freq(pa_fi_pasture8522)
pa_area_ha <- expanse(pa_fi_pasture8522, unit = "ha")
pixel_size_ha <- unique(round(values(cellSize(pa_fi_pasture8522, unit = "ha"), na.rm = TRUE), digits = 3))[1,1]
pa_fi_pasture_df <- data.frame(pasture = pa_fi_pasture_freq[,2], 
                          pixel_count =  pa_fi_pasture_freq[,3]) %>%
  mutate(area = pixel_count * pixel_size_ha,
         area_prop = area/pa_area_ha*100)

## get the numbers:
hist(pa_fi_lc)
pa_fi_lc_freq <- freq(pa_fi_lc)
pa_area_ha <- expanse(pa_fi_lc, unit = "ha")
pixel_size_ha <- unique(round(values(cellSize(pa_fi_lc, unit = "ha"), na.rm = TRUE), digits = 3))[1,1]
pa_fi_lc_df <- data.frame(lc = pa_fi_lc_freq[,2], 
           pixel_count =  pa_fi_lc_freq[,3]) %>%
  mutate(area = pixel_count * pixel_size_ha,
         area_prop = area/pa_area_ha*100)

# Tree cover loss ---------------------------------------------------------

# Tree cover 2000:
treecover2000 <- rast(paste0(ddir0, "tree_cover/Hansen_GFC-2022-v1.10_treecover2000_10N_080W.tif"))
#plot(treecover2000)
pa_fi_tc2000 <- terra::crop(treecover2000, pa_fi)
pa_fi_tc2000 <- terra::mask(pa_fi_tc2000, pa_fi)
plot(pa_fi_tc2000)

# Tree cover gain between 2000-2022:
gain2022 <- rast(paste0(ddir0, "tree_cover/Hansen_GFC-2022-v1.10_gain_10N_080W.tif"))
#plot(treecover2000)
pa_fi_gain2022 <- terra::crop(gain2022, pa_fi)
pa_fi_gain2022 <- terra::mask(pa_fi_gain2022, pa_fi)
plot(pa_fi_gain2022)

# Tree cover loss between 2000-2022:
lossyear <- rast(paste0(ddir0, "tree_cover/Hansen_GFC-2022-v1.10_lossyear_10N_080W.tif"))
#plot(treecover2000)
pa_fi_lossyr <- terra::crop(lossyear, pa_fi)
pa_fi_lossyr <- terra::mask(pa_fi_lossyr, pa_fi)
plot(pa_fi_lossyr)

# Tree cover in 2022:
## Forest loss before 2000:
pa_fi_forestloss2000 <- pa_fi_tc2000
pa_fi_forestloss2000[pa_fi_forestloss2000 < 30] <- 0   # assumption of forest is cover >30%
pa_fi_forestloss2000[pa_fi_forestloss2000 >= 30] <- 1
plot(pa_fi_forestloss2000)
freq(pa_fi_forestloss2000)
#writeRaster(forestloss2000, filename = paste0(ddir0, "tree_cover/Hansen_GFC-2022-v1.10_forestlossbefore2000_10N_080W.tif"), overwrite=FALSE)
## Forest loss between 2000 and 2022:
pa_fi_lossmask <- pa_fi_lossyr
pa_fi_lossmask[pa_fi_lossmask != 0] <- 1
plot(pa_fi_lossmask)
pa_fi_tc2022 <- pa_fi_forestloss2000 - pa_fi_lossmask
pa_fi_tc2022[pa_fi_tc2022 < 0] <- 0
plot(pa_fi_tc2022)

pa_fi_tc2000 <- terra::crop(treecover2000, pa_fi)
pa_fi_tc2000 <- terra::mask(pa_fi_tc2000, pa_fi)
plot(pa_fi_tc2000)

hist(pa_fi_tc2000)  
freq(pa_fi_gain2022)  
pa_fi_lossyr_freq <- freq(pa_fi_lossyr)  
plot(x = pa_fi_lossyr_freq[2:19,2], y = pa_fi_lossyr_freq[2:19,3], type = "l")

# Compare 
par(mfrow = c(1,2))
plot(pa_fi_tc2022, col = c("yellow", "darkgreen"))
plot(pa_fi_lc2022, col = c("darkgreen", "yellow"))


# Forest regrowth expansion area (Mapbiomas): -----------------------------------------

ea_lc1985 <- terra::crop(mapbiomas1985, ea)
ea_lc1985 <- terra::mask(ea_lc1985, ea)
ea_lc1985[ea_lc1985 %in% c(1,3,5,6,49)] <- 1
ea_lc1985[ea_lc1985 %in% c(10,11,12,13,32,29,50)] <- 10
ea_lc1985[ea_lc1985 %in% c(26,33,31,34)] <- 26
ea_lc1985[ea_lc1985 == 27] <- 27
ea_lc1985[ea_lc1985 %in% c(14,9,35,21,22,23,24,25,30)] <- 14
plot(ea_lc1985)  # , col = c("darkgreen", "lightgreen", "yellow")

ea_lc2015 <- terra::crop(mapbiomas2015, ea)
ea_lc2015 <- terra::mask(ea_lc2015, ea)
ea_lc2015[ea_lc2015 %in% c(1,3,5,6,49)] <- 1
ea_lc2015[ea_lc2015 %in% c(10,11,12,13,32,29,50)] <- 10
ea_lc2015[ea_lc2015 %in% c(26,33,31,34)] <- 26
ea_lc2015[ea_lc2015 == 27] <- 27
ea_lc2015[ea_lc2015 %in% c(14,9,35,21,22,23,24,25,30)] <- 14
plot(ea_lc2015)  # , col = c("darkgreen", "lightgreen", "yellow")

ea_lc2022 <- terra::crop(mapbiomas2022, ea)
ea_lc2022 <- terra::mask(ea_lc2022, ea)
ea_lc2022[ea_lc2022 %in% c(1,3,5,6,49)] <- 1
ea_lc2022[ea_lc2022 %in% c(10,11,12,13,32,29,50)] <- 10
ea_lc2022[ea_lc2022 %in% c(26,33,31,34)] <- 26
ea_lc2022[ea_lc2022 == 27] <- 27
ea_lc2022[ea_lc2022 %in% c(14,9,35,21,22,23,24,25,30)] <- 14
plot(ea_lc2022)

## Make irrelevant land uses 0, forest 1, and ag 2:
ea_lc1985[ea_lc1985 %in% c(10,26,27)] <- 0
ea_lc2015[ea_lc2015 %in% c(10,26,27)] <- 0
ea_lc2022[ea_lc2022 %in% c(10,26,27)] <- 0
ea_lc1985[ea_lc1985 == 14] <- 2
ea_lc2015[ea_lc2015 == 14] <- 2
ea_lc2022[ea_lc2022 == 14] <- 2
plot(ea_lc1985) 
plot(ea_lc2015)
plot(ea_lc2022)

# the change:
ea_lcc8522 <- ea_lc2022-ea_lc1985
ea_lcc1522 <- ea_lc2022-ea_lc2015
ea_lcc8522[ea_lcc8522 == -2] <- 0
ea_lcc8522[ea_lcc8522 == 2] <- 0
ea_lcc1522[ea_lcc1522 == -2] <- 0
ea_lcc1522[ea_lcc1522 == 2] <- 0
plot(ea_lcc8522)
freq(ea_lcc8522)
freq(ea_lcc1522)
# in a table:
ea_lc_freq8522 <- freq(ea_lcc8522)
ea_lc_freq1522 <- freq(ea_lcc1522)
ea_area_ha <- sum(expanse(ea, unit = "ha"))
pixel_size_ha <- unique(round(values(cellSize(ea_lcc8522, unit = "ha"), na.rm = TRUE), digits = 3))[1,1]
ea_lc_df <- data.frame(lcc = ea_lc_freq8522[,2], 
                          pixel_count_8522 =  ea_lc_freq8522[,3],
                          pixel_count_1522 =  ea_lc_freq1522[,3]) %>%
  mutate(area8522 = pixel_count_8522 * pixel_size_ha, area1522 = pixel_count_1522 * pixel_size_ha,
         area_prop8522 = area8522/ea_area_ha*100, area_prop1522 = area1522/ea_area_ha*100)

# Forest regrowth expansion area (Tree cover): -----------------------------------------

# Tree cover gain between 2000-2022:
gain2022 <- rast(paste0(ddir0, "tree_cover/Hansen_GFC-2022-v1.10_gain_10N_080W.tif"))
#plot(treecover2000)
ea_gain2022 <- terra::crop(gain2022, ea)
ea_gain2022 <- terra::mask(ea_gain2022, ea)
plot(ea_gain2022)
freq(ea_gain2022)


