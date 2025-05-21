
# Created 12.05.2025

# Aggregate on shapefile

#-------------------------------------------------------------------------------
      
library(tidyverse)
library(sf)
library(FNN)
library(patchwork)

# wd
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

cntr <- "ESP"
cntr <- "PRT"
##
## Function 


meteo <- readRDS(paste0("output/PopweightedTemperature_", cntr, ".rds"))
shp_file <- list.files(paste0(path, "/output/"), 
                       pattern = paste0("\\_", cntr, ".shp$"), full.names = TRUE)


shp_string <- shp_file

# read the shp
sf_use_s2(TRUE)
shp <- read_sf(shp_string)
  
sf::sf_use_s2(FALSE)
shp <- st_transform(shp, crs = 4326)  
if(cntr == "PRT"){
  shp$NAME <- shp$n2_L_20
}

if(cntr == "ESP"){
  shp$NAME <- shp$NAME_LATN
}

shp <- shp[complete.cases(shp$NAME),]
  
dat_points <- meteo[!duplicated(meteo$space_id),]
dat_points <- dat_points[,c("x", "y", "space_id")]
  
# Convert points to an sf object
points_sf <- st_as_sf(dat_points, coords = c("x", "y"), crs = st_crs(shp))
# and st_join them
point_in_polygon <- st_join(points_sf, shp, join = st_within)
  
# need to select the name of the level
dat_points$NAME <- point_in_polygon %>% 
    pull(NAME)
dat_points_tmp <- dat_points
  
dat_points_tmp <- dat_points_tmp[!is.na(dat_points_tmp$NAME),]
  
# if TRUE there are no NAs. 
# If its FALSE, there are NAs, we need to do more!!
print("First check: If false there are regions without meteorological estimates")
print((unique(dat_points_tmp$NAME) %>% length()) == nrow(shp))
  
  
# bring back to meteo
meteo_tmp <- left_join(meteo, dat_points_tmp[,c("space_id", "NAME")], by = c("space_id" = "space_id"))
  
# The NAs here could be the ones out of the shp
meteo_tmp <- meteo_tmp[!is.na(meteo_tmp$NAME),]
  
# and aggregate per shp
meteo_tmp %>% 
  dplyr::group_by(NAME, dates) %>% 
  dplyr::summarise(
    variable = sum(variable*pop)/sum(pop)
  ) -> meteo_weighted
  
  
##
## THIS NEEDS TO BE TRUE!! (CHECK)
print("Second check: needs to be TRUE")
print(
  (meteo_weighted$dates %>% unique() %>% length())*(meteo_weighted$NAME %>% unique() %>% length()) == nrow(meteo_weighted)
)
  

saveRDS(meteo_weighted, file = paste0("output/CleanPopWeightedTemperature", "_", cntr, ".rds"))

rm(list = ls())
dev.off()
gc()


