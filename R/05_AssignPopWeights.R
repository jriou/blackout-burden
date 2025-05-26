
# Created 12.05.2025

# Combine meteorology with population.

#-------------------------------------------------------------------------------

library(tidyverse)
library(terra)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

cntr <- "PRT"
cntr <- "ESP"

t_0 <- Sys.time()
dat_meteo <- readRDS(paste0("output/CleanTemperature_", cntr, ".rds"))
dat_meteo$space_id <- paste0(dat_meteo$x, dat_meteo$y) %>% as.factor() %>% as.numeric()
dat_meteo$dates <- as.Date(dat_meteo$date2)
dat_meteo$wweek <- lubridate::week(dat_meteo$dates)
dat_meteo$yyear <- lubridate::year(dat_meteo$dates)
dat_meteo <- dat_meteo %>% dplyr::select(x, y, space_id, wweek, yyear, temperature, dates)
t_1 <- Sys.time()
t_1 - t_0

head(dat_meteo)
# apply(dat_meteo, 2, function(X) sum(is.na(X)))
dat_meteo <- dat_meteo %>% dplyr::rename(variable = temperature)
# the function

##
## The output of the meteo file should be:
#        x     y space_id wweek yyear variable dates     
# <dbl> <dbl>    <dbl> <int> <int>    <dbl> <date>    
# 1     78   8.4       91     1  2017     26.1 2017-01-02
# 2     78   8.4       91     1  2018     25.7 2018-01-01
# 3     78   8.4       91     1  2019     25.2 2018-12-31
# 4     78   8.4       91     1  2020     26.5 2019-12-30
# 5     78   8.4       91     1  2021     25.6 2020-12-28
# 6     78   8.4       91     1  2022     25.8 2021-12-27
# 7     78   8.4       91     1  2023     26.1 2023-01-02
# 8     78   8.4       91     1  2024     26.4 2024-01-01
# 9     78   8.4       91     1  2025     26.0 2024-12-30
# 10    78   8.4       91     2  2017     25.8 2017-01-09

##
## make sure that the meteo files have this format
##

meteo <- dat_meteo


GetPopulationWeights <- function(plot = F, year = NULL, pop, 
                                 store = TRUE, spain = F){
  
  # the idea here is that I will find the NN to the coarse space id and sum
  meteo_xy <- meteo[,c("x", "y", "space_id")]
  meteo_xy <- meteo_xy[!duplicated(meteo_xy$space_id),]
  
  meteo_xy_sp <- vect(meteo_xy[,c("x", "y")] %>% as.matrix(), crs="+proj=longlat +datum=WGS84")
  meteo_xy_sp$space_id <- meteo_xy$space_id
  
  extr.dt <- terra::extract(pop, meteo_xy_sp)
  if(spain == TRUE){
    meteo_xy$pop <- extr.dt$esp_ppp_2018
    cntr <- "ESP"
  }else{
    meteo_xy$pop <- extr.dt$prt_ppp_2018
    cntr <- "PRT"
  }
  
  
  if(plot == TRUE){
    print(ggplot() +
            geom_point(data = meteo_xy, aes(x=x, y=y, col = pop), size = 4) +
            theme_bw() + scale_color_viridis_c(na.value = "red"))
    
  }
  
  ##
  ## and bring back to meteorology
  meteo <- left_join(meteo, meteo_xy[,c("space_id", "pop")], by = c("space_id" = "space_id"))
  
  
  # and remove NAs
  meteo <- meteo[!is.na(meteo$pop),]
  
  if(store == TRUE){
    # and store
    saveRDS(meteo, file = paste0("output/PopweightedTemperature_", cntr, year, ".rds"))
  }
  
  return(meteo)
}


####
#### POPULATION WEIGHTING BASED ON A SINGLE YEAR.  

year <- 2018
pop_year <- readRDS(paste0("output/population_", cntr, "_", year))
t_0 <- Sys.time()
GetPopulationWeights(pop = pop_year, plot = TRUE, spain = TRUE)
t_1 <- Sys.time()
t_1 - t_0 # 5 seconds
  

rm(list = ls())
dev.off()
gc()






