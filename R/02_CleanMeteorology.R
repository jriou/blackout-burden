
# Created 02.04.2025

# Clean the ERA5 meteorology data in Sri Lanka

#-------------------------------------------------------------------------------

# wd
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

# libraries
library(tidyverse)
library(terra)
library(tidyr)

# function to retrieve daily statistic
DailyStat <- function(start, stop, datenam, metric, stat, d = d){
  
  if(stat == "mean"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop] %>% apply(., 1, mean, na.rm = TRUE))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "min"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop] %>% apply(., 1, min, na.rm = TRUE))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "max"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop] %>% apply(., 1, max, na.rm = TRUE))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "sum"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop] %>% apply(., 1, sum, na.rm = TRUE))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  d_stat$date <- datenam
  
  return(d_stat)
}


metric_loop <- "2m_temperature" 
stat_loop <- "mean"

files2read <- list.files()[list.files() %>% startsWith(.,metric_loop)]

GetTemperature <- function(a,b){
  
  files2read_sub <- files2read[a:b]
  meteo_extract <- lapply(files2read_sub, terra::rast) 
  
  t_0 <- Sys.time()
  lapply(meteo_extract, function(X){
    d <- as.data.frame(X, xy=TRUE)
    d[,-c(1:2)] <- d[,-c(1:2)] - 273.15
    return(d)
  }) -> d_long
  t_1 <- Sys.time()
  t_1 - t_0 # 3 MINUTES
  
  
  for(i in 1:length(d_long)) print(sum(d_long[[1]][,1] - d_long[[i]][,1]))
  for(i in 1:length(d_long)) print(sum(d_long[[1]][,2] - d_long[[i]][,2]))
  
  # ok they are identical so we can put them together
  d_long_all <- d_long[[1]]
  for(i in 2:length(d_long)) d_long_all <- cbind(d_long_all, d_long[[i]][,-c(1:2)])
  
  d_long_all <- d_long_all[,!duplicated(colnames(d_long_all))]
  colnames(d_long_all) <- gsub("\\..*", "", colnames(d_long_all))
  colnames(d_long_all)[length(colnames(d_long_all))]
  
  data_long <- gather(d_long_all, date, t2m, colnames(d_long_all)[3]:colnames(d_long_all)[length(colnames(d_long_all))], factor_key=TRUE)
  
  t_0 <- Sys.time()
  date2 <- as.POSIXct(sub(".*=", "", data_long$date) %>% as.numeric(), origin = "1970-01-01")
  date2 <- format(date2, format='%Y-%m-%d', tz = "Europe/Lisbon")
  data_long$date2 <- date2
  t_1 <- Sys.time()
  t_1 - t_0 # 2 minutes
  
  
  data_long %>% 
    dplyr::group_by(x, y, date2) %>% 
    dplyr::summarise(temperature = mean(t2m)) -> dat_temperature_clean
  
  dat_temperature_clean %>% return()
  
}


length(files2read)


a_loop <- seq(from = 1, to = length(files2read), by = 5)
b_loop <- seq(from = 1, to = length(files2read), by = 5)-1
b_loop <- b_loop[-1]
b_loop <- c(b_loop, length(files2read))


# t_0 <- Sys.time()
# tmp <- GetTemperature(a = a_loop[1], b = b_loop[1])
# t_1 <- Sys.time()
# t_1 - t_0

t_0 <- Sys.time()
loop <- list()
for(i in 1:length(a_loop)) loop[[i]] <- GetTemperature(a = a_loop[i], b = b_loop[i])
t_1 <- Sys.time()
t_1 - t_0

dat.temperature <- do.call(rbind, loop)
saveRDS(dat.temperature, file = "CleanTemperature.rds")


rm(list = ls())
gc()



