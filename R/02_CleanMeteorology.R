
# Created 02.04.2025

# Clean the ERA5 meteorology data in Sri Lanka

#-------------------------------------------------------------------------------

# wd
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/output"
setwd(path)

country <- "PRT"
country <- "ESP"

# libraries
library(tidyverse)
library(terra)
library(tidyr)
library(parallel)

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

GetTemperature <- function(a, b, country){
  
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
  
  if(country == "PRT"){
    date2 <- format(date2, format='%Y-%m-%d', tz = "Europe/Lisbon")
  }
  
  if(country == "ESP"){
    date2 <- format(date2, format='%Y-%m-%d', tz = "Europe/Madrid")
  }
  data_long$date2 <- date2
  t_1 <- Sys.time()
  t_1 - t_0 # 2 minutes
  
  
  data_long %>% 
    dplyr::group_by(x, y, date2) %>% 
    dplyr::summarise(temperature = mean(t2m, na.rm = TRUE)) -> dat_temperature_clean
  
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

# t_0 <- Sys.time()
# loop <- list()
# for(i in 1:length(a_loop)) loop[[i]] <- GetTemperature(a = a_loop[i], b = b_loop[i])
# t_1 <- Sys.time()
# t_1 - t_0 # ~4 hours
# 
# dat.temperature <- do.call(rbind, loop)
# saveRDS(dat.temperature, file = "CleanTemperature.rds")

##
## RUN ON PARALLEL
# Set up parallel environment
k <- 1:length(a_loop)
par.fun <- function(k) GetTemperature(a = a_loop[k], b = b_loop[k], country = country)

# cores
ncores <- 20
cl_inla <- makeCluster(ncores, methods=FALSE)

# extract packages on parallel environment 
clusterEvalQ(cl_inla, {
  library(tidyverse)
  library(terra)
  library(tidyr)
})

# extract R objects on parallel environment
clusterExport(cl_inla, c("a_loop", "b_loop", "k", "DailyStat", "metric_loop", "stat_loop",
                         "files2read", "GetTemperature", "country"))

# run the the function in parallel
t_0 <- Sys.time()
outpar <- parLapply(cl = cl_inla, k, par.fun)
t_1 <- Sys.time()
t_1 - t_0

dat.temperature <- do.call(rbind, outpar)
saveRDS(dat.temperature, file = "CleanTemperature_", country, ".rds")

# close parallel environment
stopCluster(cl_inla)

rm(list = ls())
gc()



