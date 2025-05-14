
# Created 20.04.2025

# Download population

#-------------------------------------------------------------------------------

# install.packages("devtools")
# devtools::install_github("wpgp/wpgpDownloadR")

# wd
wd <- "C:/Users/gkonstan/OneDrive - Imperial College London/Desktop/Portugal/"
setwd(wd)

# load package
library(wpgpDownloadR)
library(tidyverse)
library(terra)

# set the country
# Check the ISO3 of the country
# wpgpDownloadR::wpgpDatasets %>% View()

iso3 <- "PRT"
# set the years
year <- 2018
cov <- paste0("ppp_", year)

f <- lapply(cov, function(X) wpgpGetCountryDataset(ISO3 = iso3, covariate = X, destDir = "output/") )
pop <- lapply(f, terra::rast)
terra::plot(pop[[1]])

pop_ag <- lapply(pop, terra::aggregate, fact = 90, fun = "sum", na.rm = TRUE)
terra::plot(pop_ag[[1]])
# fact is an aggregation factor expressed as number of cells in each direction 
# this ensure that the population and meteorology are more or less on the same dimension
# pop <- as.data.frame(pop, xy=TRUE)
# summary(pop)

saveRDS(pop_ag[[1]], file = paste0("output/population_", year))



rm(list = ls())
gc()


