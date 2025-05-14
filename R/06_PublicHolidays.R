
# Created 09.05.2025

# Get public holidays

#-------------------------------------------------------------------------------


library(jsonlite)
library(tidyverse)

path <- "C:/Users/gkonstan/OneDrive - Imperial College London/Desktop/Portugal/"
setwd(path)

# select country
country <- "PT"

years <- 2010:2025
pathURL <- paste0("https://date.nager.at/api/v3/publicholidays/", years, "/", country)

gEtBankHol <- function(X){
  bankHolidays <- fromJSON(X)
  bankHolidays %>% dplyr::select(date, name) %>% 
    mutate(hol = 1) %>%  return()
}


for(i in 1:length(pathURL)){print(i); gEtBankHol(pathURL[i])}

lapply(pathURL, gatBankHol) -> hol
hol <- do.call(rbind, hol)

saveRDS(hol, file = "output/hol.rds")


rm(list = ls())
gc()