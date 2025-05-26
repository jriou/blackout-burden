
# Created 09.05.2025

# Get public holidays

#-------------------------------------------------------------------------------

library(httr)
library(jsonlite)
library(giscoR)
library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

# select country
country <- "PT"
country <- "ES"

years <- 2010:2025
pathURL <- paste0("https://date.nager.at/api/v3/publicholidays/", years, "/", country)

gEtBankHol <- function(X){
  bankHolidays <- fromJSON(X)
  bankHolidays %>% dplyr::select(date, name, counties) %>% 
    mutate(hol = 1) %>%  return()
}


for(i in 1:length(pathURL)){print(i); gEtBankHol(pathURL[i])}

lapply(pathURL, gEtBankHol) -> hol
hol <- do.call(rbind, hol)

if(country == "PT"){
  # need to remove the holidays in the islands not in Iberian peninsula
  hol$counties %>% unlist() %>% table()
  # these two are Madeira and Azores and need to be excluded
  hol$counties[sapply(hol$counties, is.null)] <- NA
  hol$counties <- unlist(hol$counties)
  hol <- hol[is.na(hol$counties),]
  hol$counties <- NULL
  cntr <- "PRT"
}

if(country == "ES"){
  
  # need to account for regional holidays and remove the islands not in iberian
  hol$counties[sapply(hol$counties, is.null)] <- "National"
  hol <- hol |> unnest(counties)

  # now i need to establish a link between the counties and nuts2 regions and iso used in nager
  iso_to_nuts2 <- data.frame(
    region_code = c("ES-AN", "ES-AR", "ES-AS", "ES-IB", "ES-CN", "ES-CM", "ES-CL", "ES-CT",
                    "ES-EX", "ES-GA", "ES-MD", "ES-MC", "ES-NC", "ES-PV", "ES-RI", "ES-VC", "ES-CE", "ES-ML"),
    NAME_LATN = c("Andalucía", "Aragón", "Principado de Asturias", "Illes Balears", "Canarias",
                  "Castilla-La Mancha", "Castilla y León", "Cataluña", "Extremadura", "Galicia",
                  "Comunidad de Madrid", "Región de Murcia", "Comunidad Foral de Navarra",
                  "País Vasco", "La Rioja", "Comunidad Valenciana", "Ceuta", "Melilla"),
    stringsAsFactors = FALSE
  )
  
  hol <- left_join(hol, iso_to_nuts2, by = c("counties" = "region_code"))
  hol$NAME_LATN[is.na(hol$NAME_LATN)] <- "National"
  cntr <- "ESP"
  
  # check that the names are the same
  # es_nuts2 <- gisco_get_nuts(year = 2003, nuts_level = 2, country = "ES", resolution = "03")
  # sum(!es_nuts2$NUTS_NAME %in% unique(hol$NAME_LATN))
  # unique(hol$NAME_LATN)[!unique(hol$NAME_LATN) %in% es_nuts2$NUTS_NAME]
  # # ok it is identical with the names of 2003.
}

saveRDS(hol, file = paste0("output/hol_", cntr, ".rds"))

rm(list = ls())
gc()

