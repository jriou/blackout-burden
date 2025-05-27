
# Created 13.05.2025

# Bring everything together

#-------------------------------------------------------------------------------

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)


library(tidyverse)
library(giscoR)

cntr <- "PRT"
cntr <- "ESP"

if(cntr == "PRT"){
  ##
  ## and after bringing together 
  
  dat.grid <- readRDS(paste0("output/OutcomeData_", cntr, ".rds"))
  pop_daily <- readRDS(paste0("output/pop_nutsii_lp_", cntr, ".rds"))
  
  head(dat.grid)
  head(pop_daily)
  
  dat.grid$sex <- ifelse(dat.grid$sex == "F", "female", "male") 
  dat.fin <- left_join(pop_daily, dat.grid, 
                       by = c("date" = "date", 
                              "nuts2_nameLatin_2024" = "NUTSII_name", 
                              "sex" = "sex", 
                              "age" = "agelg"))
  
  ##
  ## merge age groups
  dat.fin$age %>% table()
  dat.fin$age[dat.fin$age %in% c("65-69", "70-74", "75-79", "80-84")] <- "65-84"
  dat.fin %>% 
    dplyr::group_by(date, nuts2_nameLatin_2024, age, sex) %>% 
    summarise(pop=sum(pop), 
              deaths = sum(deaths)) -> dat.fin
  
  
  # bring temperature
  temperature <- readRDS(paste0("output/CleanPopWeightedTemperature_", cntr, ".rds"))
  temperature$NAME[temperature$NAME %in% "Centro (PT)"] <- "Centro"
  dat.fin <- left_join(dat.fin, temperature, 
                       by = c("nuts2_nameLatin_2024" = "NAME", 
                              "date" = "dates"))
  
  summary(dat.fin)
  
  # bring public holidays
  hol <- readRDS(paste0("output/hol_", cntr, ".rds"))
  
  head(hol)
  hol$date <- as.Date(hol$date)
  dat.fin <- left_join(dat.fin, hol, 
                       by = c("date" = "date"))
  
  dat.fin$name <- NULL
  dat.fin$hol <-ifelse(is.na(dat.fin$hol), 0, 1)
  
  
  ##
  ## Some checks
  
  # population totals:
  dat.fin$year <- lubridate::year(dat.fin$date)
  dat.fin$pop[dat.fin$date %in% "2010-01-01"] %>% sum()
  dat.fin$pop[dat.fin$date %in% "2020-01-01"] %>% sum()
  dat.fin$pop[dat.fin$date %in% "2025-01-01"] %>% sum()
  
  # temperature
  dat.fin %>% 
    filter(sex %in% "male", age %in% "0-64", 
           nuts2_nameLatin_2024 %in% "Alentejo") %>% 
    ggplot(aes(x=date, y=variable)) +  
    geom_point()
  
  # they look correct. 
  
  # rename and store
  dat.fin %>% 
    dplyr::rename(NUTSII = nuts2_nameLatin_2024, 
                  temperature_lag0 = variable) -> dat.fin
  
  # define ids for temporal and spatial trends
  dat.fin$month <- lubridate::month(dat.fin$date)
  dat.fin$yweek <- lubridate::week(dat.fin$date)
  dat.fin$yday <- lubridate::yday(dat.fin$date)
  dat.fin$day <- as.numeric(as.factor(dat.fin$date))
  dat.fin$week <- INLA::inla.group(dat.fin$day, 
                                   n = max(dat.fin$day)/7 %>% round(digits = 0),
                                   idx.only =  TRUE
  )
  
  dat.fin$id.space <- as.numeric(as.factor(dat.fin$NUTSII))
  
  # and bring also covid_19 deaths
  covid_deaths <- readRDS("output/COVID_DEATHS.rds")
  dat.fin$isoweek <- substr(ISOweek::date2ISOweek(dat.fin$date),1,8)
  
  covid_deaths %>% 
    dplyr::filter(country %in% "Portugal") %>% 
    dplyr::select(isoweek, OWID_covid_deaths) %>% 
    left_join(dat.fin, .) -> dat.fin
  
  summary(dat.fin)
  dat.fin$OWID_covid_deaths[is.na(dat.fin$OWID_covid_deaths)] <- 0
  
  saveRDS(dat.fin, file = paste0("output/FinalData_", cntr, ".rds"))
}

if(cntr == "ESP"){
  
  dat.grid <- readRDS(paste0("output/OutcomeData_", cntr, ".rds"))
  pop_daily <- readRDS(paste0("output/pop_nutsii_lp_", cntr, ".rds"))
  
  # bring temperature
  temperature <- readRDS(paste0("output/CleanPopWeightedTemperature_", cntr, ".rds"))
  
  # cbind(
  #   temperature$NAME %>% unique() %>%  sort(),
  #   pop_daily$nutsii_name %>% unique() %>%  sort()
  # ) %>% View()
  
  # focus on the Spanish mainland
  pop_nutsii <- pop_daily$nutsii_name %>% unique()
  nuts2rem <- pop_nutsii[!pop_nutsii %in% temperature$NAME]
  temperature$NAME[!temperature$NAME %in% pop_nutsii]
  
  pop_daily <- 
    pop_daily %>% 
    dplyr::filter(!nutsii_name %in% nuts2rem)
  
  # quick check
  spain_nuts2_2003 <- gisco_get_nuts(year = 2003, nuts_level = 2, country = "ES", resolution = "03")
  spain_nuts2_2003 %>% 
    dplyr::filter(!NUTS_NAME %in% nuts2rem) %>% 
    ggplot() + 
    geom_sf(fill = NA) + theme_bw()
  
  
  pop_daily <- left_join(pop_daily, temperature, 
                       by = c("nutsii_name" = "NAME", 
                              "date" = "dates"))
  
  summary(pop_daily)
  
  # bring public holidays
  hol <- readRDS(paste0("output/hol_", cntr, ".rds"))
  
  hol$date <- as.Date(hol$date)
  pop_daily <- left_join(pop_daily, 
                       hol %>% 
                         dplyr::select(date, counties, hol) %>% 
                         dplyr::filter(counties %in% "National"), 
                       by = c("date" = "date"))
  pop_daily$hol <-ifelse(is.na(pop_daily$hol), 0, 1)
  
  pop_daily <- left_join(pop_daily, 
                         hol %>% 
                           dplyr::select(date, counties, hol) %>% 
                           dplyr::filter(!counties %in% c("National", "Canarias")), 
                         by = c("date" = "date", "nutsii_name" = "counties"))
  
  pop_daily$hol.y <-ifelse(is.na(pop_daily$hol.y), 0, 1)
  pop_daily$hol <- pop_daily$hol.x + pop_daily$hol.y
  pop_daily$hol.x <- pop_daily$hol.y <- NULL
  pop_daily$counties <- NULL
  
  # and bring deaths:
  head(dat.grid)
  head(pop_daily)
  
  pop_daily <- left_join(
    pop_daily, 
    dat.grid, 
    by = c("date" = "date", 
           "age" = "ageg", 
           "sex" = "sex", 
           "nutsii_name" = "nutsii_name")
  )
  summary(pop_daily)
  
  pop_daily$pop[pop_daily$date %in% "2010-01-10"] %>% sum()
  dat.fin <- pop_daily
  
  # define ids for temporal and spatial trends
  dat.fin$month <- lubridate::month(dat.fin$date)
  dat.fin$yweek <- lubridate::week(dat.fin$date)
  dat.fin$yday <- lubridate::yday(dat.fin$date)
  dat.fin$day <- as.numeric(as.factor(dat.fin$date))
  dat.fin$week <- INLA::inla.group(dat.fin$day, 
                                   n = max(dat.fin$day)/7 %>% round(digits = 0),
                                   idx.only =  TRUE)
  dat.fin$id.space <- as.numeric(as.factor(dat.fin$nutsii_name))
  
  # and bring also covid_19 deaths
  covid_deaths <- readRDS("output/COVID_DEATHS.rds")
  dat.fin$isoweek <- substr(ISOweek::date2ISOweek(dat.fin$date),1,8)
  
  covid_deaths %>% 
    dplyr::filter(country %in% "Spain") %>% 
    dplyr::select(isoweek, OWID_covid_deaths) %>% 
    left_join(dat.fin, .) -> dat.fin
  
  summary(dat.fin)
  dat.fin$OWID_covid_deaths[is.na(dat.fin$OWID_covid_deaths)] <- 0
  dat.fin$year <- lubridate::year(dat.fin$date)
  
  dat.fin %>% 
    dplyr::rename(
      date = date, 
      NUTSII = nutsii_name, 
      age = age, 
      sex = sex, 
      pop = pop, 
      deaths = deaths, 
      temperature_lag0 = variable, 
      hol = hol, 
      year = year
    ) %>% 
    dplyr::select(
      date, NUTSII, age, sex, pop, deaths, temperature_lag0, hol, year, month, yweek, yday, day, week, 
      id.space, isoweek, OWID_covid_deaths
    ) -> dat.fin
  
  # i need to make sure the categories are consistent with the other file
  dat.fin$age <- as.character(dat.fin$age)
  dat.fin$age[dat.fin$age %in% "65<"] <- "0-64"
  dat.fin$age[dat.fin$age %in% "65-84"] <- "65-84" 
  dat.fin$age[dat.fin$age %in% ">84"] <- "85+"
    
  dat.fin$sex <- as.character(dat.fin$sex)
  dat.fin$sex <- ifelse(dat.fin$sex == "Females", "female", "male")
    
  saveRDS(dat.fin, file = paste0("output/FinalData_", cntr, ".rds"))
}


rm(list = ls())
dev.off()
gc()
