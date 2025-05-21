
# Created 13.05.2025

# Bring everything together

#-------------------------------------------------------------------------------

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)


library(tidyverse)

cntr <- "PRT"
cntr <- "ESP"

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

rm(list = ls())
dev.off()
gc()
