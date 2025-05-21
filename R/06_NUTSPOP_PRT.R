

# Created 13.05.2025

# Clean the population

# https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_indicadores&indOcorrCod=0008273&contexto=bd&selTab=tab2
# https://www.ine.pt/xportal/xmain?bdind_por_pagina=15&bdfreetext=Word%28s%29+to+find&bdtemas=1115&bdnivelgeo=4&contexto=bd&atributoordenar=null&atributoordem=null&bdsubtemas=111516&xpid=INE&xpgid=ine_base_dados&bdpagenumber=1

#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(giscoR)
library(tidyr)
library(stringr)

portugal_nuts3_2013 <- gisco_get_nuts(year = 2013, nuts_level = 3, country = "PT", resolution = "03")
portugal_nuts3_2016 <- gisco_get_nuts(year = 2016, nuts_level = 3, country = "PT", resolution = "03")
sum(!portugal_nuts3_2013$NAME_LATN %in% portugal_nuts3_2016$NAME_LATN)
# nice nutsiii 2013 and nutsiii 2016 are the same

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/Desktop/Portugal/"
setwd(path)

list.files("data/")[list.files("data/") %>% startsWith(., "pop")] %>% 
  lapply(., function(X) read_excel(paste0("data/", X))) -> popfiles

CleanPopulation <- function(Z){
  Z[ , apply(Z, 2, function(x) !any(is.na(x)))]
  Z <- Z[, colSums(is.na(Z)) < nrow(Z)]
  
  colnames(Z) <- Z[4,]
  colnames(Z)[-c(1:4)] <- Z[8,-c(1:4)]
  Z <- Z[-c(3:4)]
  Z <- Z[-c(3:(which(Z[6,] %in% "M")-1))]
  
  
  c(
    "year", 
    "nutsiii",
    paste("male", Z[8,3:(which(Z[6,] %in% "F")-1)] , sep = "."), 
    paste("female", Z[8,(which(Z[6,] %in% "F")):ncol(Z)] , sep = ".")
  ) -> colnames(Z)
  
  Z <- Z[-c(1:9),]
  
  Z %>% 
    fill(year, .direction = "down") -> Z
  
  Z <- Z[!is.na(Z$`male.0 - 4 years`),]
  
  # lets do long format
  colnames(Z) %>% tail()
  data_long <- gather(Z, cat, pop, `male.Total`:`female.85 and more years`)
  data_long$sex <- sapply(strsplit(data_long$cat, "\\."), function(X) X[1])
  data_long$ageg <- sapply(strsplit(data_long$cat, "\\."), function(X) X[2])
  data_long$cat <- NULL
  
  data_long %>% filter(!ageg %in% "Total") -> data_long
  data_long$ageg <- gsub("years", "", data_long$ageg)
  data_long$ageg <- gsub(" ", "", data_long$ageg)
  data_long %>% 
    dplyr::filter(
      nutsiii %in% portugal_nuts3_2013$NAME_LATN
    ) -> data_long
  
  # and need to remove the duplocated (nutsii same name as nutsiii)
  data_long <- unique(data_long)
  return(data_long)
}



# run the function
lapply(popfiles, CleanPopulation) -> pop_res
lapply(pop_res, nrow)
pop_res <- do.call(rbind, pop_res)

saveRDS(pop_res, file = "output/pop_allage.rds")

# one check

pop_res_AC <- 
  pop_res %>% 
  filter(nutsiii  %in% "Alentejo Central", 
         sex %in% "female", 
         ageg %in% "80-84") 

plot(pop_res_AC$year, pop_res_AC$pop)


##
## ok now i need to do the two stage linear interpolation.

# First I will aggregate by the relevant age groups.

pop_res$ageg[pop_res$ageg %in% c("0-4", "5-9", "10-14", "15-19", "20-24", 
                          "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                          "55-59", "60-64")] <- "0-64"
pop_res$ageg[pop_res$ageg %in% c("85andmore")] <- "85+"

pop_res %>% 
  dplyr::group_by(year, nutsiii, sex, ageg) %>% 
  dplyr::summarise(pop = sum(as.numeric(pop))) -> pop_res
pop_res$year <- as.numeric(pop_res$year)
# first I need to impute two points, 2010 and 2024 (we can talk about 2024 in a second)

# The indicator you mention in your request is sourced from the Estimates of the Resident Population reported on 31/12 of each year.
# we do not have the population for 2009, 2010, 2024 and 2025, we can calculate with linear interpolation
pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2009))) %>% 
  mutate(year = 2009) -> pop2009

pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2010))) %>% 
  mutate(year = 2010) -> pop2010

pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2024))) %>% 
  mutate(year = 2024) -> pop2024

pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2025))) %>% 
  mutate(year = 2025) -> pop2025

# and bring together

pop <- rbind(pop_res, 
             pop2009 %>% dplyr::select(colnames(pop_res)),
             pop2010 %>% dplyr::select(colnames(pop_res)), 
             pop2024 %>% dplyr::select(colnames(pop_res)), 
             pop2025 %>% dplyr::select(colnames(pop_res)))


# I will assume that these are the same as the 1st of January each year
pop$year <- pop$year + 1

expand.grid(age = pop$ageg %>% unique(), 
            sex = pop$sex %>% unique(), 
            region = pop$nutsiii %>% unique(), 
            date = seq(from = as.Date("2010-01-01"), to = as.Date("2025-5-31"), by = "days")) -> pop_daily

# As the population is only available for the 1st of January of every year, we need to create a weekly version
# to feed in the model. The above gives as all the possible combinations required for the linear interpolation.

# the reference date refers to the population availability of the first of each year
pop_daily$year <- lubridate::year(pop_daily$date)
pop_daily %>% group_by(year) %>% mutate(refdate = as.Date(paste0(year, "-01-01"))) -> pop_daily
# the day2pred is needed for the linear interpolation within the year
pop_daily$day2pred <- as.numeric(pop_daily$date - as.Date("2010-01-01") + 1)
pop_daily$refdate2 <- as.Date(paste0(pop_daily$year + 1, "-01-01"))


##
## and i need to add this years and next years population
head(pop_daily)
head(pop)
pop_daily <- left_join(pop_daily, pop, 
                       by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "nutsiii"))

pop$year <- pop$year - 1
colnames(pop)[5] <- "pop.next.year"

pop_daily <- left_join(pop_daily, pop, 
                       by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "nutsiii"))

pop_daily %>% 
  mutate(lambda = (pop.next.year - pop)/as.numeric((refdate2 - refdate))) %>% 
  mutate(beta0 = pop - lambda*as.numeric(refdate - as.Date("2010-01-01") + 1)) %>% 
  mutate(popfin = beta0 + lambda*day2pred) -> pop_daily

# remove what is not needed any longer
pop_daily$day2pred <- pop_daily$refdate <- 
  pop_daily$population <- pop_daily$pop.next.year <- 
  pop_daily$refdate2 <-
  pop_daily$lambda <- pop_daily$beta0 <-  pop_daily$days2plot <- NULL

colnames(pop_daily)[3] <- "nutsiii"
colnames(pop_daily)[6] <- "population"


# For the plot we are focusing on Alentejo Central and to female population less than 40

pop_daily_AC <- 
  pop_daily %>% 
  filter(nutsiii  %in% "Alentejo Central", 
         sex %in% "female", 
         age %in% "80-84", year == 2015) %>% 
  mutate(x = as.numeric(as.factor(date)))

pop_daily_AC <- 
  pop_daily %>% 
  filter(nutsiii  %in% "Alentejo Central", 
         sex %in% "female", 
         age %in% "80-84")

plot(pop_daily_AC$date, pop_daily_AC$popfin)

# looks correct

##
## now we need to aggregate by NUTSII (our specification of consistent boundaries)
## and store

shp <- read_sf("output/shp.shp")
link <- read.csv("output/linkageNUTS.csv")
link2 <- link %>% dplyr::select(nuts3_nameLatin_2016, nuts2_nameLatin_2024) 
link2 <- link2[!duplicated(link2$nuts3_nameLatin_2016), ]
# i need to do this because metropolitan lisboa is repeated 
pop_daily <- left_join(pop_daily, link2, by = c("nutsiii" = "nuts3_nameLatin_2016"))

pop_daily %>% 
  dplyr::group_by(date, nuts2_nameLatin_2024, age, sex) %>% 
  dplyr::summarise(pop = sum(popfin)) -> pop_daily

##
## the problem now is the Oeste e Vale do Tejo, we need to correct it having in mind the issue
## with lisboa metropolitana

pop_daily$nuts2_nameLatin_2024[pop_daily$nuts2_nameLatin_2024 %in% "Grande Lisboa"] <- "Área Metropolitana de Lisboa"
pop_daily$nuts2_nameLatin_2024 %>% unique()

pop_daily$nuts2_nameLatin_2024[pop_daily$nuts2_nameLatin_2024 %in% "Centro (PT)"] <- "Centro"

pop_daily %>% dplyr::filter(!(nuts2_nameLatin_2024 %in% c("Região Autónoma da Madeira", "Região Autónoma dos Açores"))) -> pop_daily


##
## this should be identical with the outcome. Prob temporal missalign.
dat.grid <- readRDS("output/OutcomeData.rds")
pop_daily %>% filter(date <= max(dat.grid$date)) %>% dim()
dim(dat.grid) # perfect, they are identical!

# store
saveRDS(pop_daily, file = "output/pop_nutsii_lp.rds")

rm(list = ls())
dev.off()
gc()

