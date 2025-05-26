

# Created 25.05.2025

# Clean the population for Spain

# https://www.ine.es/jaxiT3/Tabla.htm?t=56945&L=0
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(giscoR)
library(tidyr)
library(stringr)
library(giscoR)

path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden"
setwd(path)

pop <- read.csv("data/Spain/pop.csv", sep = ";", header = TRUE)
head(pop)


# Clean age
pop$Edad.simple %>% table()

torem <- 
  c(paste0(85:99, " años"), "Todas las edades", "100 y más años")

pop %>% 
  dplyr::filter(!Edad.simple %in% torem) -> pop

pop$Edad.simple <- gsub(" años", replacement = "", x = pop$Edad.simple)
pop$Edad.simple[pop$Edad.simple %in% "85 y más"] <- ">84"
pop$Edad.simple[pop$Edad.simple %in% 0:64] <- "65<"
pop$Edad.simple[pop$Edad.simple %in% "1 año"] <- "65<"
pop$Edad.simple[pop$Edad.simple %in% 65:84] <- "65-84"
pop$Edad.simple <- factor(pop$Edad.simple, levels = c("65<", "65-84", ">84"))

# Clean sex
pop$Sexo %>% table()
pop %>% 
  dplyr::filter(!Sexo %in% "Total") -> pop
pop$Sexo <- ifelse(pop$Sexo == "Hombres", "Males", "Females")
pop$Sexo <- factor(pop$Sexo, levels = c("Males", "Females"))

# Clean period
pop$Periodo %>% table()
pop[grepl("enero", pop$Periodo, fixed = TRUE),] -> pop
pop$year <- str_sub(pop$Periodo,-4,-1) %>% as.numeric()
pop$Periodo <- NULL

pop %>% 
  dplyr::filter(year >= 2010) -> pop

# Clean NUTS3
pop$Provincias %>% table()
pop %>% dplyr::filter(!Provincias %in% "Total Nacional") -> pop
head(pop)
pop %>% dplyr::rename(year = year, 
                      nutsiii = Provincias, 
                      sex = Sexo, 
                      ageg = Edad.simple,  
                      pop = Total) %>%
  dplyr::select(year, nutsiii, sex, ageg, pop) -> pop
pop$pop <- gsub("\\.", "", pop$pop) %>% as.numeric()

pop %>% 
  dplyr::group_by(year, nutsiii, sex, ageg) %>% 
  dplyr::summarise(pop = sum(pop)) -> pop

# lets remove the number
pop$nutsiii_code <- substr(pop$nutsiii, start = 1, 2)
pop$nutsiii <- gsub('[[:digit:]]+', '', pop$nutsiii) %>% gsub(" ", "", .)
# expand.grid(
#   year = pop$year %>% unique(), 
#   sex = pop$sex %>% unique(), 
#   ageg = pop$ageg %>% unique(), 
#   nutsiii = pop$nutsiii %>% unique()
# ) -> dat.expand

# Now I need to retrieve the shp for NUTS2 and NUTS3 and aggregate per NUTS2 regions.

spain_nuts3_2003 <- gisco_get_nuts(year = 2003, nuts_level = 3, country = "ES", resolution = "03")

cbind(
  spain_nuts3_2003$NUTS_NAME %>% sort(),
  pop$nutsiii %>% unique() %>% sort()
) %>% View()

pop_nutsiii <- pop$nutsiii %>% unique()
spain_nuts3_2003$NUTS_NAME[spain_nuts3_2003$NUTS_NAME %in% pop_nutsiii]
spain_nuts3_2003$NUTS_NAME[!spain_nuts3_2003$NUTS_NAME %in% pop_nutsiii]
pop_nutsiii[pop_nutsiii %in% spain_nuts3_2003$NUTS_NAME]
pop_nutsiii[!pop_nutsiii %in% spain_nuts3_2003$NUTS_NAME]

# to change
pop$nutsiii[pop$nutsiii %in% "Araba/Álava"] <- "Álava"
pop$nutsiii[pop$nutsiii %in% "Alicante/Alacant"] <- "Alicante / Alacant"
pop$nutsiii[pop$nutsiii %in% "Balears,Illes"] <- "Illes Balears"
pop$nutsiii[pop$nutsiii %in% "Castellón/Castelló"] <- "Castellón / Castelló"
pop$nutsiii[pop$nutsiii %in% "CiudadReal"] <- "Ciudad Real"
pop$nutsiii[pop$nutsiii %in% "Coruña,A"] <- "A Coruña"
pop$nutsiii[pop$nutsiii %in% "Gipuzkoa"] <- "Guipúzcoa"
pop$nutsiii[pop$nutsiii %in% "Rioja,La"] <- "La Rioja"
pop$nutsiii[pop$nutsiii %in% "Palmas,Las"] <- "Las Palmas"
pop$nutsiii[pop$nutsiii %in% "SantaCruzdeTenerife"] <- "Santa Cruz de Tenerife"
pop$nutsiii[pop$nutsiii %in% "Valencia/València"] <- "Valencia / València"
pop$nutsiii[pop$nutsiii %in% "Bizkaia"] <- "Vizcaya"

# ok so the population file is consistent with the nutsiii. Now I need to aggregate per nutsii
# and perform the linear interpolation

spain_nuts2_2003 <- gisco_get_nuts(year = 2003, nuts_level = 2, country = "ES", resolution = "03")
spain_nuts2_2016 <- gisco_get_nuts(year = 2016, nuts_level = 2, country = "ES", resolution = "03")
spain_nuts2_2024 <- gisco_get_nuts(year = 2024, nuts_level = 2, country = "ES", resolution = "03")
spain_nuts2_2003$NUTS_NAME
spain_nuts2_2016$NUTS_NAME
spain_nuts2_2024$NUTS_NAME

plot(spain_nuts2_2003$geometry)
plot(spain_nuts2_2016$geometry, border = "red")
plot(spain_nuts2_2024$geometry, border = "blue") # seem identical

cbind(
  spain_nuts2_2003$NUTS_NAME %>% sort(), 
  spain_nuts2_2016$NUTS_NAME %>% sort(), 
  spain_nuts2_2024$NUTS_NAME %>% sort()
) %>% View()

# perfect, they are identical! So the nuts2 levels is consistent 
# across the regions. so my population here will be fine, as I asked
# for nutsiii from them. 

spain_nuts2_2003$NUTS_ID
spain_nuts3_2003$NUTS_ID

# do the linear interpolation at the nutsiii and aggregate to nutsii, 
# as in Portugal

pop$year %>% table()
# a couple of checks on pop
pop[duplicated(pop),]
expand.grid(
  year = pop$year %>% unique(), 
  sex = pop$sex %>% unique(), 
  ageg = pop$ageg %>% unique(), 
  nutsiii = pop$nutsiii %>% unique()
) -> dat.expand # seems fine

# ok i have until January 1st 2024, I need January 1st 2025 and 2026.

pop_res <- pop
head(pop_res)
pop_res$nutsiii_code <- NULL

pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2025))) %>% 
  mutate(year = 2025) -> pop2025

pop_res %>% group_by(nutsiii, sex, ageg) %>% 
  summarise(pop = as.vector(coef(lm(pop ~ year)) %*% c(1, 2026))) %>% 
  mutate(year = 2026) -> pop2026

pop <- rbind(pop_res, 
             pop2025 %>% dplyr::select(colnames(pop_res)), 
             pop2026 %>% dplyr::select(colnames(pop_res)))

# pop$year <- pop$year + 1

expand.grid(age = pop$ageg %>% unique(), 
            sex = pop$sex %>% unique(), 
            region = pop$nutsiii %>% unique(), 
            date = seq(from = as.Date("2010-01-01"), 
                       to = as.Date("2025-5-31"), by = "days")) -> 
  pop_daily

# the reference date refers to the population availability of the first of each year
pop_daily$year <- lubridate::year(pop_daily$date)
pop_daily %>% group_by(year) %>% mutate(refdate = as.Date(paste0(year, "-01-01"))) -> pop_daily
# the day2pred is needed for the linear interpolation within the year
pop_daily$day2pred <- as.numeric(pop_daily$date - as.Date("2010-01-01") + 1)
pop_daily$refdate2 <- as.Date(paste0(pop_daily$year + 1, "-01-01"))
head(pop_daily)

##
## and i need to add this years and next years population
head(pop_daily)
head(pop)

pop_daily <- left_join(pop_daily, pop, 
                       by = c("year" = "year", 
                              "age" = "ageg", 
                              "sex" = "sex", 
                              "region" = "nutsiii"))


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


# For the plot we are focusing on Cáceres and to female population less than 40

pop_daily_C <- 
  pop_daily %>% 
  filter(nutsiii  %in% "Cáceres", 
         sex %in% "Females", 
         age %in% "65<", year == 2015) %>% 
  mutate(x = as.numeric(as.factor(date)))

pop_daily_C <- 
  pop_daily %>% 
  filter(nutsiii  %in% "Cáceres", 
         sex %in% "Females", 
         age %in% "65<")

plot(pop_daily_C$date, pop_daily_C$popfin)
dev.off()


##
## now we need to aggregate by NUTSII (our specification of consistent boundaries)
## and store

spain_nuts2_2003$NUTS_ID
spain_nuts3_2003$NUTS2_ID <- substr(spain_nuts3_2003$NUTS_ID, 
                                    start = 1, stop = 4)
spain_nuts3_2003$NUTS2_ID  %>% unique() %>% sort()
spain_nuts2_2003$NUTS_ID %>% sort()

link_nutsii_nutsiii <- 
  data.frame(
    nutsii_code = spain_nuts3_2003$NUTS2_ID,
    nutsiii_name = spain_nuts3_2003$NUTS_NAME,
    nutsiii_code = spain_nuts3_2003$NUTS_ID
  )

left_join(
  link_nutsii_nutsiii, 
  data.frame(
    nutsii_code = spain_nuts2_2003$NUTS_ID, 
    nutsii_name = spain_nuts2_2003$NUTS_NAME
  )
) -> link_nutsii_nutsiii

##
## Check the validity of this linkage

# library(sf)
# st_centroid(spain_nuts3_2003)
# ggplot() + 
#   geom_sf(data=spain_nuts3_2003, fill = NA) + 
#   geom_sf(data=st_centroid(spain_nuts3_2003), aes(col = NUTS2_ID))
# 
# spain_nuts3_2003 %>% 
#   filter(NUTS2_ID == "ES24") %>% 
#   ggplot() + 
#   geom_sf(fill = NA) + theme_bw()
# looks good

summary(pop_daily)
pop_daily <- 
  left_join(
    pop_daily, 
    link_nutsii_nutsiii, 
    by = c("nutsiii" = "nutsiii_name")
  )

# is.na(pop_daily$age) %>% sum()
# is.na(pop_daily$date) %>% sum()
# is.na(pop_daily$nutsii_code) %>% sum()


pop_daily %>% 
  dplyr::group_by(date, nutsii_name, age, sex) %>% 
  dplyr::summarise(pop = sum(popfin)) -> pop_daily


# store
saveRDS(pop_daily, file = "output/pop_nutsii_lp_ESP.rds")

