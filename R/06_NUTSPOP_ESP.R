

# Created 25.05.2025

# Clean the population for Spain

# https://www.ine.es/jaxiT3/Tabla.htm?t=56945&L=0
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(giscoR)
library(tidyr)
library(stringr)

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

# Now I need to retrieve the shp for NUTS2 and NUTS3 and aggregate per NUTS2 regions.