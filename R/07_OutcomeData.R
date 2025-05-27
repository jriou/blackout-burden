

# Created 08.05.2025

# Clean the outcome data

################################################################################
################################################################################

library(readxl)
library(lubridate)
library(dplyr)
library(rvest)
library(tidyr) 
library(giscoR)

cntr <- "PRT"
cntr <- "ESP"

# wd
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

if(cntr == "PRT"){
  dat <- read_excel("data/PED-724404370_250502.xlsx")
  colnames(dat) <- dat[2,]
  dat <- dat[-c(1:2),]
  dat$date <- paste(dat$Ano,  dat$`Mes óbito`, dat$`Dia óbito`, sep = "-")
  dat$date <- as.Date(dat$date)
  dat$ageg <- dat$`Escalão 6 Dsg`
  dat$ageg <- gsub(" anos", "", dat$ageg)
  dat$ageg <- gsub(" ", "", dat$ageg)
  dat$ageg[dat$ageg %in% "ignorada"] <- NA
  table(dat$ageg)
  dat$`Nut III - N2013 Dsg`[dat$`Nut III - N2013 Dsg` %in% "Ignorado"] <- NA
  
  
  
  ##
  ## Get the link of the NUTSII to be consistent with the newer files
  # NUTSII changed in 2024, whereas NUTSIII remained the same
  linkNUTSII <- read.csv("output/linkageNUTS.csv")
  
  
  
  ##
  dat$NUTSII <- dat$`Nut II - N2013`
  dat$NUTSII_name <- dat$`Nut II - N2013 Dsg`
  
  dat$NUTSII[dat$NUTSII %in% "99"] <- NA
  dat$NUTSII_name[dat$NUTSII_name %in% "Ignorado"] <- NA
  
  
  dat$`Sexo Dsg`[dat$`Sexo Dsg` %in% "Indeterminado"] <- NA
  dat <- dat[complete.cases(dat),]
  
  dat$sex <- ifelse(dat$`Sexo Dsg` == "H", "M", "F")
  dat$deaths <- dat$`Nº Óbitos`
  
  # NUTSIII
  dat$NUTSIII <- dat$`Nut III - N2013`
  dat$NUTSIII_name <- dat$`Nut III - N2013 Dsg`
  
  
  dat %>% 
    dplyr::select(date, NUTSII, NUTSII_name, NUTSIII, NUTSIII_name, ageg, sex, deaths) -> dat
  dat$deaths <- as.numeric(dat$deaths)
  
  dat$NUTSIII_name %>% unique() %>% sort() 
  linkNUTSII$nuts3_nameLatin_2016 %>% unique() %>% sort()
  
  (dat$NUTSIII_name %>% unique())[!(dat$NUTSIII_name %>% unique()) %in% 
                                    (linkNUTSII$nuts3_nameLatin_2016 %>% unique())]
  
  ##
  ##
  sum(!(dat$NUTSIII_name %>% unique()) %in% (linkNUTSII$nuts3_nameLatin_2016 %>% unique()))
  # nice!
  
  dat$agelg <- dat$ageg
  dat$agelg[dat$ageg %in% c("0", "1-4", "5-9", "10-14", "15-19", "20-24", 
                            "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
                            "55-59", "60-64")] <- "0-64"
  dat$agelg[dat$ageg %in% c("85-89", "90-94", "95-99", "100-104", "105-109", "110-114")] <- "85+"
  
  
  
  ##
  ##
  head(dat)
  head(linkNUTSII)
  link2 <- linkNUTSII %>% dplyr::select(nuts3_nameLatin_2016, nuts2_nameLatin_2024) 
  link2 <- link2[!duplicated(link2$nuts3_nameLatin_2016), ]
  dat <- left_join(dat, link2, 
                   by = c("NUTSIII_name" = "nuts3_nameLatin_2016"))
  
  
  dat %>% 
    dplyr::group_by(date, nuts2_nameLatin_2024, agelg, sex) %>% 
    dplyr::summarise(deaths = sum(deaths)) -> dat
  
  ##
  ## the problem now is the Oeste e Vale do Tejo, we need to correct it having in mind the issue
  ## with lisboa metropolitana
  
  dat$nuts2_nameLatin_2024[dat$nuts2_nameLatin_2024 %in% "Grande Lisboa"] <- "Área Metropolitana de Lisboa"
  
  
  ##
  ## Add the most recent file, the modifications are done here, namely there are 3 changes:
  # the metropolitan lisboa is split into two, this reflects both nutsiii changes and nutsii.
  # Área Metropolitana de Lisboa <- Grande Lisboa
  # Área Metropolitana de Lisboa <- Península de Setúbal
  # The third is a change in one name
  dat_new <- readRDS("output/deaths21_25.rds")
  dat_new$nuts2_name %>% table()
  # dat$NUTSII %>% table()
  # dat$NUTSII_name %>% table()
  
  # make sure is the same as the other file
  dat_new %>% 
    dplyr::select(date, nuts2_code, nuts2_name, age_group, sex, deaths) %>% 
    dplyr::rename(date=date, NUTSII = nuts2_code, NUTSII_name = nuts2_name, agelg = age_group, sex=sex) -> dat_new
  
  dat_new$NUTSII_name <- gsub(" ", "", dat_new$NUTSII_name)
  dat_new$NUTSII_name %>% table()
  dat_new$NUTSII_name[dat_new$NUTSII_name %in% "GrandeLisboa"] <- "Área Metropolitana de Lisboa"
  dat_new$NUTSII_name[dat_new$NUTSII_name %in% "PenínsuladeSetúbal"] <- "Área Metropolitana de Lisboa"
  dat_new$NUTSII_name[dat_new$NUTSII_name %in% "OesteeValedoTejo"] <- "Oeste e Vale do Tejo"
  
  
  dat_new %>% 
    dplyr::group_by(date, NUTSII_name, agelg, sex) %>% 
    dplyr::summarise(deaths = sum(deaths)) -> dat_new
  
  ## this is fine. 
  
  # remove the islands
  dat <- dat %>% dplyr::filter(!nuts2_nameLatin_2024 %in% c("Região Autónoma da Madeira", "Região Autónoma dos Açores")) 
  
  #
  
  # check if identical
  
  year(dat$date) %>% unique()
  year(dat_new$date) %>% unique()
  
  # 2021 2022 2023 2024
  dat[year(dat$date) %in% 2021:2022,] %>% dim()
  dat_new[year(dat_new$date) %in% 2021:2022,] %>% dim()
  dat_new$NUTSII_name %>% unique()
  dat$nuts2_nameLatin_2024 %>% unique()
  dat$nuts2_nameLatin_2024[dat$nuts2_nameLatin_2024 %in% "Centro (PT)"] <- "Centro"
  left_join(dat_new[year(dat_new$date) %in% 2021:2022,], 
            dat[year(dat$date) %in% 2021:2022,], 
            by = c("date" = "date", "NUTSII_name" = "nuts2_nameLatin_2024", 
                   "agelg" = "agelg", "sex" = "sex")) -> tmp
  
  tmp$deaths.y[is.na(tmp$deaths.y)] <- 0
  plot(tmp$deaths.x, tmp$deaths.y)
  
  sum(tmp$deaths.x); sum(tmp$deaths.y) # the total number of deaths is identical
  tmp[tmp$deaths.x != tmp$deaths.y,] %>% View()
  
  # the discrepancies are very small, so i think we can go ahead and merge. 
  head(dat);head(dat_new)
  
  rbind(
    dat[year(dat$date)<2021,]  %>% rename(NUTSII_name = nuts2_nameLatin_2024), 
    dat_new) -> dat_fin
  
  ##
  ## need to do the expand grid
  expand.grid(
    NUTSII_name = unique(dat_fin$NUTSII_name), 
    date = seq(from = dat_fin$date %>% min() %>% as.Date(), to = as.Date("2025-05-04"), by = "days"),
    agelg = dat_fin$agelg %>% unique(), 
    sex = dat_fin$sex %>% unique()
  ) -> dat.grid
  
  dat.grid <- left_join(dat.grid, dat_fin)
  dat.grid$deaths[is.na(dat.grid$deaths)] <- 0
  
  
  saveRDS(dat.grid, file = paste0("output/OutcomeData_", cntr, ".rds"))
  
}

if(cntr == "ESP"){
  
  dat <- read.table("data/Spain/Deaths.txt", sep = " ", header = TRUE)
  head(dat)
  
  # prov: nut3 (we send excel with the name of each nut3).
  # gredad: age group (0-14, 15-44, 45-64, 65-75 years)
  # sexo_cod_ine: sex (1=men, 6=women)
  # year=year
  # date_def: date of death
  # def: number of deaths
  # pobl: population (for 2023, 2024 and 2025 the population is that of 2022)
  # 
  # The nuts3 file is ‘Nuts3.txt’ and contains the variables
  # prov: nuts3 id
  # name: name of the nuts3
  
  dat$pobl <- NULL
  colnames(dat) <- c("nutsiii", "ageg", "sex", "year", "date", "deaths")
  dat$date <- as.Date(dat$date)
  
  # subset on 2010
  dat %>% dplyr::filter(year >= 2010) -> dat
  
  # bring nutsiii
  nutsiii <- read.table("data/Spain/Nuts3.txt", sep = " ", header = TRUE)
  dat <- left_join(dat, nutsiii, by = c("nutsiii" = "prov"))
  sum(is.na(dat$name)) # perfect match!
  
  ##
  ## Get the link of the NUTSII to be consistent with the newer files
  # NUTSII changed in 2024, whereas NUTSIII remained the same
  
  # first link nutsiii and nutsii as the population file
  spain_nuts2_2003 <- gisco_get_nuts(year = 2003, nuts_level = 2, country = "ES", resolution = "03")
  spain_nuts3_2003 <- gisco_get_nuts(year = 2003, nuts_level = 3, country = "ES", resolution = "03")
  
  
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
  
  
  
  # and now bring the nutsiii by them:
  nutsiii %>% View()
  link_nutsii_nutsiii <- left_join(link_nutsii_nutsiii, nutsiii, by = c("nutsiii_name" = "name"))
  # and correct the missing ones
  link_nutsii_nutsiii[is.na(link_nutsii_nutsiii$prov),]
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Castellón / Castelló"] <- 12
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Vizcaya"] <- 48
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Álava"] <- 1
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "La Rioja"] <- 26
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Las Palmas"] <- 35
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "A Coruña"] <- 15
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Guipúzcoa"] <- 20
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Valencia / València"] <- 46
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Alicante / Alacant"] <- 3
  link_nutsii_nutsiii$prov[link_nutsii_nutsiii$nutsiii_name %in% "Illes Balears"] <- 7
  
  # this is perfect match. Lets bring to the other dataset
  dat <- left_join(dat, link_nutsii_nutsiii, by = c("nutsiii" = "prov"))
  sum(is.na(dat$nutsii_name))
  
  dat$nutsiii <- dat$nutsiii_code <-dat$nutsiii_name <- NULL
  dat %>% 
    dplyr::group_by(date, ageg, sex, year, nutsii_name, nutsii_code) %>% 
    dplyr::summarise(deaths = sum(deaths)) -> dat
  
  ## and now group by nutsii
  dat$sex <- ifelse(dat$sex == 1, "Males", "Females")
  dat$sex <- factor(dat$sex, levels = c("Males", "Females"))
  dat %>% 
    dplyr::group_by(ageg) %>% 
    dplyr::summarise(sum(deaths))
  
  dat$ageg[dat$ageg %in% "1"] <- "65<" # 0-14
  dat$ageg[dat$ageg %in% "2"] <- "65<" # 15-44
  dat$ageg[dat$ageg %in% "3"] <- "65<" # 45-64
  dat$ageg[dat$ageg %in% "4"] <- "65-84" # 65-75
  dat$ageg[dat$ageg %in% "5"] <- "65-84" # 75-84
  dat$ageg[dat$ageg %in% "6"] <- ">84" # 85+
  
  dat$ageg <- factor(dat$ageg, levels = c("64<", "65-84", ">85"))
  
  dat %>% 
    dplyr::group_by(date, ageg, sex, year, nutsii_name, nutsii_code) %>% 
    dplyr::summarise(deaths = sum(deaths)) -> dat
  
  
  ##
  ## need to do the expand grid
  expand.grid(
    nutsii_name = unique(dat$nutsii_name), 
    date = seq(from = dat$date %>% min() %>% as.Date(), to = as.Date("2025-05-04"), by = "days"),
    ageg = dat$ageg %>% unique(), 
    sex = dat$sex %>% unique()
  ) -> dat.grid
  
  dat.grid <- left_join(dat.grid, dat)
  dat.grid$deaths[is.na(dat.grid$deaths)] <- 0
  
  dat.grid %>% 
    dplyr::group_by(ageg) %>% 
    dplyr::summarise(sum(deaths))
  
  saveRDS(dat.grid, file = paste0("output/OutcomeData_", cntr, ".rds"))
  
}

rm(list = ls())
dev.off()
gc()




