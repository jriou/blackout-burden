
# Created 04.04.2025

# Download NUTS3 regions
# However, as the NUTS3 regions in Portugal have changed during our study period
# I need to establish the linkage but also create consistent shp boundaries. 

#-------------------------------------------------------------------------------

wd <- "C:/Users/gkonstan/OneDrive - Imperial College London/Desktop/Portugal/"
setwd(file.path(wd, "output"))

# install.packages("giscoR")
library(giscoR)
library(sf)

portugal_nuts3_2016 <- gisco_get_nuts(year = 2016, nuts_level = 3, country = "PT", resolution = "03")
portugal_nuts3_2024 <- gisco_get_nuts(year = 2024, nuts_level = 3, country = "PT", resolution = "03")

portugal_nuts3_2024$NAME_LATN[!portugal_nuts3_2024$NAME_LATN %in% 
                               portugal_nuts3_2016$NAME_LATN]

portugal_nuts3_2024$geometry[!portugal_nuts3_2024$NAME_LATN %in% 
                                portugal_nuts3_2016$NAME_LATN] %>% plot()

ggplot() + 
  geom_sf(data = portugal_nuts3_2016) + 
  geom_sf(data = portugal_nuts3_2024[!portugal_nuts3_2024$NAME_LATN %in% 
                                                portugal_nuts3_2016$NAME_LATN, ], 
          fill = "red", col = NA, alpha = 0.2)

# it seems that they have changed names
ggplot() + 
  geom_sf(data = portugal_nuts3_2016[portugal_nuts3_2016$NAME_LATN %in% 
                                       "Alto Tâmega", ]) + 
  geom_sf(data = portugal_nuts3_2024[portugal_nuts3_2024$NAME_LATN %in% 
                                       "Alto Tâmega e Barroso", ], 
          fill = "red", col = NA, alpha = 0.2)

# ok Alto Tâmega is identical with Alto Tâmega e Barroso.

portugal_nuts3_2024$NAME_LATN[!portugal_nuts3_2024$NAME_LATN %in% 
                                portugal_nuts3_2016$NAME_LATN]

portugal_nuts3_2016$NAME_LATN[!portugal_nuts3_2016$NAME_LATN %in% 
                                portugal_nuts3_2024$NAME_LATN]

ggplot() + 
  geom_sf(data = portugal_nuts3_2016[portugal_nuts3_2016$NAME_LATN %in% 
                                       "Área Metropolitana de Lisboa", ]) + 
  geom_sf(data = portugal_nuts3_2024[portugal_nuts3_2024$NAME_LATN %in% 
                                       c("Grande Lisboa", "Península de Setúbal"), ], 
          fill = "red", col = NA, alpha = 0.2)

# and "Área Metropolitana de Lisboa" was split into "Grande Lisboa", "Península de Setúbal"

# so what im doing is create a link between the nutsiii of the two years and 
# then between the nutsii

data.link <- 
  data.frame(
    nuts3_nameLatin_2024 = portugal_nuts3_2024$NAME_LATN, 
    nuts3_code_2024 = portugal_nuts3_2024$NUTS_ID
  )

data.link <- left_join(data.link, 
                       data.frame(
                         nuts3_nameLatin_2024 = portugal_nuts3_2016$NAME_LATN,
                         nuts3_nameLatin_2016 = portugal_nuts3_2016$NAME_LATN, 
                         nuts3_code_2016 = portugal_nuts3_2016$NUTS_ID
                       ))

data.link[data.link$nuts3_nameLatin_2024 %in% "Alto Tâmega e Barroso",
          c("nuts3_nameLatin_2016", "nuts3_code_2016")] <- 
  c("Alto Tâmega", "PT11B")

data.link$nuts3_nameLatin_2016[data.link$nuts3_nameLatin_2024 %in% 
                                 "Grande Lisboa"] <- 
  data.link$nuts3_nameLatin_2016[data.link$nuts3_nameLatin_2024 %in% 
                                   "Península de Setúbal"] <- 
  "Área Metropolitana de Lisboa"

data.link$nuts3_code_2016[data.link$nuts3_nameLatin_2016 %in% 
                                 "Área Metropolitana de Lisboa"] <- 
  "PT170"

##
## nice and here I need to bring the NUTSII regions

portugal_nuts3_2016 <- gisco_get_nuts(year = 2016, nuts_level = 3, country = "PT", resolution = "03")
portugal_nuts2_2016 <- gisco_get_nuts(year = 2016, nuts_level = 2, country = "PT", resolution = "03")

ggplot() + 
  geom_sf(data = portugal_nuts2_2016) + 
  geom_sf(data = portugal_nuts3_2016, 
          aes(fill = NAME_LATN), col = NA, alpha = 0.2) + 
  theme(legend.position = "none") 

LINKNUTSII_2016 <- st_intersection(
  portugal_nuts2_2016 %>% 
    dplyr::select(NAME_LATN, NUTS_ID) %>% 
    dplyr::rename(nuts2_nameLatin_2016 = NAME_LATN, 
                  nuts2_code_2016 = NUTS_ID), 
  portugal_nuts3_2016 %>% 
    dplyr::select(NAME_LATN, NUTS_ID) %>% 
    dplyr::rename(nuts3_nameLatin_2016 = NAME_LATN, 
                  nuts3_code_2016 = NUTS_ID))
LINKNUTSII_2016$geometry <- NULL

##
## same for 2024

portugal_nuts3_2024 <- gisco_get_nuts(year = 2024, nuts_level = 3, country = "PT", resolution = "03")
portugal_nuts2_2024 <- gisco_get_nuts(year = 2024, nuts_level = 2, country = "PT", resolution = "03")

LINKNUTSII_2024 <- st_intersection(
  portugal_nuts2_2024 %>% 
    dplyr::select(NAME_LATN, NUTS_ID) %>% 
    dplyr::rename(nuts2_nameLatin_2024 = NAME_LATN, 
                  nuts2_code_2024 = NUTS_ID), 
  portugal_nuts3_2024 %>% 
    dplyr::select(NAME_LATN, NUTS_ID) %>% 
    dplyr::rename(nuts3_nameLatin_2024 = NAME_LATN, 
                  nuts3_code_2024 = NUTS_ID))
LINKNUTSII_2024$geometry <- NULL
head(data.link)
##
## and bring to the nutsiii link file
data.link <- left_join(data.link, LINKNUTSII_2016, by = c("nuts3_nameLatin_2016" = "nuts3_nameLatin_2016", 
                                                        "nuts3_code_2016" = "nuts3_code_2016"))

data.link <- left_join(data.link, LINKNUTSII_2024, by = c("nuts3_nameLatin_2024" = "nuts3_nameLatin_2024", 
                                                          "nuts3_code_2024" = "nuts3_code_2024"))

colnames(data.link)

data.link %>% 
  dplyr::select(
    "nuts2_nameLatin_2016", "nuts2_code_2016", 
    "nuts3_nameLatin_2016", "nuts3_code_2016",
    "nuts2_nameLatin_2024", "nuts2_code_2024", 
    "nuts3_nameLatin_2024", "nuts3_code_2024") -> data.link

write.csv(data.link, file = "linkageNUTS.csv")


rm(list = ls())
dev.off()
gc()



##
## I need to create a new nuts ii shp from the latest nutsiii
link <- read.csv("output/linkageNUTS.csv")

portugal_nuts3 <- gisco_get_nuts(year = 2024, nuts_level = 3, country = "PT", resolution = "03")
plot(portugal_nuts3$geometry)

portugal_nuts3 <- left_join(portugal_nuts3, link, by = c("NAME_LATN" = "nuts3_nameLatin_2024"))
portugal_nuts3 %>% 
  dplyr::group_by(nuts2_nameLatin_2024) %>% 
  dplyr::summarise(x=sum(as.numeric(MOUNT_TYPE), na.rm = TRUE)) -> shp_tmp

shp_tmp$nuts2_nameLatin_2024[shp_tmp$nuts2_nameLatin_2024 %in% 
                               "Grande Lisboa"] <- 
  shp_tmp$nuts2_nameLatin_2024[shp_tmp$nuts2_nameLatin_2024 %in% 
                                 "Península de Setúbal"] <-
  "Área Metropolitana de Lisboa"

shp_tmp %>% 
  dplyr::group_by(nuts2_nameLatin_2024) %>% 
  dplyr::summarise(x=sum(as.numeric(x), na.rm = TRUE)) -> shp_tmp

# and remove the islands
shp_tmp %>% 
  dplyr::filter(!nuts2_nameLatin_2024 %in% c("Região Autónoma da Madeira", "Região Autónoma dos Açores")) -> shp_tmp

plot(shp_tmp$geometry)

write_sf(shp_tmp, "output/shp.shp")


rm(list = ls())
dev.off()
gc()
