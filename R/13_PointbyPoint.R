

# Created 08.01.2026

# Point by point reply

# -----------------------------------------------------------------------

library(tidyverse)
library(INLA)
library(patchwork)
library(xtable)
library(sf)
library(units)

# set path
path <- "."
setwd(path)


datESP <- readRDS(paste0("output/FinalData_ESP.rds"))
datESP <- datESP[!is.na(datESP$deaths),]
shp_fileESP <- list.files(paste0(path, "/output/"), 
                       pattern = paste0("\\_", "ESP", ".shp$"), full.names = TRUE)
shp_fileESP <- read_sf(shp_fileESP)
shp_fileESP$area <- st_area(shp_fileESP) |>  set_units(km^2) |> drop_units() |> round()

datPRT <- readRDS(paste0("output/FinalData_PRT.rds"))
datPRT <- datPRT[!is.na(datPRT$deaths),]
shp_filePRT <- list.files(paste0(path, "/output/"), 
                          pattern = paste0("\\_", "PRT", ".shp$"), full.names = TRUE)
shp_filePRT <- read_sf(shp_filePRT)
shp_filePRT$area <- st_area(shp_filePRT) |>  set_units(km^2) |> drop_units() |> round()


## Table of # of deaths by age group
getTable <- function(df){
  
  df |>
    # base summary by NUTSII–age–sex
    dplyr::group_by(NUTSII, age, sex) |>
    dplyr::summarise(
      deaths_total = sum(deaths, na.rm = TRUE),
      pop_mean     = mean(pop, na.rm = TRUE),.groups = "drop"
    ) |>
    # add totals over sex (within NUTSII–age)
    left_join(
      df |>
        dplyr::group_by(NUTSII, age) |>
        dplyr::summarise(
          deaths_total_age = sum(deaths, na.rm = TRUE),
          pop_mean_age     = mean(pop, na.rm = TRUE),.groups = "drop"
        ),
      by = c("NUTSII", "age")
    ) |>
    # add totals over age (within NUTSII–sex)
    left_join(
      df |>
        dplyr::group_by(NUTSII, sex) |>
        dplyr::summarise(
          deaths_total_sex = sum(deaths, na.rm = TRUE),
          pop_mean_sex     = mean(pop, na.rm = TRUE),.groups = "drop"
        ),
      by = c("NUTSII", "sex")
    ) |>
    # add totals over age & sex (within NUTSII)
    left_join(
      df |>
        dplyr::group_by(NUTSII) |>
        dplyr::summarise(
          deaths_total_all = sum(deaths, na.rm = TRUE),
          pop_mean_all     = mean(pop, na.rm = TRUE),.groups = "drop"
        ),
      by = "NUTSII"
    ) %>% return()
  
}

tabPRT <- getTable(datPRT)
tabESP <- getTable(datESP)

shp_filePRT$n2_L_20[shp_filePRT$n2_L_20 %in% "Centro (PT)"] <- "Centro"
tabPRT <- tabPRT %>% left_join(., shp_filePRT, by = c("NUTSII" = "n2_L_20"))

tabESP <- tabESP %>% left_join(., shp_fileESP, by = c("NUTSII" = "NUTS_NAME"))

rbind(
  tabPRT %>% dplyr::select(NUTSII, deaths_total_all, pop_mean_all, area) %>% dplyr::filter(!duplicated(NUTSII)), 
  tabESP %>% dplyr::select(NUTSII, deaths_total_all, pop_mean_all, area) %>% dplyr::filter(!duplicated(NUTSII))
) %>% xtable() %>% print(include.rownames = FALSE)



##
## Now do the deprivation

# Download data (this can take a bit of time)
library(eurostat)
gdp_raw <- get_eurostat("nama_10r_2gdp", time_format = "num")

gdp_es_pt_nuts2 <- 
  gdp_raw %>%
  # Keep NUTS2 regions only
  filter(nchar(as.character(geo)) == 4) %>%  # NUTS2 codes have length 4 (e.g. ES11, PT17)
  # Keep GDP per capita in PPS
  filter(unit == "PPS_EU27_2020_HAB") %>%              # PPS per inhabitant
  # Keep Spain and Portugal
  filter(substr(as.character(geo), 1, 2) %in% c("ES", "PT"))

gdp_es_pt_2023 <- 
  gdp_es_pt_nuts2 %>%
  filter(TIME_PERIOD == 2023) %>%
  select(geo, TIME_PERIOD, values) %>%
  arrange(geo)

##
## We want the geo matched with the one at map_plot

##
## and i can do maps of this but also correlation plots
dev.off()
map_plot <- readRDS("output/map_plot.rds")
map_plot %>% dplyr::filter(country %in% "Portugal") %>% dplyr::pull(NAME_LATN) %>% unique()
# shp <- gisco_get_nuts(year = 2024, nuts_level = 2, country = "PT", resolution = "03")
linkESP <- readRDS("output/linkESP.rds")
linkPRT <- 
  data.frame(
    nutsii_code = c("PT1C", "PT15", "PT19", "PT11", "PT1D", "PT1A", "PT1B"),
    nutsii_name = c("Alentejo", "Algarve", "Centro", "Norte", "Oeste e Vale do Tejo", 
                    "Área Metropolitana de Lisboa", "Área Metropolitana de Lisboa")
  )

link <- rbind(linkESP, linkPRT)
left_join(gdp_es_pt_2023, link, by = c("geo" = "nutsii_code")) %>% 
  left_join(map_plot, ., by = c("NAME_LATN" = "nutsii_name"), relationship = "many-to-many") -> gdp_plots

gdp_plots$gdp <- gdp_plots$values %>% as.numeric()


(ggplot(data = gdp_plots %>% dplyr::filter(lag == "Lag0")) + 
  geom_sf(aes(fill = gdp), col = NA) +
  scale_fill_viridis_c(option = "E", alpha = 0.9) +
  theme(legend.title = element_blank()) + 
  ggtitle("Scaled GDP")) 

ggsave(filename = "output/gdp.png", dpi = 300, width = 5, height = 3)

(ggplot(data = gdp_plots, aes(x = gdp, y = `50%`)) + 
  geom_point() +
  # geom_smooth(method='lm', formula= y~x) + 
  facet_grid(cols = vars(lag), rows = vars(country)) + theme_bw() + 
  ylab("Median posterior relative excess mortality") + xlab("Scaled GDP") )

ggsave(filename = "output/cor_gdp.png", dpi = 300, width = 8.5, height = 5)


tab <- readRDS("output/tab1.rds")

# 2. Rename columns to standard names (optional but helpful)
df2 <- tab %>%
  rename(
    Blackout_PT       = Blackout.x,
    After2days_PT     = `2 days after.x`,
    After1week_PT     = `1 week after.x`,
    Blackout_ES       = Blackout.y,
    After2days_ES     = `2 days after.y`,
    After1week_ES     = `1 week after.y`
  )

# 3. Long format by country (Portugal / Spain), keeping period columns wide
df_country_long <- df2 %>%
  pivot_longer(
    cols = c(Blackout_PT, After2days_PT, After1week_PT,
             Blackout_ES, After2days_ES, After1week_ES),
    names_to = c("period", "country"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  mutate(
    country = case_when(
      country == "PT" ~ "Portugal",
      country == "ES" ~ "Spain",
      TRUE ~ country
    ),
    period = case_when(
      period == "Blackout"    ~ "Blackout",
      period == "After2days"  ~ "2 days after",
      period == "After1week"  ~ "1 week after",
      TRUE ~ period
    )
  )


# 4. Split value into estimate / lower / upper numeric columns
df_final <- df_country_long %>%
  # value is like "7 (-3, 17)"
  mutate(
    estimate = str_extract(value, "^-?\\d+"),
    ci       = str_extract(value, "\\(-?\\d+,\\s*-?\\d+\\)")
  ) %>%
  mutate(
    estimate = as.numeric(estimate),
    ci       = str_remove_all(ci, "[()]"),
    lower    = as.numeric(str_trim(str_split_fixed(ci, ",", 2)[, 1])),
    upper    = as.numeric(str_trim(str_split_fixed(ci, ",", 2)[, 2]))
  ) %>%
  select(age, sex, country, period, estimate, lower, upper)

##
## and now the plot

library(ggplot2)
library(dplyr)

# ensure age has a sensible order
df_final <- df_final %>%
  mutate(
    age = factor(age, levels = c("65<", "65-84", ">84", "Total")),
    sex = factor(sex, levels = c("Males", "Females", "Total")),
    period = factor(period, levels = c("Blackout", "2 days after", "1 week after"))
  )

ggplot(
  df_final,  # example: drop "Total" if you want
  aes(x = age, y = estimate, color = sex, group = sex)
) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(
    aes(ymin = lower, ymax = upper),
    position = position_dodge(width = 0.4),
    size = 0.2
  ) +
  scale_colour_viridis_d(option = "B", end = .6) + 
  facet_grid(country ~ period) +   # rows = country, columns = period
  labs(
    x = "Age group",
    y = "Excess deaths (estimate and 95% CrI)",
    color = "Sex"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(filename = "output/fig_TAB1.png", dpi = 300, width = 8.5, height = 4)



##
## Age group histograms by sex


cntr <- "ESP"
datESP <- readRDS(paste0("output/FinalData_", cntr, ".rds"))
datESP <- datESP[!is.na(datESP$deaths),]

cntr <- "PRT"
datPRT <- readRDS(paste0("output/FinalData_", cntr, ".rds"))
datPRT <- datPRT[!is.na(datPRT$deaths),]

rbind(
  datPRT %>% 
    dplyr::group_by(age, sex) %>% 
    dplyr::summarise(deaths = sum(deaths),.groups = "drop") %>% 
    dplyr::mutate(country = "Portugal"),
  
  datESP %>% 
    group_by(age, sex) %>% 
    summarise(deaths = sum(deaths),.groups = "drop") %>% 
    dplyr::mutate(country = "Spain")
) %>% 
  ggplot(aes(x = age, y = deaths)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_comma()) + 
  facet_grid(cols = vars(sex), rows = vars(country), scales = "free_y") + theme_bw()

ggsave(filename = "output/fig_sex.png", dpi = 300, width = 8.5, height = 6)


##
## Sensitivity 1


# cntr <- "PRT"
# cntr <- "ESP"
dlnm_nam <- "_dlnm"

res_form_prt <- readRDS(file = paste0("output/RES_MAIN_BYM2_", "PRT", dlnm_nam, "_bym.rds"))
res_form_esp <- readRDS(file = paste0("output/RES_MAIN_BYM2_", "ESP", dlnm_nam, "_bym.rds"))

ExtractResults <- function(Y){
  # Y is the actual model result which needs to run by week and age*sex
  
  do.call(c, Y$predictions) -> pred_combined
  pred_combined <- data.frame(truth = rep(Y$true_values, 
                                          times = Y$predictions %>% length()), 
                              predictions = pred_combined)
  
  pred.samples <- do.call(cbind, Y$prediction)
  true_values <- Y$true_values
  tmp <- Y$datainfo
  tmp %>% select(!starts_with("V")) %>% # just to make sure there is nothing starting with V not to mess up the pred values
    cbind(., as.data.frame(pred.samples)) %>% 
    as.data.frame() %>% 
    mutate(true_values = true_values) -> tmp
  
  tmp %>% dplyr::filter(year >= 2025) -> tmp
  
  return(tmp)
}

getSums <- function(X, res){
  
  res %>% 
    dplyr::group_by_at(X) %>% 
    summarise(across(c('deaths', paste0("V", 1:1000)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:1000, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.025, 0.05, 0.10, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.975)))
  
  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:1000, "_1"))), df_plot)
  
  return(df_plot)
}


N <- length(res_form_esp)
lapply(res_form_esp, ExtractResults) %>% do.call(rbind, .) -> res_esp
lapply(res_form_prt, ExtractResults) %>% do.call(rbind, .) -> res_prt


fig1 <- function(res, title){
  ggplot(
    data = getSums(res = res, c("date")) %>% 
      mutate(age = "Total", sex = "Total")
  ) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) + ylab("") + xlab("") + 
    facet_grid(cols = vars(sex), rows = vars(age)) + 
    scale_x_date(date_labels = "%d.%m") +
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    ggtitle(title) + 
    theme_bw() -> p1
  
  
  ggplot(data = getSums(res = res, c("date", "sex")) %>% 
           mutate(age = "Total", 
                  sex = ifelse(sex=="female", "Females", "Males")  %>% 
                    factor(., levels = c("Males", "Females")))) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) + ylab("") + xlab("") + 
    facet_grid(rows = vars(sex), cols = vars(age)) + 
    scale_x_date(date_labels = "%d.%m") +
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    theme_bw() -> p2
  
  
  
  ggplot(data = 
           getSums(res = res, c("date", "age")) %>% 
           mutate(sex = "Total",
                  age = factor(age, levels = c("0-64", "65-84", "85+"), 
                               labels = c("65<", "65-84", ">84")))
         
  ) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_point(aes(x=date, y=deaths_1)) +
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    facet_grid(cols = vars(age), rows = vars(sex)) + theme_bw() + 
    ylab("") + xlab("") + 
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    scale_x_date(date_labels = "%d.%m") -> p3
  
  
  ggplot(data = 
           getSums(res = res, c("date", "age", "sex")) %>%  
           dplyr::mutate(sex = 
                           ifelse(sex=="female", "Females", "Males") %>% 
                           factor(., levels = c("Males", "Females")), 
                         age = factor(age, levels = c("0-64", "65-84", "85+"), 
                                      labels = c("65<", "65-84", ">84")))
         
  ) +  
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) +
    #  geom_point(aes(x=date, y=`50%`), col = "red") + 
    facet_grid(cols = vars(age), rows = vars(sex)) + 
    ylab("") + xlab("") + 
    theme_bw() + 
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    scale_x_date(date_labels = "%d.%m") -> p4
  
  return(list(p1, p2, p3, p4))
}

fig1_esp <- fig1(res=res_esp, title = "b. Spain")
fig1_prt <- fig1(res=res_prt, title = "a. Portugal")


((((fig1_prt[[1]])|(fig1_prt[[3]])) + plot_layout(widths = c(1, 2)))/
    ((fig1_prt[[2]]|fig1_prt[[4]]) + plot_layout(widths = c(1, 2))))/
  (((fig1_esp[[1]])|(fig1_esp[[3]])) + plot_layout(widths = c(1, 2)))/
  ((fig1_esp[[2]]|fig1_esp[[4]]) + plot_layout(widths = c(1, 2)))

ggsave(filename = "output/figSens1.png", dpi = 300, width = 8, height = 9)
dev.off()



##
## Sensitivity 2

# cntr <- "PRT"
# cntr <- "ESP"

res_form_prt <- readRDS(file = paste0("output/RES_MAIN_", "PRT_dlnm2.rds"))
res_form_esp <- readRDS(file = paste0("output/RES_MAIN_", "ESP_dlnm2.rds"))

ExtractResults <- function(Y){
  # Y is the actual model result which needs to run by week and age*sex
  
  do.call(c, Y$predictions) -> pred_combined
  pred_combined <- data.frame(truth = rep(Y$true_values, 
                                          times = Y$predictions %>% length()), 
                              predictions = pred_combined)
  
  pred.samples <- do.call(cbind, Y$prediction)
  true_values <- Y$true_values
  tmp <- Y$datainfo
  tmp %>% select(!starts_with("V")) %>% # just to make sure there is nothing starting with V not to mess up the pred values
    cbind(., as.data.frame(pred.samples)) %>% 
    as.data.frame() %>% 
    mutate(true_values = true_values) -> tmp
  
  tmp %>% dplyr::filter(year >= 2025) -> tmp
  
  return(tmp)
}

getSums <- function(X, res){
  
  res %>% 
    dplyr::group_by_at(X) %>% 
    summarise(across(c('deaths', paste0("V", 1:1000)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:1000, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.025, 0.05, 0.10, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.975)))
  
  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:1000, "_1"))), df_plot)
  
  return(df_plot)
}


N <- length(res_form_esp)
lapply(res_form_esp, ExtractResults) %>% do.call(rbind, .) -> res_esp
lapply(res_form_prt, ExtractResults) %>% do.call(rbind, .) -> res_prt


fig1 <- function(res, title){
  ggplot(
    data = getSums(res = res, c("date")) %>% 
      mutate(age = "Total", sex = "Total")
  ) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) + ylab("") + xlab("") + 
    facet_grid(cols = vars(sex), rows = vars(age)) + 
    scale_x_date(date_labels = "%d.%m") +
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    ggtitle(title) + 
    theme_bw() -> p1
  
  
  ggplot(data = getSums(res = res, c("date", "sex")) %>% 
           mutate(age = "Total", 
                  sex = ifelse(sex=="female", "Females", "Males")  %>% 
                    factor(., levels = c("Males", "Females")))) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) + ylab("") + xlab("") + 
    facet_grid(rows = vars(sex), cols = vars(age)) + 
    scale_x_date(date_labels = "%d.%m") +
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    theme_bw() -> p2
  
  
  
  ggplot(data = 
           getSums(res = res, c("date", "age")) %>% 
           mutate(sex = "Total",
                  age = factor(age, levels = c("0-64", "65-84", "85+"), 
                               labels = c("65<", "65-84", ">84")))
         
  ) + 
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_point(aes(x=date, y=deaths_1)) +
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    facet_grid(cols = vars(age), rows = vars(sex)) + theme_bw() + 
    ylab("") + xlab("") + 
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    scale_x_date(date_labels = "%d.%m") -> p3
  
  
  ggplot(data = 
           getSums(res = res, c("date", "age", "sex")) %>%  
           dplyr::mutate(sex = 
                           ifelse(sex=="female", "Females", "Males") %>% 
                           factor(., levels = c("Males", "Females")), 
                         age = factor(age, levels = c("0-64", "65-84", "85+"), 
                                      labels = c("65<", "65-84", ">84")))
         
  ) +  
    geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
    geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
    geom_line(aes(x=date, y=`50%`), col = "blue", alpha = 0.2) + 
    geom_point(aes(x=date, y=deaths_1)) +
    #  geom_point(aes(x=date, y=`50%`), col = "red") + 
    facet_grid(cols = vars(age), rows = vars(sex)) + 
    ylab("") + xlab("") + 
    theme_bw() + 
    theme(plot.margin = unit(c(-1,-1,-1,-1), "cm")) + 
    scale_x_date(date_labels = "%d.%m") -> p4
  
  return(list(p1, p2, p3, p4))
}

fig1_esp <- fig1(res=res_esp, title = "B. Spain")
fig1_prt <- fig1(res=res_prt, title = "A. Portugal")


((((fig1_prt[[1]])|(fig1_prt[[3]])) + plot_layout(widths = c(1, 2)))/
    ((fig1_prt[[2]]|fig1_prt[[4]]) + plot_layout(widths = c(1, 2))))/
  (((fig1_esp[[1]])|(fig1_esp[[3]])) + plot_layout(widths = c(1, 2)))/
  ((fig1_esp[[2]]|fig1_esp[[4]]) + plot_layout(widths = c(1, 2)))

ggsave(filename = "output/figSens2.png", dpi = 300, width = 8, height = 9)
dev.off()


##
##

cntr <- "ESP"
datESP <- readRDS(paste0("output/FinalData_", cntr, ".rds"))
datESP <- datESP[!is.na(datESP$deaths),]

cntr <- "PRT"
datPRT <- readRDS(paste0("output/FinalData_", cntr, ".rds"))
datPRT <- datPRT[!is.na(datPRT$deaths),]

shp_fileESP <- list.files(paste0(path, "/output/"), 
                          pattern = paste0("\\_", "ESP", ".shp$"), full.names = TRUE)
shp_fileESP <- read_sf(shp_fileESP)

shp_fileESP$LEVL_CODE <- shp_fileESP$NUTS_ID <- shp_fileESP$URBN_TYPE <- shp_fileESP$MOUNT_TYPE <- 
  shp_fileESP$COAST_TYPE <- shp_fileESP$geo <- shp_fileESP$NAME_LATN <- NULL
  


shp_filePRT <- list.files(paste0(path, "/output/"), 
                          pattern = paste0("\\_", "PRT", ".shp$"), full.names = TRUE)
shp_filePRT <- read_sf(shp_filePRT)
shp_filePRT$x <- NULL
shp_filePRT$CNTR_CODE <- "PT"
shp_filePRT$NUTS_NAME <- shp_filePRT$n2_L_20
shp_filePRT <- shp_filePRT[,c("CNTR_CODE", "NUTS_NAME", "geometry")]
datPRT$NUTSII %>% unique()
shp_filePRT$NUTS_NAME[shp_filePRT$NUTS_NAME %in% "Centro (PT)"] <- "Centro"

shp <- rbind(shp_filePRT, shp_fileESP)

rbind(
  datESP %>% 
    dplyr::group_by(year, NUTSII, age, sex) %>% 
    dplyr::filter(year < 2025) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = mean(pop)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, NUTSII) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = sum(pop)) %>% 
    dplyr::mutate(MR = deaths/pop*1000), 
  datPRT %>% 
    dplyr::group_by(year, NUTSII, age, sex) %>% 
    dplyr::filter(year < 2025) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = mean(pop)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, NUTSII) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = sum(pop)) %>% 
    dplyr::mutate(MR = deaths/pop*1000) 
) %>% 
  left_join(shp, by = c("NUTSII" = "NUTS_NAME")) -> 
  map_plots

map_plots <- sf::st_as_sf(map_plots)


map_plots %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    MR_cat =  cut(
      MR,
      breaks = quantile(MR, 
                        probs = seq(0, 1, 0.2), 
                        na.rm = TRUE),
      include.lowest = TRUE
    )) %>% 
  ggplot() + 
  geom_sf(aes(fill = MR_cat)) + 
  theme_bw() + 
  facet_wrap(vars(year), nrow = 5) + 
  theme_bw() + 
  scale_fill_viridis_d(option = "E", alpha = 0.9) +
  theme(legend.title = element_blank(), 
        text = element_text(size = 10)) 
  
  
ggsave(filename = "output/sup_MR.png", dpi = 300, width = 8.5, height = 10)




rbind(
  datESP %>% 
    dplyr::group_by(year, NUTSII, age, sex) %>% 
    dplyr::filter(year < 2025) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = mean(pop)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, NUTSII) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = sum(pop)) %>% 
    dplyr::mutate(MR = deaths/pop*1000), 
  datPRT %>% 
    dplyr::group_by(year, NUTSII, age, sex) %>% 
    dplyr::filter(year < 2025) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = mean(pop)) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year, NUTSII) %>% 
    dplyr::summarise(
      deaths = sum(deaths), 
      pop = sum(pop)) %>% 
    dplyr::mutate(MR = deaths/pop*1000) 
) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(NUTSII, ) %>% 
  dplyr::summarise(
    deaths_sum = sum(deaths),
    deaths_mean = mean(deaths), 
    pop = mean(pop), 
    MR = deaths_mean/pop*1000,
  ) -> tb

tb %>% xtable::xtable()
tb$pop %>% sum()
