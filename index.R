#' ---
#' title: blackout-burden
#' author: jriou
#' date: 2025-05-01
#' ---


# Set-up ------------------------------------------------------------------

controls = list(
  analysis_date = "2025-05-20",
  update_data=TRUE # download all external data (only the first time)
)
source("R/setup.R")

# Block 1: data preparation -----------------------------------------------

## 1.1: load all-cause mortality data
bo_1 = bo_000_load()
# saveRDS(bo_1, file = "output/deaths21_25.rds")

## 1.2: identify weeks with more than `threshold` deaths due to COVID-19 in the country
bo_2 = bo_001_covid_deaths(bo_1,threshold=100) 

## 1.3: Imperial part
if(FALSE) {
  source("R/01_Download_ERA5.R")
  ...
}

# Block 2: description ----------------------------------------------------


# Block 3: inference ------------------------------------------------------
