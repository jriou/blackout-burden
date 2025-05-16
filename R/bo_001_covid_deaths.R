bo_001_covid_deaths = function(dat_in, threshold = 100) {
  
  # OWID death data ()
  if(controls$update_data) {
    owid_url = "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    dd_0 = readr::read_csv(owid_url)
    readr::write_csv(dd_0,file=file.path(controls$savepoint,"OWID_data.csv"))
  } else {
    dd_0 = readr::read_csv(file.path(controls$savepoint,"OWID_data.csv"))
  }

  
  # Filter for Portugal and Spain and select relevant columns
  dd_1 = dd_0 %>%
    dplyr::filter(location %in% c("Portugal","Spain")) %>%
    dplyr::select(date, country=location, OWID_covid_deaths=new_deaths) %>% 
    dplyr::filter(OWID_covid_deaths>0)
    
  # Identify weeks with at least `threshold` COVID-19 deaths
  dd_2 = dd_1 %>% 
    dplyr::mutate(isoweek=substr(ISOweek::date2ISOweek(date),1,8)) %>% 
    dplyr::group_by(country,isoweek) %>% 
    dplyr::summarise(OWID_covid_deaths=sum(OWID_covid_deaths),
                     .groups="drop") %>% 
    dplyr::mutate(OWID_covid_deaths=if_else(OWID_covid_deaths>threshold,1,0))
    
  return(dd_2)
}

setwd("C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/")
saveRDS(dd_2, file = "output/COVID_DEATHS.rds")
