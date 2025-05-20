bo_000_load = function() {
  
  # Portugal
  
  ## Structure
  pt_alldates = readr::read_csv("data/alldates.csv",col_names="date_text")
  pt_deaths0 = NULL
  allyears = 2021:2025
  
  ## Daily deaths
  for(i in allyears) {
    tmp = readr::read_csv2(file.path("data/portugal/",paste0("portugal",i,".csv")), 
                           col_names = c("year", "nuts2", "sex","age_group","total", pull(pt_alldates,date_text)),
                           skip=10) %>% 
      dplyr::select(-total) %>% 
      tidyr::fill(year,nuts2,sex,.direction="down") %>% 
      dplyr::filter(year==i) %>% 
      tidyr::pivot_longer(5:383,names_to="date_text", values_to="deaths") %>% 
      dplyr::mutate(year=i,
                    deaths=as.numeric(gsub(" //","",deaths)),
                    date_text=paste0(date_text," ",i),
                    date=lubridate::dmy(date_text))
    pt_deaths0 = dplyr::bind_rows(pt_deaths0,tmp)
  }
  
  ## Clean-up
  pt_deaths1 = pt_deaths0 %>% 
    dplyr::filter(!is.na(date),!is.na(deaths)) %>% 
    # NUTS2
    tidyr::separate(nuts2, into=c("nuts2_code","nuts2_name"),sep=":") %>% 
    dplyr::mutate(nuts2_code=ifelse(nuts2_code=="1B: Pen\xednsula de Set\xfabal","1B",nuts2_code),
                  nuts2_name=ifelse(nuts2_code=="1B","Península de Setúbal",nuts2_name)) %>% 
    dplyr::mutate(country="Portugal") %>% 
    dplyr::relocate(country) %>% 
    # Dates
    dplyr::mutate(dm = sprintf("%02d-%02d", day(date), month(date)),
                  dm_index = yday(date),
                  isoweek=substr(ISOweek::date2ISOweek(date),1,8)) %>% 
    dplyr::relocate(year,dm_index,date,dm,isoweek) %>% 
    # Sex
    dplyr::mutate(sex=as.factor(sex)) %>% 
    # Age
    dplyr::mutate(age_group=case_when(age_group=="0-64 years" ~ "0-64",
                                      age_group=="65-69 years" ~ "65-69",
                                      age_group=="70-74 years" ~ "70-74",
                                      age_group=="75-79 years" ~ "75-79",
                                      age_group=="80-84 years" ~ "80-84",
                                      age_group=="85 or more years" ~ "85+")) 
  
  ## Checks
  if(FALSE) {
    summary(pt_deaths1)
    ggplot(pt_deaths1) +
      geom_point(aes(x=dm_index,y=deaths,colour=year)) +
      facet_grid(age_group~year)
  }
  
  # Spain
  es_deaths1 = NULL
  
  # Join
  dat_out = dplyr::bind_rows(pt_deaths1,es_deaths1)
  return(dat_out)

}

