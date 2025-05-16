
# Created 14.05.2025

# Run INLA

#-------------------------------------------------------------------------------

library(tidyverse)
library(INLA)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/Desktop/Portugal/"
setwd(path)

dat <- readRDS("output/FinalData.rds")

# set he inla timeout in sec
inla.setOption(inla.timeout=30000)

# inla group for the temperature
dat$id.temp <- inla.group(dat$temperature_lag0, n = 30)
# space*year interaction
dat <- dat %>% 
  mutate(id.spaceyear = paste0(year, NUTSII) %>% as.factor() %>% as.numeric())

dat$xweek <- as.numeric(dat$week)

# set the iid priors
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.1)))
hyper.iid.str <- list(theta = list(prior="pc.prec", param=c(0.01, 0.1)))

# Inla run
RunINLA <- function(form, dat_cv, n_sam = 200){
  
  set.seed(11)
  ind <- which(is.na(dat_cv$death_mod))
  
  tryCatch(
    #this is the chunk of code we want to run
    {
      mod <- inla(as.formula(form), 
                  data = dat_cv, 
                  family = "poisson", 
                  verbose = FALSE, 
                  control.compute=list(config = TRUE), 
                  control.mode=list(restart=TRUE),
                  num.threads = 1, 
                  control.predictor = list(link = 1))
      
      # mod <- inla.rerun(mod)
      
      post.samples <- inla.posterior.sample(n = n_sam, result = mod)
      lp <- lapply(post.samples, function(X) exp(X$latent[ind]))
      
      data_return <- list(
        true_values =  dat_cv$deaths[ind],
        predictions = lapply(lp, function(X) rpois(n = length(ind), 
                                                   lambda = X)), 
        datainfo =  dat_cv[ind,]
      )
      data_return %>% return()
      #when it throws an error, the following block catches the error
    }, error = function(msg){
      return(NA)
    })
} 
# Cross-validation datasets
DataCrossVal <- function(ageg, sexg, COVID=FALSE, 
                         start_date = "2025-01-01", 
                         end_date = "2025-03-31"){
  
  # subset age*sex
  dat %>% dplyr::filter(age %in% ageg, sex %in% sexg) -> dat_sexage
  
  # exclude days with high covid
  dat_sexage$death_mod <- dat_sexage$deaths
  if(COVID==FALSE){ # This excludes dates when covid deaths were larger than 100
    dat_sexage$death_mod[dat_sexage$OWID_covid_deaths == 1] <- NA
  }
  
  # select the weeks
  week_start <- dat_sexage %>% filter(date %in% start_date) %>% pull(week) %>% unique()
  week_end <- dat_sexage %>% filter(date %in% end_date) %>% pull(week) %>% unique()
  
  lapply(week_start:week_end, function(Z){
    dat_sexage %>% 
      filter(week <= Z) %>% 
      mutate(death_mod = ifelse(week == Z, NA, death_mod)) %>% return()
  })%>% return()
}

# All possible combinations of age and sex
list.sc <- expand.grid(age = as.character(unique(dat$age)), sex = as.character(unique(dat$sex)))

# get the datasets
dat_cv_list <- apply(
  list.sc, 
  1, 
  function(X) DataCrossVal(ageg = X[1], sexg =X[2])
)

# Define the INLA formulas
# weekend vs weekday
form_1 <- 
  death_mod ~ 
  1 + 
  offset(log(pop)) + 
  factor(hol) + 
  # seasonality
  f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
  # long-term trends
  f(week, model='iid', constr = TRUE, hyper = hyper.iid) + 
  # temperature effect
  f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
  # space
  f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) 
   
form_2 <- 
  death_mod ~ 
  1 + 
  offset(log(pop)) + 
  factor(hol) + 
  xweek + 
  # seasonality
  f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
  # long-term trends
  f(week, model='iid', constr = TRUE, hyper = hyper.iid) + 
  # temperature effect
  f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
  # space
  f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) 


# form_2 <- 
#   death_mod ~ 
#   1 + 
#   offset(log(pop)) + 
#   factor(hol) + 
#   xweek + 
#   # seasonality
#   f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
#   # long-term trends
#   f(week, model='iid', constr = TRUE, hyper = hyper.iid) + 
#   # temperature effect
#   f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
#   # space
#   f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) +
#   # space*year interaction
#   f(id.spaceyear, model='iid', constr = TRUE, hyper = hyper.iid)


form_3 <- 
  death_mod ~ 
  1 + 
  offset(log(pop)) + 
  factor(hol) + 
  # seasonality
  f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
  # long-term trends
  f(week, model='rw2', constr = TRUE, hyper = hyper.iid) + 
  # temperature effect
  f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
  # space
  f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) 


res_form_1 <- list()
res_form_2 <- list()
res_form_3 <- list()

t_0 <- Sys.time()
for(i in 1:length(dat_cv_list)){
  print(i)
  # res_form_1[[i]] <- lapply(dat_cv_list[[i]], RunINLA, form = form_1)
  # res_form_2[[i]] <- lapply(dat_cv_list[[i]], RunINLA, form = form_2)
  res_form_3[[i]] <- lapply(dat_cv_list[[i]], RunINLA, form = form_3)
}
t_1 <- Sys.time()
t_1 - t_0


saveRDS(res_form_1, file = "output/CV_FORM1.rds")
saveRDS(res_form_2, file = "output/CV_FORM2.rds")
saveRDS(res_form_3, file = "output/CV_FORM3.rds")

# t_0 <- Sys.time()
# tmp <- RunINLA(form = form_1, dat_cv = dat_cv_list[[i]][[1]], n_sam = 200)
# t_1 <- Sys.time()
# t_1 - t_0 # ~ 20 seconds for 1, I need to run 3*12*14 (14 is the number of the first weeks in 2025 until end of March)
# # I expect ~ 3 hours


rm(list = ls())
dev.off()
gc()

