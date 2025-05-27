
# Created 17.05.2025

# Run the best performing model

#-------------------------------------------------------------------------------

library(tidyverse)
library(INLA)
library(dlnm)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

dlnm = TRUE

# cntr <- "PRT"
cntr <- "ESP"

dat <- readRDS(paste0("output/FinalData_", cntr, ".rds"))
dat <- dat[!is.na(dat$deaths),]

# set he inla timeout in sec
inla.setOption(inla.timeout=30000)

# inla group for the temperature
dat$id.temp <- inla.group(dat$temperature_lag0, n = 30)

# add a dlnm component:
if(dlnm == TRUE){
  ## lag basis functions argument list
  n.lags <- 21
  blag.args <- list(
    fun = "bs",
    df = 5,
    degree = 2
  )
  blag.args$knots <- do.call(
    "equalknots", c(list(x = 0:n.lags), blag.args))
  blag.args$knots 
  
  ## temperature basis functions argument list
  btemp.args <- list(
    fun = "bs",
    df = 5,
    degree = 2)
  btemp.args$knots <- do.call(
    "equalknots", c(list(x = dat$temperature_lag0), btemp.args))
  btemp.args$knots
  
  ## apply crossbasis for lag0 temperature time series from each location
  ## Note that your data is by age and sex, thus you need to account for this
  dat_noagesex <- 
    dat %>% dplyr::filter(age %in% "85+", sex %in% "female")
  
  btemp.locs <- lapply(
    split(x = dat_noagesex[c("date", "NUTSII", "temperature_lag0")],
          f = dat_noagesex$NUTSII), function(d) {
            data.frame(
              d,
              crossbasis(d$temperature_lag0,
                         lag = n.lags,
                         argvar = btemp.args,
                         arglag = blag.args)
            )
          }
  )
  btemp1 <- do.call("rbind", btemp.locs)
  summary(btemp1) # 126 NAs, which are likely the ones we are missing from the start of the
  # study. Lets caclulate 6 NUTSII and 21 lags
  
  dim(btemp1)
  names(btemp1)
  
  head(btemp1, 2)
  tail(btemp1, 2)
  
  ## number of basis functions
  (nbt1 <- ncol(btemp1) - 3)
  
  btemp1$temperature_lag0 <- NULL
  rownames(btemp1) <-  NULL
  
  dat <- left_join(dat, btemp1)
  summary(dat) # this needs to have 6*6*21 NAs
}

# space*year interaction
dat <- dat %>% 
  mutate(id.spaceyear = paste0(year, NUTSII) %>% as.factor() %>% as.numeric())

dat$xweek <- as.numeric(dat$week)
dat$id.day <- lubridate::wday(dat$date)

# set the iid priors
hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.1)))

# Inla run
RunINLA <- function(form, dat_agesex, n_sam = 1000){
  
  set.seed(11)
  ind <- which(is.na(dat_agesex$death_mod))
  
  tryCatch(
    #this is the chunk of code we want to run
    {
      mod <- inla(as.formula(form), 
                  data = dat_agesex, 
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
        true_values =  dat_agesex$deaths[ind],
        predictions = lapply(lp, function(X) rpois(n = length(ind), 
                                                   lambda = X)), 
        datainfo =  dat_agesex[ind,]
      )
      data_return %>% return()
      #when it throws an error, the following block catches the error
    }, error = function(msg){
      return(NA)
    })
} 



# Define age*sex group and the prediction sex
DataAgeSex <- function(ageg, sexg, COVID=FALSE, 
                         start_date = "2025-04-28", 
                         end_date = "2025-05-04"){
  
  # subset age*sex
  dat %>% dplyr::filter(age %in% ageg, sex %in% sexg) -> dat_sexage
  
  # exclude days with high covid
  dat_sexage$death_mod <- dat_sexage$deaths
  if(COVID==FALSE){ # This excludes dates when covid deaths were larger than 100
    dat_sexage$death_mod[dat_sexage$OWID_covid_deaths == 1] <- NA
  }
  
  date_sub <- 
    seq.Date(from = as.Date(start_date), 
             to = as.Date(end_date), by = "day")
  
  dat_sexage %>% 
    filter(date <= as.Date(end_date)) %>% 
    mutate(death_mod = ifelse(date %in% date_sub, NA, death_mod)) %>% 
    return()
}

# All possible combinations of age and sex
list.sc <- expand.grid(age = as.character(unique(dat$age)), sex = as.character(unique(dat$sex)))

# get the datasets
dat_cv_list <- apply(
  list.sc, 
  1, 
  function(X) DataAgeSex(ageg = X[1], sexg =X[2])
)

form <- 
  death_mod ~ 
  1 + 
  offset(log(pop)) + 
  factor(hol) + 
  # day of the week
  f(id.day, model='iid', constr = TRUE, hyper = hyper.iid) + 
  # seasonality
  f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
  # long-term trends
  f(week, model='rw2', constr = TRUE, hyper = hyper.iid) + 
  # temperature effect
  f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
  # space
  f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) 

if(dlnm == TRUE){
  
  bt1.terms <- colnames(btemp1)[colnames(btemp1) %>% startsWith(prefix = "v")]
  
  update(
    form,
    paste(".~. -f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) +
          ", paste(bt1.terms, collapse = "+"))
  ) -> form
  
  dlnm_nam <- "_dlnm"
}

t_0 <- Sys.time()
res_form <- lapply(dat_cv_list, RunINLA, form = form)
t_1 <- Sys.time()
t_1 - t_0 # 5 minutes for Portugal, 10 minutes for Spain.


saveRDS(res_form, file = paste0("output/RES_MAIN_", cntr, dlnm_nam, ".rds"))

rm(list = ls())
dev.off()
gc()


# sensitivity
if(sens == TRUE){
  form_2 <- 
    death_mod ~ 
    1 + 
    offset(log(pop)) + 
    factor(hol) + 
    xweek + 
    # day of the week
    f(id.day, model='iid', constr = TRUE, hyper = hyper.iid) + 
    # seasonality
    f(yweek, model='rw2', constr = TRUE, cyclic = TRUE, hyper = hyper.iid) + 
    # long-term trends
    f(week, model='iid', constr = TRUE, hyper = hyper.iid) + 
    # temperature effect
    f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) + 
    # space
    f(id.space, model='iid', constr = TRUE, hyper = hyper.iid) 
  
  
  if(dlnm == TRUE){
    
    bt1.terms <- colnames(btemp1)[colnames(btemp1) %>% startsWith(prefix = "v")]
    
    update(
      form_2,
      paste(".~. -f(id.temp, model='rw2', hyper=hyper.iid, constr = TRUE, scale.model = TRUE) +
          ", paste(bt1.terms, collapse = "+"))
    ) -> form_2
    
    dlnm_nam <- "_dlnm"
  }
  
  t_0 <- Sys.time()
  res_form <- lapply(dat_cv_list, RunINLA, form = form_2)
  t_1 <- Sys.time()
  t_1 - t_0 # 5 minutes
  
  saveRDS(res_form, file = paste0("output/RES_MAIN_form2_", cntr, dlnm_nam, ".rds"))
}
