

# Created 21.05.2025

# Extract the main results

#-------------------------------------------------------------------------------

library(tidyverse)
library(INLA)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

cntr <- "PRT"
cntr <- "ESP"

res_form <- readRDS(file = paste0("output/RES_MAIN_", cntr, ".rds"))

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

sapply(1:12, function(Z) lapply(res_form, ExtractResults)) -> res
do.call(rbind, res) -> res

##
## lets do totals by day

getSums <- function(X){
  
  res %>% 
    dplyr::group_by_at(X) %>% 
    summarise(across(c('deaths', paste0("V", 1:200)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:200, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.025, 0.05, 0.10, 0.2, 0.3, 0.7, 0.8, 0.9, 0.95, 0.975)))

  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:200, "_1"))), df_plot)
  
  return(df_plot)
}




ggplot(data = getSums(c("date", "sex"))) + 
  geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
  geom_point(aes(x=date, y=deaths_1)) + 
  facet_grid(cols = vars(sex)) + 
  theme_bw()

ggplot(data = getSums(c("date", "age"))) + 
  geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
  geom_point(aes(x=date, y=deaths_1)) +
  facet_grid(cols = vars(age)) + theme_bw() + ylab("Observed and predicted mortality")


ggplot(data = getSums(c("date", "age", "sex"))) + 
  geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
  geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
  geom_point(aes(x=date, y=deaths_1)) +
#  geom_point(aes(x=date, y=`50%`), col = "red") + 
  facet_grid(cols = vars(age), rows = vars(sex)) + 
  ylab("Observed and predicted mortality") + 
  theme_bw()
