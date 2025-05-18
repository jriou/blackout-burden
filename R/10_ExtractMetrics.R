
# Created 16.05.2025

# Extract metrics

#-------------------------------------------------------------------------------

library(tidyverse)
library(patchwork)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

cntr <- "PRT"
cntr <- "ESP"

res_form_1 <- readRDS(paste0("output/CV_FORM1_", cntr, ".rds"))
res_form_2 <- readRDS(paste0("output/CV_FORM2_", cntr, ".rds"))
res_form_3 <- readRDS(paste0("output/CV_FORM3_", cntr, ".rds"))

# 12 age groups and 14 weeks of predictions
# Y <- res_form_1[[1]][[2]]
metrics <- function(Y){
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
  
  # focus on 2025
  tmp %>% dplyr::filter(year >= 2025) -> tmp
  
  lapply(list(week = "week", 
              space = "NUTSII", 
              time = "date", 
              space_week = c("week", "NUTSII"), 
              space_date = c("date", "NUTSII")), function(Y){
    
    tmp %>% 
        ungroup() %>% 
        select(starts_with("V"), Y, true_values) %>% 
        group_by_at(Y) %>% 
        dplyr::summarise_all(sum) -> 
      ag_deaths
    
    pois.quant <- as.data.frame(t(apply(ag_deaths[,!(colnames(ag_deaths) %in% 
                                                       c(Y, "true_values"))], 1, function(Z) quantile(Z, probs = c(0.025, 0.975)))))
    
    list(
      bias = sweep(ag_deaths %>% 
                     ungroup() %>% 
                     dplyr::select(!Y), 1, ag_deaths$true_values, "-") %>% 
        select(!true_values) %>% 
        as.data.frame(), 
      mse = sweep(ag_deaths %>% 
                    ungroup() %>% 
                    dplyr::select(!Y), 1, ag_deaths$true_values, "-")^2 %>% 
        select(!true_values) %>% 
        as.data.frame(), 
      coverage = mean((pois.quant$`2.5%` <= ag_deaths$true_values) & 
                        (pois.quant$`97.5%` > ag_deaths$true_values)), 
      pois.quant = pois.quant, 
      true_values = ag_deaths$true_values
    ) %>% return()
  }) -> ret_list
  names(ret_list) <- c("week", "space", "time", "space_week", "space_date")
  ret_list %>% return()
}

# 12 age groups and 14 weeks
t_0 <- Sys.time()
lapply(1:12, function(Z) lapply(res_form_1[[Z]], metrics)) -> cv_form_1
lapply(1:12, function(Z) lapply(res_form_2[[Z]], metrics)) -> cv_form_2
lapply(1:12, function(Z) lapply(res_form_3[[Z]], metrics)) -> cv_form_3
t_1 <- Sys.time()
t_1 - t_0 # 2 minutes


##
## Space*date
# Predicted - truth
Bias <-function(form_res){
  lapply(form_res, function(X){
    lapply(X, function(Z) Z$space_date$bias %>% apply(., 1, mean)) %>% unlist()
  }
  ) %>% return()
}

nams <- sapply(res_form_1, function(Z)
  paste(Z[[1]]$datainfo$age %>% unique(), Z[[1]]$datainfo$sex %>% unique(), sep = "_"))

mean.bias.df.1 <- data.frame(do.call(cbind, Bias(cv_form_1)))
mean.bias.df.2 <- data.frame(do.call(cbind, Bias(cv_form_2)))
mean.bias.df.3 <- data.frame(do.call(cbind, Bias(cv_form_3)))

mean.bias.df.1 %>% apply(., 2, sum)
mean.bias.df.2 %>% apply(., 2, sum)
mean.bias.df.3 %>% apply(., 2, sum)

# boxplot(mean.bias.df.1)
# abline(h = 0, col = "red", lty = 2)
# 
# boxplot(mean.bias.df.2)
# abline(h = 0, col = "red", lty = 2)
# 
# boxplot(mean.bias.df.3)
# abline(h = 0, col = "red", lty = 2)

# sum of bias:

SumBiasQ <-function(form_res){
  lapply(form_res, function(X){
    lapply(X, function(Z) Z$space_date$bias %>% apply(., 2, sum)) %>% 
      do.call(cbind, .) %>% apply(., 1, sum) %>% 
      quantile(., probs = c(0.5, 0.025, 0.975))
  }
  ) %>% return()
}

rbind(
  SumBiasQ(cv_form_1) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 1", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])),
  
  SumBiasQ(cv_form_2) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 2", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])),
  
  SumBiasQ(cv_form_3) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 3", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2]))
  
) -> bias_sum_tab
bias_sum_tab$mod2 <- factor(bias_sum_tab$mod, labels = paste0("M", 1:3)) 
bias_sum_tab$sex <-
  factor(bias_sum_tab$sex, 
         levels = c("male", "female"), 
         labels = c("Males", "Females"))

ggplot(data = bias_sum_tab) + 
  geom_point(aes(x=mod2, y=`50%`)) + 
  geom_errorbar(aes(x=mod2, ymin = `2.5%`, ymax = `97.5%`), width = 0.5) + 
  facet_grid(cols = vars(age), rows = vars(sex)) + 
  geom_hline(yintercept = 0, lty = 2, col = "red") + 
  theme_bw() + ylab("") + xlab("") + 
  ggtitle("A. Bias") + 
  theme(
    strip.text = element_text(margin = margin(t = 1, r = 1, b = 1, l = 1)),
    strip.background = element_rect(fill = NA, color = "black"), 
    plot.title = element_text(size = 10, face = "bold")
  ) -> p1



##
## Now retrieve the coverage


Coverage <-function(form_res){
  lapply(form_res, function(X){
    lapply(X, function(Z) Z$space_date$coverage) %>% unlist()
  }
  ) %>% return()
}

rbind(
  Coverage(cv_form_1) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 1", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])) %>% 
    tidyr::gather(., nam, coverage, V1:V14) %>% 
    select(-nam), 
  Coverage(cv_form_2) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 2", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])) %>% 
    tidyr::gather(., nam, coverage, V1:V14) %>% 
    select(-nam), 
  Coverage(cv_form_3) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(mod = "Model 3", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])) %>% 
    tidyr::gather(., nam, coverage, V1:V14) %>% 
    select(-nam)
) -> coverage_res

coverage_res$mod2 <- factor(coverage_res$mod, labels = paste0("M", 1:3)) 
coverage_res$sex <-
  factor(coverage_res$sex, 
         levels = c("male", "female"), 
         labels = c("Males", "Females"))

ggplot(data = coverage_res) + 
  geom_boxplot(aes(x=mod2, y=coverage)) +
  facet_grid(cols = vars(age), rows = vars(sex)) + 
  geom_hline(yintercept = 0.95, lty = 2, col = "red") + 
  theme_bw() + ylab("") + xlab("") + 
  ggtitle("B. Coverage probability") + 
  theme(
    strip.text = element_text(margin = margin(t = 1, r = 1, b = 1, l = 1)),
    strip.background = element_rect(fill = NA, color = "black"), 
    plot.title = element_text(size = 10, face = "bold")
  ) -> p2


##
## And the MSE


MSE <-function(form_res){
  lapply(form_res, function(X){
    lapply(X, function(Z) Z$space_date$mse %>% apply(., 2, sum)) %>% 
      do.call(cbind, .) %>% apply(., 1, sum) %>% 
      quantile(., probs = c(0.5, 0.025, 0.975))
  }
  ) %>% return()
}


rbind(
  MSE(cv_form_1) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    sqrt() %>% 
    mutate(mod = "Model 1", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])),
  
  MSE(cv_form_2) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    sqrt() %>% 
    mutate(mod = "Model 2", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2])),
  
  MSE(cv_form_3) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    sqrt() %>% 
    mutate(mod = "Model 3", 
           age = sapply(str_split(nams, pattern = "_"), function(X) X[1]), 
           sex = sapply(str_split(nams, pattern = "_"), function(X) X[2]))
  
) -> mse_tab
mse_tab$mod2 <- factor(bias_sum_tab$mod, labels = paste0("M", 1:3)) 
mse_tab$sex <-
  factor(mse_tab$sex, 
         levels = c("male", "female"), 
         labels = c("Males", "Females"))

ggplot(data = mse_tab) + 
  geom_point(aes(x=mod2, y=`50%`)) + 
  geom_errorbar(aes(x=mod2, ymin = `2.5%`, ymax = `97.5%`), width = 0.5) + 
  facet_grid(cols = vars(age), rows = vars(sex)) + 
  theme_bw() + ylab("") + xlab("") + 
  ggtitle("C. Square root of MSE") + 
  theme(
    strip.text = element_text(margin = margin(t = 1, r = 1, b = 1, l = 1)),
    strip.background = element_rect(fill = NA, color = "black"), 
    plot.title = element_text(size = 10, face = "bold")
  ) -> p3

p1/p2/p3  + plot_layout() &  # use ampersand to apply to all
  theme(plot.margin = margin(2, 2, 1, 0)) 

ggsave(paste0("output/CVMetrics_", cntr ,".png"), width = 8, height = 8, dpi = 300)

rm(list = ls())
dev.off()
gc()



