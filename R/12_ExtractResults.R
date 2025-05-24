

# Created 21.05.2025

# Extract the main results

#-------------------------------------------------------------------------------

library(tidyverse)
library(INLA)
library(patchwork)
library(xtable)


# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

cntr <- "PRT"
cntr <- "ESP"

res_form <- readRDS(file = paste0("output/RES_MAIN_", cntr, ".rds"))
# res_form <- readRDS(file = paste0("output/RES_MAIN_form2_", cntr, ".rds"))

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

N <- length(res_form)
sapply(1:N, function(Z) lapply(res_form, ExtractResults)) -> res
do.call(rbind, res) -> res




##
## lets do totals by day

getSums <- function(X){
  
  res %>% 
    dplyr::group_by_at(X) %>% 
    summarise(across(c('deaths', paste0("V", 1:200)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:200, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.025, 0.05, 0.10, 0.2, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.975)))

  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:200, "_1"))), df_plot)
  
  return(df_plot)
}


ggplot(
  data = getSums(c("date")) %>% 
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
  theme_bw() -> p1


ggplot(data = getSums(c("date", "sex")) %>% 
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
         getSums(c("date", "age")) %>% 
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
         getSums(c("date", "age", "sex")) %>%  
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

(((p1)|(p3)) + plot_layout(widths = c(1, 2)))/
  ((p2|p4) + plot_layout(widths = c(1, 2))) 


ggsave(filename = "output/fig1.png", dpi = 300, width = 8)

# +
#   plot_annotation('Observed and expected mortality', 
#                   theme=theme(plot.title=element_text(hjust=0.5)))
# 


# d1 = getSums(c("date")) %>% mutate(age = "Total", sex = "Total") %>% dplyr::select(date, age, sex, deaths_1, `2.5%`:`97.5%`)
# d2 = getSums(c("date", "sex")) %>% mutate(age = "Total") %>% dplyr::select(date, age, sex, deaths_1, `2.5%`:`97.5%`)
# d3 = getSums(c("date", "age")) %>% mutate(sex = "Total") %>% dplyr::select(date, age, sex, deaths_1, `2.5%`:`97.5%`)
# d4 = getSums(c("date", "age", "sex")) %>% dplyr::select(date, age, sex, deaths_1, `2.5%`:`97.5%`)
# 
# d <- rbind(d1, d2, d3, d4)
# 
# ggplot(data = d) + 
#   geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), fill = "blue", alpha = 0.1) + 
#   geom_ribbon(aes(x = date, ymin = `5%`, ymax = `95%`), fill = "blue", alpha = 0.1) + 
#   geom_ribbon(aes(x = date, ymin = `10%`, ymax = `90%`), fill = "blue", alpha = 0.1) + 
#   geom_ribbon(aes(x = date, ymin = `20%`, ymax = `80%`), fill = "blue", alpha = 0.1) + 
#   geom_ribbon(aes(x = date, ymin = `30%`, ymax = `70%`), fill = "blue", alpha = 0.1) + 
#   geom_point(aes(x=date, y=deaths_1)) +
#   #  geom_point(aes(x=date, y=`50%`), col = "red") + 
#   facet_grid(cols = vars(sex), rows = vars(age)) + 
#   ylab("Observed and predicted mortality") + 
#   theme_bw()


##
## Lets calculate excess mortality

# absolute across the country

res %>% 
  dplyr::select(date, NUTSII, age, sex, true_values) %>% 
  cbind(., 
        sweep(
          x = res %>% 
            dplyr::select(starts_with("V")), 
          MARGIN = 1, 
          FUN = "-", 
          STATS = res$true_values
        )*(-1)) -> excess


getSums2 <- function(X, Y = NULL, groupd = FALSE){
  
  if(sum(X %in% "date") == 1){
    if(is.null(Y)){
      Y <- unique(excess$date)
    }
    excess %>% 
      dplyr::filter(date %in% Y) -> excess.fun
    
    if(groupd == TRUE){
      X <- X[!(X %in% "date")]
    }
    
  }else{
    excess.fun <- excess
  }
  
  excess.fun %>% 
    dplyr::group_by_at(X) %>% 
    summarise(across(c(paste0("V", 1:200)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:200, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.5, 0.025, 0.975)))
  
  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:200, "_1"))), df_plot)
  
  return(df_plot)
}

# getSums2(X = c("date", "NUTSII", "age", "sex"), Y = "2025-04-28") %>% View()
# getSums2(c("date", "NUTSII", "sex"), Y = "2025-04-28")
# getSums2(c("date"), Y = c("2025-04-28", "2025-04-29", "2025-04-30"))
# getSums2(X = c("date", "NUTSII"), Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% View()
# getSums2(X = c("date","age", "NUTSII"), Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% View()


# 2025-04-28
getTable <- function(Z, groupd = FALSE){
  rbind(
    getSums2(c("date", "age", "sex"), Y = c(Z), groupd = groupd) %>% 
      mutate(
        CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)), 
    
    getSums2(c("date", "age"), Y = c(Z), groupd = groupd) %>%
      mutate(sex = "Total", 
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)),
    
    getSums2(c("date", "sex"), Y = c(Z), groupd = groupd) %>% 
      mutate(sex2=sex, sex=NULL) %>% 
      mutate(age = "Total", 
             sex = sex2,
             sex2=NULL,
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)),
    
    
    getSums2(c("date"), Y = c(Z), groupd = groupd) %>%
      mutate(age = "Total", sex = "Total", 
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)) %>% return()
  )
}

purrr::reduce(lapply(c("2025-04-28", "2025-04-29", "2025-04-30"), getTable), 
              dplyr::left_join, by = c("age", "sex")) -> tab1

tab1$date.x <- tab1$date.y <- tab1$date <- NULL
colnames(tab1)[-c(1:2)] <- c("2025-04-28", "2025-04-29", "2025-04-30")

print(xtable(tab1), include.rownames=FALSE)


##
## Cumulative table
purrr::reduce(lapply(list(
  c("2025-04-28"),
  c("2025-04-28", "2025-04-29", "2025-04-30"),
  c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
), getTable, groupd = TRUE), dplyr::left_join, by = c("age", "sex")) -> tab1

tab1$age[tab1$age %in% "0-64"] <- "65<"
tab1$age[tab1$age %in% "85+"] <- ">84"

tab1$sex[tab1$sex %in% "female"] <- "Females"
tab1$sex[tab1$sex %in% "male"] <- "Males"
tab1$sex <- factor(tab1$sex, levels = c("Males", "Females", "Total"))
tab1$age <- factor(tab1$age, levels = c("65<", "65-84", ">84", "Total"))

tab1 <- tab1 %>% arrange(age, sex)
colnames(tab1)[-c(1:2)] <- c("Blackout", "2 days after", "1 week after")
print(xtable(tab1), include.rownames=FALSE)




##
## and something about space, like relative excess. 
getRelativeExcess <- function(X, Y = NULL, groupd = FALSE){
    
    if(sum(X %in% "date") == 1){
      if(is.null(Y)){
        Y <- unique(excess$date)
      }
      excess %>% 
        dplyr::filter(date %in% Y) -> excess.fun
      
      if(groupd == TRUE){
        X <- X[!(X %in% "date")]
      }
      
    }else{
      excess.fun <- excess
    }
  

    
    excess.fun %>% 
      dplyr::group_by_at(X) %>% 
      summarise(across(c(paste0("V", 1:200)), list(sum))) -> res_tot 
    
    res %>% 
      dplyr::select(date, NUTSII, age, sex, true_values, starts_with("V")) %>% 
      dplyr::group_by_at(X) %>% 
      summarise(across(c(paste0("V", 1:200)), list(sum))) -> res_exp
    
    (res_tot %>% ungroup() %>% dplyr::select(starts_with("V")))/
      (res_exp %>% ungroup() %>% dplyr::select(starts_with("V"))) -> relativeex
    
    # cbind(excess.fun %>% dplyr::select(all_of(X)), relativeex)
    
    test <- relativeex[,paste0("V", 1:200, "_1")] %>% 
      apply(., 1, function(x) quantile(x, 
                                       probs = c(0.5, 0.025, 0.975)))
    
    df_plot <- test %>% t() %>% as.data.frame()
    df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:200, "_1"))), df_plot)
    
    return(df_plot)
}


(getRelativeExcess(c("date", "NUTSII"), 
                   groupd = TRUE, 
                   Y = c("2025-04-28")) %>% 
    ggplot() + 
    geom_hline(yintercept=0, col = "red", lty = 2) + 
    geom_point(aes(x=NUTSII, y = `50%`)) + 
    geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) +
  theme_bw() + ylab("") + ggtitle("A. Blackout day") + 
  xlab("") -> p1


(getRelativeExcess(c("date", "NUTSII"), 
                     groupd = TRUE, 
                     Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% 
     ggplot() + 
     geom_hline(yintercept=0, col = "red", lty = 2) + 
     geom_point(aes(x=NUTSII, y = `50%`)) + 
     geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) + 
  theme_bw() + ylab("") +  xlab("") + ggtitle("B. Cummulative effect 2 days after") -> p2

(getRelativeExcess(c("NUTSII")) %>% 
     ggplot() + 
     geom_hline(yintercept=0, col = "red", lty = 2) + 
     geom_point(aes(x=NUTSII, y = `50%`)) + 
     geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) + 
  theme_bw() + ylab("") + ggtitle("C. Cummulative effect 1 week after") -> p3

p1/p2/p3

ggsave(filename = "output/fig2.png", dpi = 300, width = 8)
