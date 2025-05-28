

# Created 21.05.2025

# Extract the main results

#-------------------------------------------------------------------------------

library(tidyverse)
library(INLA)
library(patchwork)
library(xtable)
library(sf)

# set path
path <- "C:/Users/gkonstan/OneDrive - Imperial College London/ICRF Imperial/Projects/blackout-burden/"
setwd(path)

# cntr <- "PRT"
# cntr <- "ESP"
dlnm_nam <- "_dlnm"

res_form_prt <- readRDS(file = paste0("output/RES_MAIN_", "PRT", dlnm_nam, ".rds"))
res_form_esp <- readRDS(file = paste0("output/RES_MAIN_", "ESP", dlnm_nam, ".rds"))

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



ggsave(filename = "output/fig1.png", dpi = 300, width = 8, height = 9)
dev.off()
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

res_prt %>% 
  dplyr::select(date, NUTSII, age, sex, true_values) %>% 
  cbind(., 
        sweep(
          x = res_prt %>% 
            dplyr::select(starts_with("V")), 
          MARGIN = 1, 
          FUN = "-", 
          STATS = res_prt$true_values
        )*(-1)) -> excess_prt

res_esp %>% 
  dplyr::select(date, NUTSII, age, sex, true_values) %>% 
  cbind(., 
        sweep(
          x = res_esp %>% 
            dplyr::select(starts_with("V")), 
          MARGIN = 1, 
          FUN = "-", 
          STATS = res_esp$true_values
        )*(-1)) -> excess_esp

getSums2 <- function(X, Y = NULL, groupd = FALSE, excess){
  
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
    summarise(across(c(paste0("V", 1:1000)), list(sum))) -> res_tot 
  
  test <- res_tot[,paste0("V", 1:1000, "_1")] %>% 
    apply(., 1, function(x) quantile(x, 
                                     probs = c(0.5, 0.025, 0.975)))
  
  df_plot <- test %>% t() %>% as.data.frame()
  df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:1000, "_1"))), df_plot)
  
  return(df_plot)
}

# getSums2(X = c("date", "NUTSII", "age", "sex"), Y = "2025-04-28") %>% View()
# getSums2(c("date", "NUTSII", "sex"), Y = "2025-04-28")
# getSums2(c("date"), Y = c("2025-04-28", "2025-04-29", "2025-04-30"))
# getSums2(X = c("date", "NUTSII"), Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% View()
# getSums2(X = c("date","age", "NUTSII"), Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% View()


# 2025-04-28
getTable <- function(Z, groupd = FALSE, excess){
  rbind(
    getSums2(excess = excess, c("date", "age", "sex"), Y = c(Z), groupd = groupd) %>% 
      mutate(
        CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)), 
    
    getSums2(excess = excess, c("date", "age"), Y = c(Z), groupd = groupd) %>%
      mutate(sex = "Total", 
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)),
    
    getSums2(excess = excess, c("date", "sex"), Y = c(Z), groupd = groupd) %>% 
      mutate(sex2=sex, sex=NULL) %>% 
      mutate(age = "Total", 
             sex = sex2,
             sex2=NULL,
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)),
    
    
    getSums2(excess = excess, c("date"), Y = c(Z), groupd = groupd) %>%
      mutate(age = "Total", sex = "Total", 
             CrI = paste0(round(`50%`), " (", round(`2.5%`), ", ", round(`97.5%`), ")")) %>% 
      dplyr::select(!c(`50%`, `2.5%`, `97.5%`)) %>% return()
  )
}

# portugal
purrr::reduce(lapply(c("2025-04-28", "2025-04-29", "2025-04-30"), getTable, excess = excess_prt), 
              dplyr::left_join, by = c("age", "sex")) -> tab1_prt

tab1_prt$date.x <- tab1_prt$date.y <- tab1_prt$date <- NULL
colnames(tab1_prt)[-c(1:2)] <- c("2025-04-28", "2025-04-29", "2025-04-30")

print(xtable(tab1_prt), include.rownames=FALSE)

# spain

purrr::reduce(lapply(c("2025-04-28", "2025-04-29", "2025-04-30"), getTable, 
                     excess = excess_esp), 
              dplyr::left_join, by = c("age", "sex")) -> tab1_esp

tab1_esp$date.x <- tab1_esp$date.y <- tab1_esp$date <- NULL
colnames(tab1_esp)[-c(1:2)] <- c("2025-04-28", "2025-04-29", "2025-04-30")

print(xtable(tab1_esp), include.rownames=FALSE)


##
## Cumulative table
purrr::reduce(lapply(list(
  c("2025-04-28"),
  c("2025-04-28", "2025-04-29", "2025-04-30"),
  c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
), getTable, groupd = TRUE, excess = excess_prt), dplyr::left_join, by = c("age", "sex")) -> 
  tab1_prt

purrr::reduce(lapply(list(
  c("2025-04-28"),
  c("2025-04-28", "2025-04-29", "2025-04-30"),
  c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
), getTable, groupd = TRUE, excess = excess_esp), dplyr::left_join, by = c("age", "sex")) -> 
  tab1_esp


list_tab <- list(tab1_prt, tab1_esp)
for(i in 1:2){
  tab1 <- list_tab[[i]]
  tab1$age[tab1$age %in% "0-64"] <- "65<"
  tab1$age[tab1$age %in% "85+"] <- ">84"
  
  tab1$sex[tab1$sex %in% "female"] <- "Females"
  tab1$sex[tab1$sex %in% "male"] <- "Males"
  tab1$sex <- factor(tab1$sex, levels = c("Males", "Females", "Total"))
  tab1$age <- factor(tab1$age, levels = c("65<", "65-84", ">84", "Total"))
  
  tab1 <- tab1 %>% arrange(age, sex)
  colnames(tab1)[-c(1:2)] <- c("Blackout", "2 days after", "1 week after")
  list_tab[[i]] <- tab1
}


left_join(list_tab[[1]],
          list_tab[[2]], 
          by = c("age" = "age", "sex" = "sex")) -> tab1


print(xtable(tab1), include.rownames=FALSE)




##
## and something about space, like relative excess. 
getRelativeExcess <- function(X, Y = NULL, groupd = FALSE, excess, res){
    
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
      summarise(across(c(paste0("V", 1:1000)), list(sum))) -> res_tot 
    
    res %>% 
      dplyr::select(date, NUTSII, age, sex, true_values, starts_with("V")) %>% 
      dplyr::group_by_at(X) %>% 
      summarise(across(c(paste0("V", 1:1000)), list(sum))) -> res_exp
    
    (res_tot %>% ungroup() %>% dplyr::select(starts_with("V")))/
      (res_exp %>% ungroup() %>% dplyr::select(starts_with("V"))) -> relativeex
    
    # cbind(excess.fun %>% dplyr::select(all_of(X)), relativeex)
    
    test <- relativeex[,paste0("V", 1:1000, "_1")] %>% 
      apply(., 1, function(x) quantile(x, 
                                       probs = c(0.5, 0.025, 0.975)))
    
    post_prob <- relativeex[,paste0("V", 1:1000, "_1")] > 0
    post_prob <- post_prob %>% apply(., 1, mean)
    df_plot <- test %>% t() %>% as.data.frame()
    df_plot <- cbind(res_tot %>% dplyr::select(-(paste0("V", 1:1000, "_1"))), df_plot, post_prob)
    
    return(df_plot)
}



lag0_map <- getRelativeExcess(excess = excess_esp, 
                              res = res_esp, 
                              c("date", "NUTSII"), 
                              groupd = TRUE, 
                              Y = c("2025-04-28"))

lag0_map$lag <- "Lag0"

lag0_2_map <- getRelativeExcess(excess = excess_esp, 
                              res = res_esp, 
                              c("date", "NUTSII"), 
                              groupd = TRUE, 
                              Y = c("2025-04-28", "2025-04-29", "2025-04-30"))

lag0_2_map$lag <- "Lag0-2"


lag0_6_map <- getRelativeExcess(excess = excess_esp, 
                                res = res_esp, 
                                c("NUTSII"))

lag0_6_map$lag <- "Lag0-6"

map_spain <- rbind(lag0_map, lag0_2_map, lag0_6_map)
map_spain$country <- "Spain"


##
## same for Portugal

lag0_map <- getRelativeExcess(excess = excess_prt, 
                              res = res_prt, 
                              c("date", "NUTSII"), 
                              groupd = TRUE, 
                              Y = c("2025-04-28"))

lag0_map$lag <- "Lag0"

lag0_2_map <- getRelativeExcess(excess = excess_prt, 
                                res = res_prt, 
                                c("date", "NUTSII"), 
                                groupd = TRUE, 
                                Y = c("2025-04-28", "2025-04-29", "2025-04-30"))

lag0_2_map$lag <- "Lag0-2"


lag0_6_map <- getRelativeExcess(excess = excess_prt, 
                                res = res_prt, 
                                c("NUTSII"))

lag0_6_map$lag <- "Lag0-6"

map_port <- rbind(lag0_map, lag0_2_map, lag0_6_map)
map_port$country <- "Portugal"

map_plot <- rbind(map_spain, map_port)

shp_prt <- read_sf("output/shp_PRT.dbf")
shp_esp <- read_sf("output/shp_ESP.dbf")
shp_esp %>% 
  select(NAME_LATN) %>% 
  filter(!NAME_LATN %in% "Canarias") -> shp_esp

shp <- rbind(shp_esp, 
             shp_prt %>% 
               select(NAME_LATN = n2_L_20)) 
plot(shp$geometry)

shp$NAME_LATN[shp$NAME_LATN %in% "Centro (PT)"] <- "Centro"
# nice and bring to the other file
map_plot <- left_join(shp, map_plot, by = c("NAME_LATN" = "NUTSII"))

map_plot <- map_plot[complete.cases(map_plot$country),]
# lim <- map_plot$`50%` %>% max() %>% round(digits = 2)
map_plot %>% 
  mutate(
    med_cat = 
      case_when(
        `50%` < -0.05 ~ "-5%<",
        `50%` >= -0.05 & `50%` < -0.01 ~ "[-5%, 1%)",
        `50%` >= -0.01 & `50%` < 0.01 ~ "[-1%, 1%)",
        `50%` >= 0.01 & `50%` < 0.05 ~ "[1%, 5%)",
        `50%` >= 0.05 ~ ">=5%"
      ) %>% factor(levels = c("-5%<", "[-5%, 1%)", "[-1%, 1%)", "[1%, 5%)", ">=5%"))
  ) -> map_plot

ggplot(data = map_plot) + 
  geom_sf(aes(fill = med_cat), col = NA) +
  facet_grid(cols = vars(lag)) + theme_bw() + 
  geom_sf(data = map_plot %>% 
            group_by(country) %>% 
            summarize(geometry = st_union(geometry)), 
          col = "black", fill = NA, lwd = 0.7) + 
  # scale_fill_viridis_c(option = "E", limits = c(-lim, lim), alpha = 0.9) +
  scale_fill_viridis_d(option = "E", alpha = 0.9) +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.title = element_blank()) + 
  ggtitle("A. Relative excess mortality") -> p1



map_plot %>% 
  mutate(
    post_cat = case_when(
      post_prob < 0.2 ~ "[0.0, 0.2)",
      post_prob >= 0.2 & post_prob < 0.8 ~ "[0.2, 0.8)",
      post_prob > 0.8 ~ "[0.8, 1.0]"
    )
  ) -> map_plot

ggplot(data = map_plot) + 
  geom_sf(aes(fill = post_cat), col = NA) +
  facet_grid(cols = vars(lag)) + theme_bw() + 
  geom_sf(data = map_plot %>% 
            group_by(country) %>% 
            summarize(geometry = st_union(geometry)), 
          col = "black", fill = NA, lwd = 0.7) + 
  scale_fill_viridis_d(option = "E") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        legend.title = element_blank()) + 
  ggtitle("B. Posterior probability of positive excess") -> p2

p1/p2
ggsave(filename = "output/fig3.png", dpi = 300, width = 8.5, height = 5)






##
## Get a relative excess table for the supplement


excess_list <- list(excess_prt, excess_esp)
res_list <- list(res_prt, res_esp)
tab_list <- list()

for(i in 1:2){
  purrr::reduce(lapply(list(
    c("2025-04-28"),
    c("2025-04-28", "2025-04-29", "2025-04-30"),
    c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
  ), function(Y) getRelativeExcess(Y=Y, groupd = TRUE, excess = excess_list[[i]], res = res_list[[i]], X = c("date", "age", "sex"))), 
  dplyr::left_join, by = c("age", "sex")) -> 
    tab11
  
  purrr::reduce(lapply(list(
    c("2025-04-28"),
    c("2025-04-28", "2025-04-29", "2025-04-30"),
    c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
  ), function(Y) getRelativeExcess(Y=Y, groupd = TRUE, excess = excess_list[[i]], res = res_list[[i]], X = c("date", "age"))), 
  dplyr::left_join, by = c("age")) %>% mutate(sex="Total") -> 
    tab12
  
  purrr::reduce(lapply(list(
    c("2025-04-28"),
    c("2025-04-28", "2025-04-29", "2025-04-30"),
    c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
  ), function(Y) getRelativeExcess(Y=Y, groupd = TRUE, excess = excess_list[[i]], res = res_list[[i]], X = c("date", "sex"))), 
  dplyr::left_join, by = c("sex")) %>% mutate(age="Total") -> 
    tab13
  
  purrr::reduce(lapply(list(
    c("2025-04-28"),
    c("2025-04-28", "2025-04-29", "2025-04-30"),
    c("2025-04-28", "2025-04-29", "2025-04-30", "2025-05-01", "2025-05-02", "2025-05-03", "2025-05-04")
  ), function(Y) getRelativeExcess(Y=Y, groupd = TRUE, excess = excess_list[[i]], res = res_list[[i]], X = c("date"))), 
  cbind) -> 
    tab14
  
  names(tab14) <- colnames(tab11)[-c(1:2)]
  tab14 <- tab14 %>% as_tibble() %>% mutate(age="Total", sex = "Total")
  # fix colnames
  
  tab11 %>% 
    dplyr::rename(
      post_prob.x = `...6.x`, 
      post_prob.y = `...6.y`, 
      post_prob = `...6`
    ) -> tab11
  
  tab14 %>% 
    dplyr::rename(
      post_prob.x = `...6.x`, 
      post_prob.y = `...6.y`, 
      post_prob = `...6`
    ) -> tab14
  
  rbind(
    tab11, 
    tab12[,colnames(tab11)], 
    tab13[,colnames(tab11)], 
    tab14[,colnames(tab11)]
  ) -> tab1

  
  tab1$age[tab1$age %in% "0-64"] <- "65<"
  tab1$age[tab1$age %in% "85+"] <- ">84"
  
  tab1$sex[tab1$sex %in% "female"] <- "Females"
  tab1$sex[tab1$sex %in% "male"] <- "Males"
  tab1$sex <- factor(tab1$sex, levels = c("Males", "Females", "Total"))
  tab1$age <- factor(tab1$age, levels = c("65<", "65-84", ">84", "Total"))
  
  tab1 <- tab1 %>% arrange(age, sex)
  
  tab_list[[i]] <- tab1
}


rbind(
  tab_list[[1]],
  tab_list[[2]]
) -> tab1

tab1[,3:ncol(tab1)] <- tab1[,3:ncol(tab1)]*100

# and get the 95%CrI
paste0(round(tab1$`50%.x`, digits = 2) %>% sprintf("%.2f", .), 
       " (", round(tab1$`2.5%.x`, digits = 2) %>% sprintf("%.2f", .), 
       ", ", round(tab1$`97.5%.x`, digits = 2) %>% sprintf("%.2f", .), ")")

data.frame(
  Age = tab1$age, 
  Sex = tab1$sex, 
  Lag0 = paste0(round(tab1$`50%.x`, digits = 2) %>% sprintf("%.2f", .), "%",
                " (", round(tab1$`2.5%.x`, digits = 2) %>% sprintf("%.2f", .), "%",
                ", ", round(tab1$`97.5%.x`, digits = 2) %>% sprintf("%.2f", .), "%", ")"), 
  Probability = (tab1$post_prob.x/100) %>% round(digits = 2) %>% format(., nsmall = 2),
  Lag0 = paste0(round(tab1$`50%.y`, digits = 2) %>% sprintf("%.2f", .), "%",
                " (", round(tab1$`2.5%.y`, digits = 2) %>% sprintf("%.2f", .), "%",
                ", ", round(tab1$`97.5%.y`, digits = 2) %>% sprintf("%.2f", .), "%",")"), 
  Probability = (tab1$post_prob.y/100) %>% round(digits = 2) %>% format(., nsmall = 2),
  Lag0 = paste0(round(tab1$`50%`, digits = 2) %>% sprintf("%.2f", .), "%",
                " (", round(tab1$`2.5%`, digits = 2) %>% sprintf("%.2f", .), "%",
                ", ", round(tab1$`97.5%`, digits = 2) %>% sprintf("%.2f", .), "%", ")"), 
  Probability = (tab1$post_prob/100) %>% round(digits = 2) %>% format(., nsmall = 2)
) -> tab1
print(xtable(tab1, digits=2), include.rownames=FALSE)


?round

(tab1$post_prob.x / 100) %>%
  round(2) %>%
  format(nsmall = 2) %>%
  paste0("%")
##
## I need some sort of posterior here


(getRelativeExcess(excess = excess_esp, 
                   res = res_esp, 
                   c("date", "NUTSII"), 
                   groupd = TRUE, 
                   Y = c("2025-04-28")) %>% 
    ggplot() + 
    geom_hline(yintercept=0, col = "red", lty = 2) + 
    geom_point(aes(x=NUTSII, y = `50%`)) + 
    geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) +
  theme_bw() + ylab("") + ggtitle("A. Blackout day") + 
  xlab("") -> p1


(getRelativeExcess(
  excess = excess_esp, 
  res = res_esp, 
  c("date", "NUTSII"), 
  groupd = TRUE, 
  Y = c("2025-04-28", "2025-04-29", "2025-04-30")) %>% 
     ggplot() + 
     geom_hline(yintercept=0, col = "red", lty = 2) + 
     geom_point(aes(x=NUTSII, y = `50%`)) + 
     geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) + 
  theme_bw() + ylab("") +  xlab("") + ggtitle("B. Cummulative effect 2 days after") -> p2

(getRelativeExcess(
  excess = excess_esp, 
  res = res_esp, 
  c("NUTSII")) %>% 
     ggplot() + 
     geom_hline(yintercept=0, col = "red", lty = 2) + 
     geom_point(aes(x=NUTSII, y = `50%`)) + 
     geom_errorbar(aes(x=NUTSII, ymin = `2.5%`, ymax = `97.5%`), width = 0.2)) + 
  theme_bw() + ylab("") + ggtitle("C. Cummulative effect 1 week after") -> p3

p1/p2/p3

ggsave(filename = "output/fig2.png", dpi = 300, width = 8)
