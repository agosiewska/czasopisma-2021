library(readxl)
library(dplyr)
library(ggplot2)


wykaz_stary <- read_excel("wykaz_stary.xlsx")
colnames(wykaz_stary)[1:8] <- wykaz_stary[3,1:8]
colnames(wykaz_stary)[9:ncol(wykaz_stary)] <- wykaz_stary[2,9:ncol(wykaz_stary)]
wykaz_stary <- wykaz_stary[4:nrow(wykaz_stary),]
wykaz_stary <- wykaz_stary[,-c(1,6,7)]


wykaz_nowy <- read_excel("wykaz_nowy.xlsx")
colnames(wykaz_nowy)[1:9] <- wykaz_nowy[1,1:9]
wykaz_nowy <- wykaz_nowy[2:nrow(wykaz_nowy),]
wykaz_nowy <- wykaz_nowy[,-c(1,2,7,8)]


dyscypliny <- colnames(wykaz_stary)[6:ncol(wykaz_stary)]

dyscypliny_all <- lapply(dyscypliny, function(dyscyplina){
  # dyscyplina <- "informatyka techniczna i telekomunikacja"
  # dyscyplina <- "nauki biologiczne"
  # dyscyplina <- "nauki teologiczne"
  # 
  dyscyplina_stary <- wykaz_stary %>%
    filter(!!as.symbol(dyscyplina) == "x") %>%
    select("Tytuł 1", "Punkty") %>%
    mutate(wykaz = "stary", 
           Punkty = as.numeric(Punkty),
           dyscyplina = dyscyplina)
  
  dyscyplina_nowy <- wykaz_nowy %>%
    filter(!!as.symbol(dyscyplina) == "x") %>%
    select("Tytuł 1", "Punkty") %>%
    mutate(wykaz = "nowy", 
           Punkty = as.numeric(Punkty),
           dyscyplina = dyscyplina)
  
  
  dyscyplina_all <- rbind(dyscyplina_stary, dyscyplina_nowy)
  dyscyplina_all[["wykaz"]] <- factor(dyscyplina_all[["wykaz"]], levels = c("stary", "nowy"))
  
  dyscyplina_all
}) %>%
  bind_rows()





