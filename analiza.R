library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


wykaz_stary <- read.csv2("wykaz_stary.csv")
colnames(wykaz_stary)[1:8] <- wykaz_stary[3,1:8]
colnames(wykaz_stary)[9:ncol(wykaz_stary)] <- wykaz_stary[2,9:ncol(wykaz_stary)]
wykaz_stary <- wykaz_stary[4:nrow(wykaz_stary),]
wykaz_stary <- wykaz_stary[,-c(1,6,7)]


wykaz_nowy <- read.csv2("wykaz_nowy.csv")
colnames(wykaz_nowy)[1:9] <- wykaz_nowy[1,1:9]
wykaz_nowy <- wykaz_nowy[2:nrow(wykaz_nowy),]
wykaz_nowy <- wykaz_nowy[,-c(1,2,7,8)]


colnames(wykaz_nowy)[6:ncol(wykaz_nowy)] <- colnames(wykaz_stary)[6:ncol(wykaz_stary)]

dyscypliny <- colnames(wykaz_stary)[6:ncol(wykaz_stary)]

dyscypliny_all <- lapply(dyscypliny, function(dyscyplina){
  # dyscyplina <- "informatyka techniczna i telekomunikacja"
  # dyscyplina <- "nauki biologiczne"
  # dyscyplina <- "nauki teologiczne"
  # 
  dyscyplina_stary <- wykaz_stary %>%
    mutate(title = ifelse(`Tytuł 1` != "", `Tytuł 1`, `Tytuł 2`)) %>% 
    filter(!!as.symbol(dyscyplina) == "x") %>%
    select("title", "Punkty", "issn") %>%
    mutate(wykaz = "stary", 
           Punkty = as.numeric(Punkty),
           dyscyplina = dyscyplina)
  
  dyscyplina_nowy <- wykaz_nowy %>%
    mutate(title = ifelse(`Tytuł 1` != "", `Tytuł 1`, `Tytuł 2`)) %>% 
    filter(!!as.symbol(dyscyplina) == "x") %>%
    select("title", "Punkty", "issn") %>%
    mutate(wykaz = "nowy", 
           Punkty = as.numeric(Punkty),
           dyscyplina = dyscyplina)
  
  rbind(dyscyplina_stary, dyscyplina_nowy) %>% 
    mutate(wykaz = factor(wykaz, levels = c("stary", "nowy")))  
}) %>% 
  bind_rows()

dyscypliny_all[dyscypliny_all[["Tytuł 1"]] == "Journal of Veterinary Research", ]

tmp <- pivot_wider(dyscypliny_all, names_from = wykaz, values_from = Punkty) 

tmp[lengths(tmp[["stary"]]) == 0, ] %>% View

wykaz_stary[wykaz_stary[["Tytuł 1"]] == "Studia Slavica", ]

  # mutate(stary = as.numeric(stary), nowy = as.numeric(nowy)) %>% 
  # group_by(dyscyplina, stary, nowy) %>% 
  # summarise(n = length(nowy))






