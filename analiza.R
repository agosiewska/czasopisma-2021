library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


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
  
  rbind(dyscyplina_stary, dyscyplina_nowy) %>% 
    mutate(wykaz = factor(wykaz, levels = c("stary", "nowy")))  
}) 


lapply(dyscypliny_all, function(i) is.na(i[["Punkty"]]))

dyscypliny_all[dyscypliny_all[["Tytuł 1"]] == "Journal of Veterinary Research", ]

tmp <- pivot_wider(dyscypliny_all, names_from = wykaz, values_from = Punkty,
            values_fill = NA) %>% 
  mutate(stary = unlist(stary))

tmp[lengths(tmp[["stary"]]) == 2, ] %>% View


  # mutate(stary = as.numeric(stary), nowy = as.numeric(nowy)) %>% 
  # group_by(dyscyplina, stary, nowy) %>% 
  # summarise(n = length(nowy))






