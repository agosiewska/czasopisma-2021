library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)


wykaz_stary <- read.csv2("wykaz_stary.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(wykaz_stary)[1:8] <- wykaz_stary[3,1:8]
colnames(wykaz_stary)[9:ncol(wykaz_stary)] <- wykaz_stary[2,9:ncol(wykaz_stary)]
wykaz_stary <- wykaz_stary[4:nrow(wykaz_stary),]

clean_colnames <- function(x) {
  colnames(x)[colnames(x) == "issn"] <- 
    paste0("issn", 1L:sum(colnames(x) == "issn"))
  x[, colnames(x) != "e-issn"] %>% 
    mutate(title = ifelse(`Tytuł 1` != "", `Tytuł 1`, `Tytuł 2`),
           issn = ifelse(issn1 != "", issn1, issn2)) %>% 
    select(title, issn, points = Punkty) %>% 
    cbind(x[, which(colnames(x) == "archeologia"):ncol(x)])
}

wykaz_stary <- clean_colnames(wykaz_stary)


wykaz_nowy <- read.csv2("wykaz_nowy.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(wykaz_nowy)[1:9] <- wykaz_nowy[1,1:9]
wykaz_nowy <- wykaz_nowy[2:nrow(wykaz_nowy),]
wykaz_nowy <- clean_colnames(wykaz_nowy)

dyscypliny <- colnames(wykaz_stary)[4:ncol(wykaz_stary)]
colnames(wykaz_nowy)[4:ncol(wykaz_nowy)] <- dyscypliny

wykazy <- list(stary = wykaz_stary, nowy = wykaz_nowy)

dyscypliny_all <- lapply(dyscypliny, function(dyscyplina){
  
  lapply(c("stary", "nowy"), function(i) {
    filter(wykazy[[i]], !!as.symbol(dyscyplina) == "x") %>%
      select("title", "issn", "points") %>%
      mutate(wykaz = i, 
             points = points,
             dyscyplina = dyscyplina)
  }) %>% 
    bind_rows() %>% 
    mutate(wykaz = factor(wykaz, levels = c("stary", "nowy")))  
}) %>% 
  bind_rows() 

dyscypliny_all[dyscypliny_all[["title"]] == "Journal of Veterinary Research", ]


plot_dat <- pivot_wider(dyscypliny_all, names_from = wykaz, values_from = points,
                        values_fill = list(points = NA)) %>% 
  group_by(dyscyplina, stary, nowy) %>%
  summarise(n = length(nowy)) %>% 
  ungroup() %>% 
  mutate(stary = ifelse(is.na(stary), "brak", stary),
         stary = factor(stary, levels = c("brak", "20", "40", "70",
                                          "100", "140", "200")),
         nowy = ifelse(is.na(nowy), "brak", nowy),
         nowy = factor(nowy, levels = c("brak", "20", "40", "70",
                                        "100", "140", "200"))) 


cairo_pdf("plot1.pdf", height = 32, width = 32)
ggplot(plot_dat, aes(x = stary, y = nowy, fill = n, label = n)) +
  geom_tile() +
  geom_label(fill = "white") +
  facet_wrap(~ dyscyplina)
dev.off()

change_plot_dat <- mutate(plot_dat, 
                          change = paste0(stary, "->", nowy),
                          change_type = ifelse(as.numeric(stary) < as.numeric(nowy), "wzrost",
                                               ifelse(as.numeric(stary) > as.numeric(nowy), "spadek",
                                                      "tak samo")),
                          change_type = ifelse(stary == "brak", "nowe czasopismo", change_type),
                          change_type = ifelse(nowy == "brak", "usunięte czasopismo", change_type))


cairo_pdf("plot2.pdf", height = 32, width = 32)
group_by(change_plot_dat, dyscyplina, change, change_type) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(dyscyplina) %>% 
  mutate(frac = n/sum(n)) %>% 
  ggplot(aes(x = change, y = frac, fill = change_type)) +
  geom_col() +
  facet_wrap(~dyscyplina) +
  coord_flip()
dev.off()


png("plot3.png", height = 480*4, width = 480*4, res = 200)
change_plot_dat %>% 
  group_by(dyscyplina, change_type) %>% 
  summarise(n = sum(n)) %>% 
  mutate(frac = n/sum(n)) %>% 
  filter(change_type == "wzrost") %>% 
  arrange(frac) %>% 
  mutate(dyscyplina = factor(dyscyplina, levels = .[["dyscyplina"]]),
         teol = dyscyplina %in% c("prawo kanoniczne", "nauki teologiczne")) %>% 
  ggplot(aes(x = dyscyplina, y = frac, fill = teol)) +
  geom_col() +
  scale_fill_manual("Związane z teologią", values = rev(c("black", "grey"))) +
  scale_x_discrete("Dyscyplina") +
  scale_y_continuous("Frakcja czasopism ze wzrostem punktów", expand = c(0, 0)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
dev.off()





plot_dat <- plot_dat[plot_dat[["stary"]] != "brak", ]
plot_dat <- plot_dat[plot_dat[["nowy"]] != "brak", ]
plot_dat[["roznica"]] <- as.numeric(as.character(plot_dat[["nowy"]])) - as.numeric(as.character(plot_dat[["stary"]]))
mean(plot_dat[["roznica"]])

tmp <- plot_dat %>%
  select(dyscyplina, roznica, n) %>%
  group_by(dyscyplina) %>%
  summarise(mean = weighted.mean(roznica, n)) %>%
  arrange(mean) %>%
  mutate(dyscyplina = factor(dyscyplina, levels = .[["dyscyplina"]]))
ggplot(tmp, aes(x = dyscyplina, y = mean)) +
  geom_col() + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Zmiana średniej punktacji czasopism, znajdujących się w nowym i poprzednim wykazie")
