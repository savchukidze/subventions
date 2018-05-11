library(ggplot2)
library(dplyr)
library(geojsonio)
library(stringr)

#data
mp2017 <- read.csv("https://raw.githubusercontent.com/OPORA/rada/master/subventions/subventions2017/MPs2017.csv",
                   stringsAsFactors = F)

#сonstituency иoundaries
okrug <- rgdal::readOGR("elections_districts.geojson")
okrug <- fortify(okrug, region = "id")
okrug <- okrug %>% 
  mutate(smd = as.numeric(okrug$id))

map2017 <- full_join(okrug, mp2017, by ="smd")
map2017[is.na(map2017)] <- 0

#classify
map2017$class = case_when(
  map2017$smd %in% c(0:10,41,42,43,44,51,54,55,56,61,104,105,108,109,110,111,224,225) ~ '8',
  between(map2017$sum, 50, 100) ~ '7',
  between(map2017$sum, 40, 50) ~ '6',
  between(map2017$sum, 30, 40) ~ '5',
  between(map2017$sum, 20, 30) ~ '4',
  between(map2017$sum, 10, 20) ~ '3',
  between(map2017$sum, 0.01, 10) ~ '2',
  between(map2017$sum, 0, 0) ~ '1'
)

#create map
png("sub_map_okrug2017.png", height = 1400, width = 1665)

ggplot(map2017)+
  geom_polygon(aes(long, lat, group = group, fill = map2017$class), 
               color = "black", size = 0.5)+
scale_fill_manual(breaks = c(1, 2, 3, 4, 5, 6, 7,8),
                  labels = paste(c("0 грн ", "0-10 млн грн", "10-20 млн грн", "20-30 млн грн", "30-40 млн грн","40-50 млн грн", "понад 50 млн грн", "округи на ТОТ")),
                  values = c(
                      '8' = "#d9d9d9",
                      '7' = "#005a32",
                      '6' = "#238b45",
                      '5' = '#41ab5d', 
                      '4' = '#74c476', 
                      '3' = '#a1d99b', 
                      '2' = '#c7e9c0',
                      '1' = '#f7fcf5'))+
guides(fill = guide_legend(
    title = "Загальна сума субвенцій",
    title.position = "top",
    title.hjust = 0.5,
    title.theme = element_text(size = 22, face = "plain", angle = 0, hjust = 0.5),
    label.position = "bottom",
    label.hjust = 0.5,
    nrow = 1,
    keywidth = 12,
    keyheight = 1.2))+
  
labs(title = "Розподіл субвенцій по округах | 2017")+
coord_map(projection = "mercator", 
          xlim = range(map2017$long), ylim = range(map2017$lat),
          orientation = c(95, 0, 0))+
theme_minimal(base_family = "Ubuntu Medium")+
  theme(text = element_text(color = "#3A3F4A"),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = unit(c(2, 2, 2, 2), "cm"),
        legend.position = "bottom",
        legend.text = element_text(family = "Ubuntu Light", size = 18, color = 'black', hjust = 0),
        legend.title = element_text(color = 'black', vjust = 2.2, family = "Ubuntu Medium"),
        plot.title = element_text(size = 45, margin = margin(t=40, b = 30), color = "#14272d", family ="Ubuntu Bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 30, margin = margin(b = 40), family = "Ubuntu Medium", hjust = 0.5),
        plot.caption = element_text(size = 14, margin = margin(b = 10, t = 25), color = "#5D646F"),
        plot.background = element_rect(fill = "#efede0", color = "black", linetype = "solid", size = 1))

dev.off()
