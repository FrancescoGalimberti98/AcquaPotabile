library(geojsonio)
library(broom)
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)
library(png)
library(ggimage)

dati <- read_excel("Acqua potabile.xlsx")
dati = dati[,-c(2,3)]
colnames(dati)= c("Regione", "Consumo")

spdf <- spdf <- geojson_read("https://raw.githubusercontent.com/openpolis/geojson-italy/master/geojson/limits_IT_regions.geojson",  what = "sp")
spdf_fortified <- tidy(spdf, region = "reg_name")

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

spdf_fortified = spdf_fortified %>%
  left_join(. , dati, by=c("id"="Regione"))

library(rgeos)
Centroid <- gCentroid(spdf, byid = TRUE)
centroidi = dati
centroidi$long = Centroid$x
centroidi$lat = Centroid$y

goccia = 'gocciadacqua.png'
goccia_df = data.frame(x = Centroid$x, y = Centroid$y, goccia = goccia)
size = dati$Consumo/7000

q <- ggplot() +
  geom_polygon(data = spdf_fortified,aes(fill = Consumo, x = long, y = lat, group = group)) +
  geom_image(data = goccia_df, aes(image = goccia,x=x,y=y),size = size) +
  theme_void() +
  scale_fill_viridis(begin= 0.01, end = 0.7, option = "G", direction = -1,
                     name="",guide= guide_colourbar(label = F, direction = "horizontal", barwidth=7, ticks=F) ) +
  theme(
    text = element_text(color = "#22211d"),
    legend.position = c(0.2,0.23)
  ) +
  coord_map()
q

p <- ggplot() +
  geom_polygon(data = spdf_fortified,aes(fill = Consumo, x = long, y = lat, group = group)) +
  geom_point(data = centroidi, aes(x=long,y=lat,size = Consumo, colour = '#F4D03F'), shape = 20, stroke = FALSE, show.legend = F) +
  scale_colour_manual(values = rep('#F4D03F',20)) +
  scale_size_continuous(name = "Consumo acqua", trans = 'identity', range = c(1,12)) +
  theme_void() +
  scale_fill_viridis(begin= 0.01, end = 0.7, option = "G", direction = -1,
                     name="",guide= guide_colourbar(label = F, direction = "horizontal", barwidth=7, ticks=F) ) +
  theme(
    text = element_text(color = "#22211d"),
    legend.position = c(0.2,0.23)
  ) +
  coord_map()
p
