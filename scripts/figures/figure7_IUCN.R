library(raster)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(data.table)
library(dplyr)
library(sf)

## read these all in and save as single CSV
rich <- fread("data/Palaearctic_PA_AsCSV3_wgs84.csv")

traits <- fread("data/traits/Palaearctic_Species.csv") 

rich_t <- left_join(rich, traits) %>% 
  filter(!is.na(redListCategory))

count <- rich_t %>% 
  group_by(redlistCategory) %>% 
  summarise(count = length(unique(binomial)))

vulnerable <- rich_t %>% 
  dplyr::filter(redlistCategory %in% c("Critically Endangered",
                                       "Endangered",
                                       "Vulnerable",
                                       "Near Threatened"))

head(vulnerable)

gb_vulnerable <- vulnerable %>% 
  group_by(x,y) %>% 
  summarise(richness = sum(PA, na.rm = T))

# plotting time

# first we need to clip to only palaearcitc region

# clip csvs to pal no china region
pal <- raster::shapefile("regions/palaearctic_noLakes_noChina.shp")

gb_vul_r <- dplyr::select(gb_vulnerable, x,y,richness) %>% 
  rasterFromXYZ(.)
vul_mask <- mask(gb_vul_r, pal)
vul_rdf <- raster::as.data.frame(vul_mask, xy = T)%>% 
  filter(!is.na(richness))

# read in base maps
pal_sf <- shapefile("regions/palaearctic_noLakes.shp") %>% 
  st_as_sf()
world <- ne_countries(scale = 110, returnclass = 'sf')

# ggplot
vp <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(vul_rdf, mapping = aes(x = x , y = y,
                                   fill = richness, color = richness)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "plasma") +
  scale_color_viridis_c(option = "plasma")+
  labs(fill = "Richness",
       color = "Richness") +
  coord_sf(xlim = c(min(vul_rdf$x) - 5, max(vul_rdf$x) + 5),
           ylim = c(min(vul_rdf$y) - 5, max(vul_rdf$y)) + 5) +
  theme_void() +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.85, 0.1)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


ggsave(vp, filename = "figures/Figure7_threatened_noNearThreatened.png", dpi = 450,
       width = 11, height = 7)

