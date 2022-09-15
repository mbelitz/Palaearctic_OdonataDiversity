library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(sf)
library(raster)
library(data.table)

# read in data
world <- ne_countries(scale = 110, returnclass = 'sf')

rich_end <- fread("data/palaearctic_rich_cwe.csv")

pal <- shapefile("regions/palaearctic_noLakes_noChina.shp")
pal_sf <- shapefile("regions/palaearctic_noLakes.shp") %>% 
  st_as_sf()

# clip rich_end to pal no china region
rich_end_r <- dplyr::select(rich_end, x,y,richness) %>% 
  rasterFromXYZ()
rich_end_mask <- mask(rich_end_r, pal)
rdf <- raster::as.data.frame(rich_end_mask, xy = T)%>% 
  mutate(z = 1:nrow(.)) %>% 
  filter(!is.na(richness))

## map 
# try with log scale on richness
rich_p <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(rdf, mapping = aes(x = x , y = y, 
                                    fill = richness,
                               color = richness)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "plasma", trans = "log",
                       breaks = c(1, 10, 100)) +
  scale_color_viridis_c(option = "plasma", trans = "log",
                       breaks = c(1, 10, 100)) +
  labs(fill = "Richness",
       color = "Richness") +
  coord_sf(xlim = c(min(rdf$x) - 5, max(rdf$x) + 5),
           ylim = c(min(rdf$y) - 5, max(rdf$y)) + 5) +
  theme_void() +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.85, 0.1)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


ggsave(filename = "figures/palaearctic_richness.png", 
       dpi = 450, width = 7,height = 4.5)


## now endemism
# try with log scale on endemism
end_end_r <- dplyr::select(rich_end, x,y,CWE) %>% 
  rasterFromXYZ()
end_end_mask <- mask(end_end_r, pal)
end_rdf <- raster::as.data.frame(end_end_mask, xy = T)%>% 
  mutate(z = 1:nrow(.)) %>% 
  filter(!is.na(CWE))

end_p <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(end_rdf, mapping = aes(x = x , y = y,
                                    fill = CWE, color = CWE)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "turbo", trans = "log",
                       breaks = c(0.0000002, 0.000015, 0.0009),
                       labels = c("Low", "Mid", "High")) +
  scale_color_viridis_c(option = "turbo", trans = "log",
                       breaks = c(0.0000002, 0.000015, 0.0009),
                        labels = c("Low", "Mid", "High"))+
  labs(fill = "CWE",
       color = "CWE") +
  coord_sf(xlim = c(min(rdf$x) - 5, max(rdf$x) + 5),
           ylim = c(min(rdf$y) - 5, max(rdf$y)) + 5) +
  theme_void() +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.85, 0.1)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


ggsave(filename = "figures/palaearctic_endemism.png", 
       dpi = 450, width = 7,height = 4.5)

## cowplot two plots together
cp <- cowplot::plot_grid(rich_p, end_p, labels = c("A","B"), nrow = 2)
ggsave(filename = "figures/Figure1_richness&endemism.png",
       dpi = 450, width = 7, height = 8)
