library(rnaturalearth)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)
library(sf)

# read in data
gb_lj <- fread("data/nObs_per_cell_50km.csv")

# get bacgkgound maps
## read in global extent
world <- ne_countries(scale = 10, returnclass = "sf")
pal_sf <- shapefile("regions/palaearctic_noLakes.shp") %>% 
  st_as_sf()

ggplot() + 
  geom_sf(pal_sf, mapping = aes(), fill = "grey60", color = NA) +
  geom_tile(gb_lj, mapping = aes(x = x, y = y, 
                                 color = nObs,fill = nObs)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "inferno", trans = "log",
                       breaks = c(1,20,200,2000), 
                       labels = c(1,20,200,"2k")) +
  scale_color_viridis_c(option = "inferno", trans = "log",
                        breaks = c(1,20,200,2000), 
                        labels = c(1,20,200,"2k")) +
  labs(fill = "Occurrences",
       color = "Occurrences") +
  theme_void() +
  coord_sf(xlim = c(min(gb_lj$x, na.rm = T), max(gb_lj$x, na.rm = T) + 5),
           ylim = c(15 - 5, 80 + 5)) +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.9, 0.05)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave(filename = "figures/dataDensity_50km.png", dpi = 700,
       width = 7,height = 4.5)


#50 km stuff

# get benchmark extent
r <- terra::rast("bioclim/bio1.tif")
r <- aggregate(r, 50)

# make it global
rdf <- raster::as.data.frame(r, xy = T)%>% 
  mutate(z = 1:nrow(.))
rxyz <- rdf %>% 
  dplyr::select(x,y,z) %>% 
  rasterFromXYZ(.)

spdf <- pal
coordinates(spdf) <- ~ decimalLongitude + decimalLatitude

e <- extract(rxyz, spdf)

csv_df <- mutate(as.data.frame(pal), z = e)

gb <- csv_df %>% 
  group_by(z) %>% 
  summarise(nObs = n())

gb_lj <- left_join(gb, rdf, by = "z") %>% 
  filter(!is.na(z))

china <- ne_countries(scale = 10, returnclass = "sf", country = "China")

gb_lj_sf <- st_as_sf(gb_lj, coords = c("x", "y"), crs = st_crs(china))

i <- st_filter(gb_lj_sf, china)

i_df <- as.data.frame(i) %>% 
  select(-geometry)

gb_lj2 <- anti_join(gb_lj, i_df, by = "z")

# make it 50km

ggplot() + 
  geom_sf(pal_sf, mapping = aes(), fill = "grey60", color = NA) +
         geom_tile(gb_lj2, mapping = aes(x = x, y = y, 
                                        color = nObs,fill = nObs)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "inferno", trans = "log",
                       breaks = c(1,20,400,6000), 
                       labels = c(1,20,400, "6k")) +
  scale_color_viridis_c(option = "inferno", trans = "log",
                       breaks = c(1,20,400,6000), 
                       labels = c(1,20,400,"6k")) +
  labs(fill = "Occurrences",
       color = "Occurrences") +
  theme_void() +
  coord_sf(xlim = c(min(gb_lj$x, na.rm = T) - 5, max(gb_lj$x, na.rm = T) + 5),
           ylim = c(15 - 5, 80 + 5)) +
  theme(
    legend.direction = "horizontal",
    legend.position = c(0.9, 0.05)) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave(filename = "palaearctic_figures4/dataDensity_50km_noChina.png", dpi = 450,
       width = 7,height = 4.5)
