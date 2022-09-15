library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(sf)
library(raster)
library(biscale)
library(cowplot)

# read in data
world <- ne_countries(scale = 110, returnclass = 'sf')

pal <- shapefile("regions/palaearctic_noLakes_noChina.shp")
pal_sf <- shapefile("regions/palaearctic_noLakes.shp") %>% 
  st_as_sf()

lentic <- data.table::fread("data/palaearctic_rich_cwe_lentic.csv")
lotic <-  data.table::fread("data/palaearctic_rich_cwe_lotic.csv")

#generate bivariate data
bdf <- left_join(lentic, lotic, by = c("x", "y")) %>% 
  dplyr::rename(len_richness = richness.x,
                lot_richness = richness.y,
                len_CWE = CWE.x,
                lot_CWE = CWE.y) %>% 
  dplyr::filter(!is.na(x), !is.na(y)) %>% 
  dplyr::select(x, y, len_richness, lot_richness, len_CWE, lot_CWE)

# get z value for everypoint
biorast <- terra::rast("bioclim/bio1.tif")
biorast <- terra::aggregate(biorast, 5)
biorast_df <- terra::as.data.frame(biorast, xy = T)%>% 
  mutate(z = 1:nrow(.)) 
biorast_reg <- biorast_df %>% 
  dplyr::select(x,y,z) %>% 
  raster::rasterFromXYZ(.)

# extract z values for bdf points
spdf <- bdf
sp::coordinates(spdf) <- ~ x + y

e <- extract(biorast_reg, spdf)

bdf <- dplyr::mutate(bdf, z = e)

# clip to palaearctic
bdf_r <- dplyr::select(bdf, x, y, z) %>% 
  rasterFromXYZ(.)
bdf_mask <- mask(bdf_r, pal)
bdf_mask_df <- raster::as.data.frame(bdf_mask, xy = T) %>% 
  dplyr::rename(z = 3) %>% 
  dplyr::filter(!is.na(z))


bdf_mask_df <- left_join(bdf, bdf_mask_df, by = c("z")) %>% 
  dplyr::filter(!is.na(x.y),
                !is.na(y.y))

bdf_mask_df <- bdf_mask_df %>% 
  dplyr::mutate(len_CWE = if_else(condition = is.na(len_CWE),
                                true = 0, false = as.double(len_CWE)),
               lot_CWE = if_else(condition = is.na(lot_CWE),
                                true = 0,
                                false = as.double(lot_CWE))) %>% 
  dplyr::mutate(len_richness = if_else(condition = is.na(len_richness),
                                  true = 0, false = as.double(len_richness)),
                lot_richness = if_else(condition = is.na(lot_richness),
                                true = 0,
                                false = as.double(lot_richness))) 

# create classes
data <-bdf_mask_df %>% 
  mutate(bc2 = case_when(len_richness <= 3  ~ 1,
                         len_richness > 3 & len_richness <= 11 ~ 2,
                         len_richness > 11 ~ 3),
         bc1 = case_when(lot_richness <= 0 ~ 1,
                         lot_richness > 0 & lot_richness <= 1 ~ 2,
                         lot_richness > 1 ~ 3))

data <- data %>% 
  mutate(bi_class = c(paste(bc1, bc2, sep = "-")))

unique(data$bi_class)

## map 
rich_bp <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(data, mapping = aes(x = x.y, y = y.y, 
                               fill = bi_class,
                               color = bi_class)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  coord_sf(xlim = c(min(data$x.y,na.rm = T) - 2, max(data$x.y,na.rm = T) + 5),
           ylim = c(min(data$y.y,na.rm = T) - 2, max(data$y.y,na.rm = T) + 2)) +
  labs(x = "", y ="") +
  bi_theme() +
  theme(legend.position = "none") 
  


legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Lotic rich",
                    ylab = "Lentic rich",
                    size = 10)

c <- ggdraw() +
  draw_plot(rich_bp, 0,0,1,1) +
  draw_plot(legend, x = 0.7, y =0.1, 
            width = 0.3, height = 0.3)

ggsave(c, filename = "figures/lenticLotic_richness_biPlot.png", dpi = 450,
       width = 11, height = 7)



# now do the lentic lotic cwe work 
quantile(bdf_mask_df$len_CWE, probs = c(0.33,0.66))
quantile(bdf_mask_df$lot_CWE, probs = c(0.33,0.66))

data2 <-bdf_mask_df %>% 
  mutate(bc2 = case_when(len_CWE <= 7.677006e-06 ~ 1,
                         len_CWE > 7.677006e-06 & len_CWE <= 1.082321e-05 ~ 2,
                         len_CWE > 1.082321e-05 ~ 3),
         bc1 = case_when(lot_CWE <= 0 ~ 0,
                         lot_CWE > 0 & lot_CWE <= 2.519936e-05  ~ 2,
                         lot_CWE > 2.519936e-05  ~ 3))

data2 <- data2 %>% 
  mutate(bi_class = c(paste(bc1, bc2, sep = "-")))

unique(data2$bi_class)

## map 
end_bp <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(data2, mapping = aes(x = x.x, y = y.x, 
                                fill = bi_class,
                                color = bi_class)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  coord_sf(xlim = c(min(data2$x.y,na.rm = T) - 2, max(data2$x.y,na.rm = T) + 5),
           ylim = c(min(data2$y.y,na.rm = T) - 2, max(data2$y.y,na.rm = T) + 2)) +
  labs(x = "", y ="") +
  bi_theme() +
  theme(legend.position = "none") 


legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Lotic CWE",
                    ylab = "Lentic CWE",
                    size = 10)

c2 <- ggdraw() +
  draw_plot(end_bp, 0,0,1,1) +
  draw_plot(legend, x = 0.7, y =0.1, 
            width = 0.3, height = 0.3)

ggsave(c2, filename = "figures/lenticLotic_CWE_biPlot.png", dpi = 450,
       width = 11, height = 7)


# now plot CWE for lentic and lotic
len_end <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(bdf_mask_df, mapping = aes(x = x.x , y = y.x,
                                   fill = len_CWE, color = len_CWE)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
    scale_fill_viridis_c(option = "turbo", trans = "log",
                         limits = c(1.455658e-07, 0.117),
                         breaks = c(0.0000016, 0.000096, 0.0067),
                         labels = c("Low", "Mid", "High")) +
    scale_color_viridis_c(option = "turbo", trans = "log",
                          limits = c(1.455658e-07, 0.117),
                          breaks = c(0.0000016, 0.000096, 0.0067),
                          labels = c("Low", "Mid", "High")) +
  labs(fill = "CWE",
       color = "CWE") +
  coord_sf(xlim = c(min(data$x.x, na.rm = T) - 5, max(data$x.x, na.rm = T) + 5),
           ylim = c(min(data$y.x, na.rm = T) - 5, max(data$y.x, na.rm = T)) + 5) +
   theme_void() +
    theme(
      legend.direction = "horizontal",
      legend.position = c(0.85, 0.1)) +
    guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))

ggsave(len_end, filename = "figures/lentic_endemism.png", dpi = 450,
       width = 11, height = 7)

# lotic endemism
lot_end <- ggplot() +
  geom_sf(pal_sf, mapping = aes(), fill = "grey45", color = NA) +
  geom_tile(bdf_mask_df, mapping = aes(x = x.y , y = y.y,
                                  fill = lot_CWE, color = lot_CWE)) +
  geom_sf(world, mapping = aes(), fill = NA, color = "grey75", size = 0.125) +
  scale_fill_viridis_c(option = "turbo", trans = "log",
                       limits = c(1.455658e-07, 0.117),
                       breaks = c(0.0000016, 0.000096, 0.0067),
                       labels = c("Low", "Mid", "High")) +
  scale_color_viridis_c(option = "turbo", trans = "log",
                        limits = c(1.455658e-07, 0.117),
                        breaks = c(0.0000016, 0.000096, 0.0067),
                        labels = c("Low", "Mid", "High")) +
  labs(fill = "CWE",
       color = "CWE") +
  coord_sf(xlim = c(min(data$x.x, na.rm = T) - 5, max(data$x.x, na.rm = T) + 5),
           ylim = c(min(data$y.x, na.rm = T) - 5, max(data$y.x, na.rm = T)) + 5) +
  theme_void() +
   theme(
     legend.direction = "horizontal",
     legend.position = c(0.85, 0.1)) +
   guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5))


ggsave(lot_end, filename = "figures/lotic_endemism.png", dpi = 450,
       width = 11, height = 7)

## ggarrange two plots together
ga <- ggpubr::ggarrange(c, c2,
                        len_end, lot_end,
                        ncol = 2, nrow = 2, 
                        labels = c("A", "B", "Lentic", "Lotic")
)

ggsave(plot = ga, filename = "figures/Figure2.png",
       dpi = 450, width = 14, height = 8)