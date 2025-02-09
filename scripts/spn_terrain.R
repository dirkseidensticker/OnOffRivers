# terrain map

library(ggplot2)
library(ggrepel)
library(viridis)
library(tidyr)
library(sf)

library(elevatr)
library(raster)

# setting up boundig box

xmin = 20.5 # 21
xmax = 20.7 # 21.5
ymin = -1.65 # -.5
ymax = -1.8 # -.1

xmin = 20.52 # 21
xmax = 20.6 # 21.5
ymin = -1.67 # -.5
ymax = -1.73 # -.1


locations <- data.frame(x = c(xmin, xmax), 
                        y = c(ymin, ymax))  

osm.rivers.lines <- geojsonsf::geojson_sf("input/gis/OSM_river_lines.geojson")
osm.rivers.poly <- geojsonsf::geojson_sf("input/gis/OSM_river_lakes_poly.geojson") %>%
  sf::st_make_valid()


# get gem
dem <- elevatr::get_elev_raster(locations = locations, 
                                prj = "EPSG:4326", 
                                z = 14, 
                                clip = "bbox")

plot(dem)

dem_df <- raster::as.data.frame(dem, xy=TRUE)
names(dem_df) <- c("x", "y", "z")

dem_df <- dplyr::filter(dem_df, z >= 0)

hist(dem_df$z)

# create slope and hillshade
slope = raster::terrain(dem, opt='slope')
aspect = raster::terrain(dem, opt='aspect')
hill = raster::hillShade(slope, aspect, 40, 270)

plot(hill)

hill_df <- as(hill, "SpatialPixelsDataFrame")
hill_df <- as.data.frame(hill_df)
colnames(hill_df) <- c("value", "x", "y")

snp.coords <- geojsonsf::geojson_sf("input/Salonga GPS.geojson")

snp.pits <- data.frame(SITE = c("SNP-01", "SNP-03"),
                       LAT = c(-1.710124, -1.697417),
                       LONG = c(20.550024, 20.550064)) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326, remove = F)

p <- ggplot() + 
  geom_tile(data = hill_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradient(low = "black", high = "white", guide="none") + 
  ggnewscale::new_scale_fill() +
  geom_tile(data = dem_df, aes(x = x, y = y, fill = z), alpha = .4) + 
  scale_fill_gradientn("m ASL", colours = terrain.colors(10)) + 
  
  geom_sf(data = osm.rivers.lines, size = .5, color = '#44afe3') + 
  geom_sf(data = osm.rivers.poly, size = .5, fill = "#44afe3", color = '#44afe3') + 
  
  geom_sf(data = snp.coords %>% 
            dplyr::filter(grepl("PIT", Description)), 
          shape = 21, fill = "grey") + 
  
  geom_sf(data = snp.pits, shape = 21, fill = "white") + 
  geom_text_repel(data = snp.pits, aes(x = LONG, y = LAT, label = SITE), bg.color = "white", bg.r = .1) + 
  
  annotate("text", x = 20.585, y = -1.7, label = "Luilaka", fontface  = "bold", colour = "#44afe3", size = 5, angle = -50) + 
  
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) + 
  ggsn::scalebar(x.min = 20.53, x.max = 20.595, y.min = -1.725, y.max = -1.7,
                 dist = 1, dist_unit = "km",
                 transform = TRUE, model = "WGS84", 
                 st.dist = .06, st.size = 4, border.size = .2) + #, 
  #height = 1, st.dist = .1, 
  #border.size = .1, st.size = 2) + 
  #scale_x_continuous(breaks = seq(21, 22, .1), expand = c(0, 0)) + 
  #scale_y_continuous(breaks = seq(-.5, -.0, .1), expand = c(0,0)) + 
  coord_sf(xlim = c(xmin, xmax),
           ylim = c(ymin, ymax)) + 
  theme(legend.position = c(.057, .175), 
        axis.title = element_blank())
ggsave("Fig_Terrain_SNP.jpg", p, width = 7, height = 5, bg = "white", dpi = 900)

