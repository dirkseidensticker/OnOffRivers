# Map

source("scripts/header.R")

sites <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.csv", 
                           encoding = "UTF-8")

sites.unique <- sites %>%
  dplyr::distinct(SITE, LAT, LONG) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), 
               remove = F, 
               crs = 4326, 
               na.fail = F)

potterygroups <- data.table::fread(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", 
  encoding = "UTF-8") %>%
  dplyr::select(-ID, -DESCRIPTION)

bbox = c(xmin = 16, xmax = 23,
         ymin = -2.5, ymax = 2)

osm.rivers.lines <- geojsonsf::geojson_sf("input/gis/OSM_river_lines.geojson") %>%
  sf::st_crop(bbox)
sf::sf_use_s2(FALSE)
osm.rivers.poly <- geojsonsf::geojson_sf("input/gis/OSM_river_lakes_poly.geojson") %>%
  sf::st_make_valid() %>% sf::st_crop(bbox)
sf::sf_use_s2(TRUE)
osm.coast.line <- geojsonsf::geojson_sf("input/gis/OSM_coast_lines.geojson")

geo7 <- sf::st_read("input/gis/geology/geo7_2ag.shp") %>%
  sf::st_crop(bbox)

geo7.labs <- data.table::fread("input/gis/geology/glg.csv")

geo7.1 <- dplyr::left_join(geo7, geo7.labs, by = "GLG")


# OVERVIE MAP INSERT ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

white <- sf::st_read("input/whitesveg/Whites vegetation.shp") %>%
  sf::st_set_crs(4326) %>%
  dplyr::filter(DESCRIPTIO %in% c("Anthropic landscapes",
                                  "Dry forest and thicket",
                                  "Swamp forest and mangrove",
                                  "Tropical lowland rainforest"))

minimap <- ggplot(data = world) +
  geom_sf(color = NA, fill = "grey") + 
  geom_sf(data = sf::st_union(white) %>% sf::st_crop(xmin = -15, xmax = 30, ymin = -10, ymax = 10), 
          fill = "black", color = NA, alpha = .5) + 
  geom_rect(xmin = 16, xmax = 22, 
            ymin = -2, ymax = 2, 
            fill = NA, color = "black") + 
  coord_sf(xlim = c(-15, 50), 
           ylim = c(-35, 35)) + 
  theme_void() + 
  theme(panel.border = element_rect(colour = "darkgrey", 
                                    fill = NA, size = .5), 
        panel.background = element_rect(fill = "white"))

# SITES IN CASE STUDIES ----

sites.casestudies <- sites.unique %>%
  dplyr::filter(SITE %in% c("Pikunda", "Munda", "Wafanya", "Monkoto", "Salonga"))

sites.group.c <- samples %>% 
  dplyr::filter(GROUP == "C") %>% 
  dplyr::distinct(SITE) %>% 
  as.data.frame() %>%
  dplyr::filter(SITE != "Pikunda") %>%
  dplyr::left_join(sites.unique) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326, remove = F)

# Overview Map ----

plt.map.general <- rbind(sites.casestudies %>% 
        dplyr::mutate(CLASS = "CASESTUDY"), 
      sites.group.c %>% 
        dplyr::mutate(CLASS = "GROUP_C")) %>%
  ggplot() +
    geom_sf(data = osm.rivers.lines, size = .5, color = '#44afe3') + 
    geom_sf(data = osm.rivers.poly, size = .5, fill = "#44afe3", color = '#44afe3') + 
    #geom_sf(data = sites.unique, shape = 21, fill = "white", color = "black") + 
    
    geom_sf(aes(fill = CLASS), shape = 21, color = "white", size = 2) + 
    scale_fill_manual(values = c("black", "grey50")) + 

    #geom_sf(data = sites.group.c, shape = 21, fill = "grey", color = "white") + 
    #geom_sf(shape = 21, fill = "black", color = "white") + 
  
    geom_text_repel(aes(x = LONG, y = LAT, 
                        label = SITE, color = CLASS, size = CLASS), 
                    bg.color = "white", bg.r = .1) + 
  
  
    geom_sf(data = sites.unique %>%
              dplyr::filter(SITE %in% c("Ikenge", "Imbonga", "Nkuse", "Isaka-Elinga", "Lotoko")), 
            fill = "grey50", 
            shape = 21, color = "white", size = 2) + 
    geom_text_repel(data = sites.unique %>%
                      dplyr::filter(SITE %in% c("Ikenge", "Imbonga", "Nkuse", "Isaka-Elinga", "Lotoko")) %>%
                      dplyr::mutate(CLASS = NA), 
                    aes(x = LONG, y = LAT,
                        label = SITE,), 
                    size = 3.5, color = "grey50",
                    bg.color = "white", bg.r = .1) + 
  
    #geom_label_repel(aes(x = LONG, y = LAT, label = SITE, fill = CLASS, size = CLASS), color = "white", min.segment.length = 0) + 
  
    annotate("text", x = 17, y = -.25, label = "Sangha", fontface  = "bold", colour = "#44afe3", size = 3, angle = -85) + 
    annotate("text", x = 17.6, y = 0.5, label = "Likwala-", fontface  = "bold", colour = "#44afe3", size = 3, angle = -55) + 
    annotate("text", x = 17.45, y = 0.5, label = "aux-Herbes", fontface  = "bold", colour = "#44afe3", size = 3, angle = -55) + 
    annotate("text", x = 21, y = -2.2, label = "Luilaka", fontface  = "bold", colour = "#44afe3", size = 3, angle = -45) + 

    annotate("text", x = 18.75, y = 1.5, label = "Congo", fontface  = "bold", colour = "#44afe3", size = 3, angle = 35) + 
    annotate("text", x = 16.5, y = -2.2, label = "Congo", fontface  = "bold", colour = "#44afe3", size = 3, angle = 35) + 
    annotate("text", x = 17.9, y = 1.4, label = "Ubangi", fontface  = "bold", colour = "#44afe3", size = 3, angle = 70) + 
    annotate("text", x = 21.5, y = -1.2, label = "Salonga", fontface  = "bold", colour = "#44afe3", size = 3, angle = -25) + 
    annotate("text", x = 21.4, y = -0.25, label = "Tshuapa", fontface  = "bold", colour = "#44afe3", size = 3, angle = -10) + 
    annotate("text", x = 21.1, y = 0.6, label = "Maringa", fontface  = "bold", colour = "#44afe3", size = 3, angle = -45) + 
  
    scale_color_manual(values = c("black", "grey50")) + 
    scale_size_manual(values = c(3.5, 2.5)) + 
    scale_x_continuous(breaks = seq(17, 25, 1)) + 
    scale_y_continuous(breaks = seq(-2, 2, 1)) + 
    coord_sf(xlim = c(16.5, 21.5), 
             ylim = c(-2.2, 1.5)) + 
    theme_few() + 
    theme(axis.title = element_blank(),
          legend.position = "none")

plt.map.general <- cowplot::ggdraw(plt.map.general) +
  cowplot::draw_plot(minimap, 
                     x = .06, y = .08, width = .33, height = .33)

# Geological maps per site (10 km radius) ----

sites.catchment <- sites.casestudies %>%
  sf::st_transform(32733) %>%
  sf::st_buffer(10000) %>%
  sf::st_transform(4326) %>%
  dplyr::arrange(LONG)
  
geo7.filt <- dplyr::filter(geo7, GLG != "H2O" & GLG != "SEA")


snp.pits <- data.frame(SITE = c("SNP-01", "SNP-03"),
                       LAT = c(-1.710124, -1.697417),
                       LONG = c(20.550024, 20.550064)) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326, remove = F)

p <- list()
for(i in 1:nrow(sites.catchment)){
  # local geology
  glg.site <- sf::st_intersection(
    geo7.filt,
    sites.catchment[i,1])
  
  glg.units <- data.frame(GLG = unique(glg.site$GLG))
  glg.units <- dplyr::left_join(glg.units, geo7.labs, by = "GLG")
  glg.units <- glg.units[order(glg.units$GLG),]
  
  bbox.site <- sf::st_bbox(glg.site)
  
  sf::sf_use_s2(FALSE)
  
  p[[i]] <- ggplot() + 
    geom_sf(data = glg.site, aes(fill = GLG), color = "grey", size = .5) + 
    scale_fill_manual(values = as.character(glg.units$col)) + 
    geom_sf(data = osm.rivers.lines %>% 
              sf::st_intersection(sites.catchment[i,1]), 
            size = .5, color = '#44afe3') + 
    geom_sf(data = osm.rivers.poly %>% 
              sf::st_intersection(sites.catchment[i,1]), 
            size = .5, fill = "#44afe3", color = '#44afe3') + 
    #geom_sf(data = osm.rivers.lines %>% sf::st_intersection(sites.catchment[i,1])) + 
    geom_sf(data = sf::st_centroid(sites.catchment[i,1]), size = 2) + 
    
    geom_sf(data = sf::st_transform(sf::st_buffer(sf::st_transform(sf::st_centroid(sites.catchment[i,1]),crs = 32733),7000),crs = 4326), fill = NA, linetype = "dashed") + # 7km radius cf Whitebread 2001
    geom_sf(data = sf::st_transform(sf::st_buffer(sf::st_transform(sf::st_centroid(sites.catchment[i,1]),crs = 32733),3000),crs = 4326), fill = NA, linetype = "dashed") + # 3 km radius cf 
    ggtitle(label = sites.catchment[i,"SITE"]) + 
    theme_void() + 
    theme(legend.position = "none", 
          plot.title = element_text(hjust = 0.5))

  if (sites.catchment[i,]$SITE == "Salonga") {
    p[[i]] <- p[[i]] + 
      geom_sf(data = snp.pits, shape = 21, fill = "white") + 
      geom_text_repel(data = snp.pits, aes(x = LONG, y = LAT, label = SITE))
  }  
}
plt.sitechatchment.glg <- do.call(gridExtra::grid.arrange, p)

# legend ----

glg.order <- c("Holocene", "Cenozoic", "Cretaceous")

glg.col <- data.frame(
  GLG = c("Qe", "QT", "K")) %>%
  #dplyr::mutate(GLG = factor(GLG, levels = c("Qe", "QT", "K", "Kl"))) %>%
  dplyr::left_join(geo7.labs, by = "GLG")

glg.col$Label <- factor(glg.col$Label, levels = glg.order)

glg.col <- glg.col %>% 
  dplyr::select(col) %>%
  dplyr::pull()


glg.legend.join <- geo7.filt %>%
  dplyr::left_join(
    data.frame(
      GLG = unique(geo7.filt$GLG)
    ) %>% 
      dplyr::left_join(geo7.labs, by = "GLG"), 
    by = "GLG")

glg.legend.join$Label <- factor(glg.legend.join$Label, levels = glg.order)

glg.legend.plt <- ggplot(glg.legend.join %>% dplyr::filter(!is.na(Label))) + 
  geom_sf(aes(fill = Label)) + 
  scale_fill_manual(values = glg.col) + 
  theme(legend.title = element_blank(), 
        legend.position = "top")

glg.legend <- cowplot::get_legend(glg.legend.plt)

# combined plot ---
plt.sitechatchment.glg.ldg <- cowplot::ggdraw(plt.sitechatchment.glg) +
  cowplot::draw_plot(glg.legend, 
                     x = .65, y = .05, width = .2, height = .2)

ggsave("Fig_Map_Geol.pdf", plt.sitechatchment.glg.ldg, width = 6, height = 8, bg = "white")
ggsave("output/map_geol.jpg", plt.sitechatchment.glg.ldg, width = 6, height = 8, bg = "white")

# version with topography: ----
p <- list()
for(i in 1:nrow(sites.catchment)){
    
  bbox <- sites.catchment[i,1] %>% sf::st_bbox()
  
  locations <- data.frame(x = c(bbox[[1]], bbox[[3]]), 
                          y = c(bbox[[2]], bbox[[4]]))  
  
  # get gem
  dem <- elevatr::get_elev_raster(locations = locations, 
                                  prj = "EPSG:4326", 
                                  z = 12, 
                                  clip = "bbox")
  
  #plot(dem)
  
  poly <- as(sites.catchment[i,1], "Spatial")
  
  dem <- dem %>% raster::mask(poly)
  
  dem_df <- raster::as.data.frame(dem, xy=TRUE)
  names(dem_df) <- c("x", "y", "z")
  
  dem_df <- dplyr::filter(dem_df, z >= 0)
  
  # hist(dem_df$z)
  
  # create slope and hillshade
  slope = raster::terrain(dem, opt='slope')
  aspect = raster::terrain(dem, opt='aspect')
  hill = raster::hillShade(slope, aspect, 40, 270)
  
  # plot(hill)
  
  hill_df <- as(hill, "SpatialPixelsDataFrame")
  hill_df <- as.data.frame(hill_df)
  colnames(hill_df) <- c("value", "x", "y")
  
  sf::sf_use_s2(FALSE)
  
  p[[i]] <- ggplot() + 
    
    geom_tile(data = hill_df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradient(low = "black", high = "white", guide="none") + 
    ggnewscale::new_scale_fill() +
    geom_tile(data = dem_df, aes(x = x, y = y, fill = z), alpha = .4) + 
    scale_fill_gradientn("m ASL", colours = terrain.colors(10)) + 
    
    geom_sf(data = osm.rivers.lines %>% 
              sf::st_intersection(sites.catchment[i,1]), 
            size = .5, color = '#44afe3') + 
    geom_sf(data = osm.rivers.poly %>% 
              sf::st_intersection(sites.catchment[i,1]), 
            size = .5, fill = "#44afe3", color = '#44afe3') + 
    
    geom_sf(data = sf::st_centroid(sites.catchment[i,1]), size = 2) + 
    
    geom_sf(data = sf::st_transform(sf::st_buffer(sf::st_transform(sf::st_centroid(sites.catchment[i,1]),crs = 32733),7000),crs = 4326), fill = NA, linetype = "dashed") + # 7km radius cf Whitebread 2001
    geom_sf(data = sf::st_transform(sf::st_buffer(sf::st_transform(sf::st_centroid(sites.catchment[i,1]),crs = 32733),3000),crs = 4326), fill = NA, linetype = "dashed") + # 3 km radius cf 
    ggtitle(label = sites.catchment[i,"SITE"]) + 
    theme_void() + 
    theme(legend.position = c(0, .15), 
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 7),
          legend.key.height = unit(.25, "cm"),
          legend.key.width = unit(.1, "cm"),
          legend.background = element_rect(fill="white", color = "white"),
          plot.title = element_text(hjust = 0.5))

  if (sites.catchment[i,]$SITE == "Salonga") {
    p[[i]] <- p[[i]] + 
      geom_sf(data = snp.pits, shape = 21, fill = "white") + 
      geom_text_repel(data = snp.pits, aes(x = LONG, y = LAT, label = SITE))
  } 
}

plt.sitechatchment.top <- do.call(gridExtra::grid.arrange, p)

plt.map <- cowplot::plot_grid(
  plt.map.general, 
  plt.sitechatchment.top,
  ncol = 1,
  rel_heights = c(1, 2), 
  labels = "AUTO"
)
ggsave("Fig_Map_Top.jpg", plt.map, width = 6, height = 12, bg = "white")