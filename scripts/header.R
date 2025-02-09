library(ggplot2)
library(ggrepel)
library(ggthemes)
library(FactoMineR)
library(factoextra)
library(tidyr)

library(elevatr)
library(raster)

samples <- data.table::fread("input/TransGenTN_Samples_Excerpt.csv", encoding = "UTF-8")
samples$POTTERY <- sub(" .*", "", samples$POTTERY) # remove question marks

farbic.col <- c("#e41a1c", "#FF7F00", "#4DAF4A", "#377EB8", "#984EA3", "#A65628", "#FFFF33")

pottery <- read.csv(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv",
  encoding = "UTF-8")
# styleschrono$POSc <- as.character(styleschrono$POS)
pottery$FROM <- as.numeric(pottery$FROM)
pottery$TO <- as.numeric(pottery$TO)

sites <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.geojson") %>%
  sf::st_as_sf() # cf. https://github.com/r-spatial/sf/issues/1381#issuecomment-719130651
