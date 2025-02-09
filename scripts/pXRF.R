########
# pXRF #
########

# Measurements were conducted on Feb. 22 & Mar. 14 2024 using a **Hitachi X-MET8000 Expert GEO** handheld XRF device.
# Each sherd was measured three times, changing measuring spot between each measurements.
# The following settings were used: **60 sec** measuring time and calibration was set to *"Aardwerke"* (pottery).
# The software *"XRF Analyzer WinGUI on Desktop PC"* was used to review the measured data on a desktop PC.

# 1 Setup ----

source("scripts/header.R")

library(tidyverse)
library(ggthemes)

s <- samples
s.all <- data.table::fread("input/TransGenTN_Samples.csv", encoding = "UTF-8")

# measurments in long format
m <- data.table::fread("input/meas.csv", encoding = "UTF-8")

# convert pct to wide format
m.w <- reshape2::dcast(m, MEAS ~ ELEMENT, value.var = "pct")

# concordance table measurments - sample id's
m.id <- xlsx::read.xlsx2("input/meas-ID.xlsx", sheetIndex = 1) %>%
  dplyr::mutate(MEAS = as.numeric(MEAS), 
                ID = as.numeric(ID))

# filter of id's in this study
filt <- s %>% dplyr::pull(ID)

## 1.1 Map of the sites ----

bb = c(xmin = 15, xmax = 26, ymin = -3, ymax = 6)

osm.rivers.lines <- geojsonsf::geojson_sf("input/gis/OSM_river_lines.geojson") %>% sf::st_crop(bb)
sf::sf_use_s2(FALSE)
osm.rivers.poly <- geojsonsf::geojson_sf("input/gis/OSM_river_lakes_poly.geojson") %>%
  sf::st_make_valid() %>% sf::st_crop(bb)
sf::sf_use_s2(TRUE)

s %>%
  dplyr::distinct(SITE, CODE) %>%
  dplyr::left_join(
    data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.csv") %>%
      dplyr::distinct(SITE, LONG, LAT), by = "SITE") %>%
  ggplot() + 
    geom_sf(data = osm.rivers.lines, size = .5, color = 'grey') + 
    geom_sf(data = osm.rivers.poly, size = .5, fill = "grey", color = 'grey') + 
    geom_point(aes(x = LONG, y = LAT)) + 
    ggrepel::geom_text_repel(aes(x = LONG, y = LAT, label = tolower(CODE))) + 
    coord_sf(xlim = c(16, 21), 
             ylim = c(-2, 2)) + 
  theme_base() + 
  theme(axis.title = element_blank(), 
        plot.background = element_blank())
ggsave("output/xrf_sites.jpg", width = 8, height = 6)


# 2 Check min, mean, max & sd for each element ----

m %>%
  dplyr::group_by(ELEMENT) %>%
  dplyr::summarise(n = length(pct),
                   min = min(pct), 
                   mean = mean(pct), 
                   max = max(pct), 
                   sd = sd(pct)) %>%
  dplyr::arrange(desc(n)) %>%
  print(n = nrow(.))

## 2.1 Only keep elements that were regularly detected ----

m %>%
  dplyr::group_by(ELEMENT) %>%
  dplyr::summarise(n = length(pct)) %>%
  dplyr::mutate(pct = n / length(unique(m$MEAS)) * 100) %>% 
  dplyr::arrange(desc(pct)) %>%
  ggplot(aes(x = pct, y = reorder(ELEMENT, pct))) + 
    geom_bar(stat = "identity") + 
    geom_vline(xintercept = 90, color = "red", linetype = "dashed")

e.retain <- m %>%
  dplyr::group_by(ELEMENT) %>%
  dplyr::summarise(n = length(pct)) %>%
  dplyr::filter(n > 400) %>%
  dplyr::pull(ELEMENT)

m <- m %>%
  dplyr::filter(ELEMENT %in% e.retain)

## 2.2 Remove outliers and average measurements ----

# Outliers were removed following Dixon's Q test

### 2.2.1 Find and flag outliers ----
id.lst <- as.numeric(unique(m.id$ID)) # list of measured samples
e.lst <- unique(m$ELEMENT)
m.lst <- list()
m.e.lst <- list()
cnt <- 1

for (i in 1:length(id.lst)) { # loop through measurements
  for (j in 1:length(e.lst)) {
    meas <- m %>% 
      dplyr::left_join(m.id, by = "MEAS")  %>% 
      dplyr::filter(ID == id.lst[[i]] & ELEMENT == e.lst[[j]])
    
    if (nrow(meas) > 2 & length(unique(meas$pct)) > 1) { # must be all three measurments & they should be different from another
      #### 2.2.1.1 Dixon's Q Test für n = 3 ----
      q.res <- outliers::dixon.test(
        meas %>% 
          dplyr::arrange(pct) %>%
          dplyr::pull(pct))
      
      if (q.res$statistic[[1]] >= 0.941) {
        # state with outlier
        meas.res <- rbind(
          meas %>%
            dplyr::filter(pct == readr::parse_number(q.res$alternative)) %>%
            dplyr::mutate(outlier = TRUE),
          meas %>%
            dplyr::filter(pct != readr::parse_number(q.res$alternative)) %>%
            dplyr::mutate(outlier = FALSE)
        )
      } else {
        # state without outlier
        meas.res <- meas %>%
          dplyr::filter(pct != readr::parse_number(q.res$alternative)) %>%
          dplyr::mutate(outlier = FALSE)
      }
    } else {
      meas.res <- meas %>%
        dplyr::mutate(outlier = FALSE)
    }
    m.e.lst[[cnt]] <- meas.res
    cnt <- cnt + 1
  }
}

### 2.2.2 Inspect outliers ----

### 2.2.3 Remove outliers ----

m <- do.call(rbind, m.e.lst) %>%
  dplyr::filter(outlier == FALSE)

# convert  to wide format
m.w <- reshape2::dcast(m, MEAS ~ ELEMENT, value.var = "pct")

# 3 Check measured elements ----

# Plot individual elements measured
e <- unique(m$ELEMENT)
e.lst <- list()
for (i in 1:length(e)) {
  e.sort <- m %>%
    dplyr::filter(ELEMENT == e[[i]]) %>%
    dplyr::arrange(pct) %>%
    tibble::rowid_to_column("cnt")
  e.lst[[i]] <- e.sort
}
m.sort <- do.call(rbind, e.lst)

ggplot(m.sort, 
       aes(
         x = cnt, 
         y = pct, 
         ymin = pct - err, 
         ymax = pct + err)) + 
  geom_pointrange(size = .1, color = "grey") + 
  geom_point() + 
  facet_wrap(ELEMENT ~ ., 
             scales = "free") + 
  theme(axis.title = element_blank()) + 
  theme_base() + 
  theme(plot.background = element_blank(), 
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("output/xrf_elements.jpg", width = 10, height = 10)

## 3.1 Get min, mean, sd &, max for each sample ----

m.sum <- m.w %>%
  dplyr::left_join(m.id %>% dplyr::select(-NOTES), by = "MEAS") %>%
  tibble::column_to_rownames("MEAS") %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(
    dplyr::across(
      dplyr::everything(), 
      .f = list(
        min = min,
        mean = mean,
        max = max, 
        sd = sd), na.rm = TRUE))
  
m.pct <- m.sum %>% 
  tibble::column_to_rownames("ID") %>%
  dplyr::select(dplyr::contains("mean")) %>% 
  dplyr::rename_with(~stringr::str_remove(., '_mean'))

m.pct[m.pct == "NaN"] <- NA
m.pct.no.nan <- m.pct[colSums(is.na(m.pct)) == 0] # remove cols NA values

# TODO: standard deviation of error per element ----
e.sd <- m.pct %>%
  reshape2::melt() %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(sd = sd(value, na.rm=TRUE))

# 4 Inspection of relationships of specific elements (biplots) ----

m.sum %>%
  dplyr::left_join(s, by = "ID") %>%
  dplyr::filter(ID %in% filt) %>%
  ggplot(aes(x = Fe_mean, y = Al_mean)) + 
  geom_point(data = m.w, aes(x = Fe, y = Al), color = "grey") + 
  geom_point(data = m.sum %>% dplyr::left_join(s, by = "ID") %>% dplyr::filter(ID %in% filt), aes(x = Fe_mean, y = Al_mean)) + 
  #geom_point(data = m.w %>% dplyr::left_join(m.id, by = "MEAS") %>% dplyr::left_join(s, by = "ID"), aes(x = Fe, y = Al)) + 
  geom_crossbar(aes(xmin = Fe_min, xmax = Fe_max)) + 
  geom_crossbar(aes(ymin = Al_min, ymax = Al_max)) + 
  facet_wrap(RIVER ~ .) + 
  scale_x_continuous("Fe") + 
  scale_y_continuous("Al") + 
  theme_base() + 
  theme(plot.background = element_blank())
ggsave("output/xrf_fe-al_rivers.jpg", width = 10, height = 6)


cowplot::plot_grid(
  ggplot(m.pct, aes(x = Fe, y = Al)) + 
    geom_point(color = "grey") + 
    geom_point(data = m.pct %>% dplyr::filter(rownames(m.pct) %in% filt), aes(x = Fe, y = Al)) + 
    theme_base() + 
    theme(plot.background = element_blank()), 
  ggplot(m.pct, aes(x = K, y = Al)) + 
    geom_point(color = "grey") + 
    geom_point(data = m.pct %>% dplyr::filter(rownames(m.pct) %in% filt), aes(x = K, y = Al)) + 
    theme_base() + 
    theme(plot.background = element_blank()), 
  ggplot(m.pct, aes(x = Fe, y = Si)) + 
    geom_point(color = "grey") + 
    geom_point(data = m.pct %>% dplyr::filter(rownames(m.pct) %in% filt), aes(x = Fe, y = Si)) + 
    theme_base() + 
    theme(plot.background = element_blank()), 
  ggplot(m.pct, aes(x = K, y = Si)) + 
    geom_point(color = "grey") + 
    geom_point(data = m.pct %>% dplyr::filter(rownames(m.pct) %in% filt), aes(x = K, y = Si)) + 
    theme_base() + 
    theme(plot.background = element_blank()),
  align = 'hv', axis = 'tblr'
)
#ggsave("Fig_XRF_biplots_fe-al-k-si.pdf", width = 10, height = 10)
ggsave("output/xrf_fe-al-k-si.jpg", width = 10, height = 10)


## 4.1 Discriminant Analysis (cf. Puerta-Schardt) ----

m.pct.prov <- m.pct %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::filter(
    ID %in% c(
      s.all %>%
      dplyr::filter(PROV == "x") %>% # filter samples with know provenance:
      dplyr::pull(ID))) %>%
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::left_join(s.all %>% dplyr::select(ID, SITE), by = "ID") %>%
  tibble::column_to_rownames("ID") %>% 
  replace(is.na(.), 0) # samples with NA will be dropped, thus replace with 0 at this stage (elements are below detection?)
m.pct.prov

e.remove <- m.pct.prov %>%
  reshape2::melt() %>%
  dplyr::group_by(SITE, variable) %>%
  dplyr::summarise(sd = sd(value)) %>% 
  dplyr::filter(sd == 0) %>%
  dplyr::ungroup() %>%
  dplyr::distinct(variable) %>% dplyr::pull(variable)

m.pct.prov.el <- m.pct.prov %>%
  dplyr::select(-SITE) %>% 
  colSums() %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("el") %>% 
  dplyr::filter(. > 0) %>%
  dplyr::pull(el)

m.pct.prov <- m.pct.prov %>%
  select(-dplyr::all_of(e.remove), SITE)

# cf. http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/

model <- MASS::lda(SITE~., 
                   data = m.pct.prov) 

model

sink(file = "output/xrf_lda.jpg.txt")
model
sink(file = NULL)

# table:

model$scaling %>%
  as.data.frame() %>% 
  dplyr::select(LD1) %>% 
  dplyr::arrange(desc(LD1))

lda.data <- cbind(
  m.pct.prov %>% 
    dplyr::filter(rownames(.) %in% c(rownames(predict(model)$x))), 
  predict(model)$x)

plt.xrf.lda.bi <- ggplot(lda.data, aes(x = LD1, y = LD2)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_point(aes(fill = SITE), size = 3, shape = 21, color = "white") +
  ggrepel::geom_text_repel(
    data = lda.data %>%
      dplyr::group_by(SITE) %>%
      dplyr::summarise(LD1 = mean(LD1), 
                       LD2 = mean(LD2)), 
    aes(x = LD1, y = LD2, label = SITE, color = SITE), 
    bg.color = "white", bg.r = .1) + 
  #coord_equal() +
  theme_base() + 
  theme(plot.background = element_blank(), 
        legend.position = "none")

# map of sites:
sites.xrf.sf <- sites %>%
  dplyr::filter(SITE %in% c(s.all %>%
                              dplyr::filter(ID %in% c(m.id %>% 
                                                        dplyr::distinct(ID) %>% 
                                                        dplyr::pull(ID))) %>%
                              dplyr::distinct(SITE) %>%
                              dplyr::pull(SITE))) %>%
  dplyr::filter(!SITE %in% c("Abang Minko'o", "Bwambé-Sommet (Est)", "Mukila"))

sites.xrf.sf.bb <- sites.xrf.sf %>% sf::st_bbox()

sites.xrf.prov <- sites.xrf.sf %>%
  dplyr::filter(SITE %in% c(m.pct.prov %>%
                              dplyr::distinct(SITE) %>%
                              dplyr::pull(SITE)))

plt.xrf.lda.sites <- ggplot() + 
  geom_sf(data = osm.rivers.lines, size = .5, color = 'grey') + 
  geom_sf(data = osm.rivers.poly, size = .5, fill = "grey", color = 'grey') + 
  geom_sf(data = sites.xrf.sf,
          shape = 21, fill = "black", color = "white") + 
  geom_sf(data = sites.xrf.prov, 
          aes(fill = SITE), shape = 21, color = "white", size = 3) + 
  ggrepel::geom_text_repel(
    data = sites.xrf.prov, 
    aes(x = LONG, y = LAT, label = SITE, color = SITE), 
    bg.color = "white", bg.r = .1, 
    max.overlaps = Inf) + 
  scale_x_continuous(breaks = seq(14, 26, 2)) + 
  scale_y_continuous(breaks = seq(-2, 6, 2)) + 
  coord_sf(xlim = c(sites.xrf.sf.bb[[1]], sites.xrf.sf.bb[[3]]), 
           ylim = c(sites.xrf.sf.bb[[2]], sites.xrf.sf.bb[[4]])) + 
  theme_base() + 
  theme(axis.title = element_blank(), 
        plot.background = element_blank(), 
        legend.position = "none")

plt.xrf.lda <- cowplot::plot_grid(
  plt.xrf.lda.bi, 
  plt.xrf.lda.sites, 
  ncol = 1, 
  labels = "AUTO", 
  rel_heights = c(1, 1.25), 
  align = "v", axis = "rl"
)
ggsave("output/xrf_lda.jpg", plt.xrf.lda, width = 8, height = 10)
ggsave("Fig_XRF_prov_LCA.pdf", plt.xrf.lda, width = 8, height = 10)

## 4.2 subsetting elements ----

### 4.2.1 Output of LDA ----

e.sel <- model$scaling %>% 
  as.data.frame() %>% 
  dplyr::select(LD1) %>% 
  dplyr::arrange(desc(LD1)) %>% 
  dplyr::filter(LD1 > 0) %>% 
  tibble::rownames_to_column("E") %>% 
  dplyr::pull(E)

# retained elements:

sort(e.sel)

length(e.sel)

# _not_ retained elements:

model$scaling %>% 
  as.data.frame() %>% 
  dplyr::select(LD1) %>% 
  dplyr::arrange(desc(LD1)) %>% 
  dplyr::filter(LD1 <= 0)

### 4.2.2 Manual check for leached elements ----

e.sel <- e.sel[-which(e.sel %in% c("P", "Rb", "Ta", "Th") )]
  
m.pct.sel <- m.pct %>% 
  dplyr::select(dplyr::all_of(e.sel))

## 4.2 check for correlations in sub-setted dataset ----

library(GGally)

m.pct.sel.corr <- m.pct.sel %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::filter(ID %in% c(samples %>% dplyr::pull(ID))) %>%
  tibble::column_to_rownames("ID") %>%
  dplyr::select(order(colnames(.))) %>%
  ggpairs() + 
  theme_base() + 
  theme(plot.background = element_blank())
#m.pct.sel.corr
ggsave("output/xrf_elements_sel_neCB_corr.jpg", m.pct.sel.corr, width = 10, height = 10)

# 5 Principal Component Analysis ----

#rowSums(m.pct.sel)
m.pct.sel <- m.pct.sel * 100 / rowSums(m.pct.sel, na.rm = T)
# rowSums(m.pct.sel)

m.pct.cb <- m.pct.sel %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::filter(
    ID %in% c(s.all %>% 
                dplyr::filter(!CODE %in% 
                                c("BWS", "ABM", "MUK") & 
                                !is.na(labels)) %>% 
                dplyr::pull(ID))) %>% # only samples from Congo Basin
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::left_join(s.all %>% dplyr::select(ID, labels) %>% dplyr::mutate(ID = as.numeric(ID)), by = "ID") %>%
  dplyr::select(-ID) %>%
  tibble::column_to_rownames("labels")

## 5.1 western Congo Basin ----

filt1 <- s %>% 
  dplyr::filter(!is.na(labels) & SITE %in% c("Pikunda", "Munda")) %>%
  dplyr::pull(labels)

pca.res <- m.pct.cb %>%
  FactoMineR::PCA(graph = F)

pca.res.samples <- pca.res[["ind"]][["coord"]] %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(s, by = "labels")

leg <- cowplot::get_legend(
  ggplot(pca.res.samples %>%
           dplyr::filter(labels %in% filt1) %>% 
           dplyr::left_join(pottery, by = "POTTERY") %>%
           dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
         aes(x = Dim.1, y = Dim.2, fill = POTTERY, shape = SITE)) + 
    geom_point(size = 4) + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::filter(labels %in% filt1) %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    scale_shape_manual(values = c(24, 25)) + 
    guides(fill = guide_legend(override.aes=list(shape = 22, size = 4))) + 
    theme_void() + 
    theme(legend.text=element_text(size=14))
)

p.rowA <- cowplot::plot_grid(
  FactoMineR::plot.PCA(pca.res, choix = "var") + 
    theme_void() + 
    theme(plot.title = element_blank(), 
          plot.background = element_rect(color = NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()),
  ggplot(pca.res.samples, 
         aes(x = Dim.1, y = Dim.2, label = labels)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(color = "grey", shape = 16) + 
    geom_point(data = pca.res.samples %>%
                 dplyr::filter(labels %in% filt1) %>% 
                 dplyr::left_join(pottery, by = "POTTERY") %>%
                 dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
               aes(x = Dim.1, y = Dim.2, fill = POTTERY, shape = SITE), 
               size = 4) + 
    scale_shape_manual(values = c(24, 25)) + 
    guides(fill = guide_legend(override.aes=list(shape = 22))) + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::filter(labels %in% filt1) %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    scale_x_continuous(paste0("Dim 1 (", round(pca.res$eig[1,2],2), "%)"), position = "top") + 
    scale_y_continuous(paste0("Dim 2 (", round(pca.res$eig[2,2],2), "%)"),
                       breaks = seq(-6, 6, 2), position = "right") + 
    ggthemes::theme_base() + 
    theme(legend.position = "none", 
          plot.background = element_blank()),
  rel_widths = c(.5, 1), 
  nrow = 1)

# run only filtered samples

m.pct.cb[is.na(m.pct.cb)] <- 0

pca.res <- m.pct.cb %>% 
  dplyr::filter(row.names(m.pct.cb) %in% filt1) %>%
  FactoMineR::PCA(graph = F)

pca.res.samples <- pca.res[["ind"]][["coord"]] %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(s, by = "labels")

p.rowB <-cowplot::plot_grid(
  FactoMineR::plot.PCA(pca.res, choix = "var") + 
    theme_void() + 
    theme(plot.title = element_blank(), 
          plot.background = element_rect(color = NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()),
  ggplot(pca.res.samples %>%
           dplyr::left_join(pottery, by = "POTTERY") %>%
           dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
         aes(x = Dim.1, y = Dim.2, label = labels, 
             fill = POTTERY, shape = SITE)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = 4) + 
    geom_text_repel(bg.r = .15, bg.color = "white") + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    scale_shape_manual(values = c(24, 25)) + 
    scale_x_continuous(paste0("Dim 1 (", round(pca.res$eig[1,2],2), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(pca.res$eig[2,2],2), "%)"), position = "right") + 
    ggthemes::theme_base() + 
    theme(legend.position = "none", 
          plot.background = element_blank()),
  rel_widths = c(.5, 1), 
  nrow = 1)

p <- cowplot::plot_grid(
  cowplot::plot_grid(
    p.rowA, 
    p.rowB, 
    ncol = 1, 
    align = "v", axis = "lr", labels = "AUTO"
  ), 
  leg, 
  nrow = 1, 
  rel_widths = c(4, 1)
)
ggsave("output/xrf_pca_pik-mun.jpg", p, width = 12, height = 12)
ggsave("Fig_XRF_pca_pik-mun.pdf", p, width = 10, height = 7)

### 5.1.1 Clustering ----

xrf.PCA.hcpc <- FactoMineR::HCPC(
  pca.res,
  nb.clust = 0,
  graph = FALSE,
  iter.max = 100,
  min = 3,
  max = NULL)

factoextra::fviz_cluster(xrf.PCA.hcpc) + 
  ggthemes::theme_base() + 
  theme(plot.background = element_blank())
ggsave("output/xrf_pca_pik-mun_cluster.jpg", width = 6, height = 6)

sink(file = "output/xrf_pca_pik-mun_cluster.jpg.txt")
xrf.PCA.hcpc$desc.var
sink(file = NULL)

xrf.wCB.PCA.hcpc.clust <- xrf.PCA.hcpc$data.clust %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(samples, by = "labels") %>% 
  dplyr::select(labels, clust)  

## 5.2 Luilaka ----

filt2 <- s %>% 
  dplyr::filter(!is.na(labels) & RIVER == "Luilaka") %>%
  dplyr::pull(labels)

pca.res <- m.pct.cb %>%
  FactoMineR::PCA(graph = F)

pca.res.samples <- pca.res[["ind"]][["coord"]] %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(s, by = "labels")

leg <- cowplot::get_legend(
  ggplot(pca.res.samples %>%
           dplyr::filter(labels %in% filt2) %>% 
           dplyr::left_join(pottery, by = "POTTERY") %>%
           dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
         aes(x = Dim.1, y = Dim.2, fill = POTTERY, shape = SITE)) + 
    geom_point(size = 4) + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::filter(labels %in% filt2) %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    scale_shape_manual(values = c(24, 22, 25)) + 
    guides(fill = guide_legend(override.aes=list(shape = 22, size = 4))) + 
    theme_void() + 
    theme(legend.text=element_text(size=14))
)

p.rowA <- cowplot::plot_grid(
  FactoMineR::plot.PCA(pca.res, choix = "var") + 
    theme_void() + 
    theme(plot.title = element_blank(), 
          plot.background = element_rect(color = NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()),
  ggplot(pca.res.samples, 
         aes(x = Dim.1, y = Dim.2, label = labels)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(color = "grey", shape = 16) + 
    geom_point(data = pca.res.samples %>%
                 dplyr::filter(labels %in% filt2) %>% 
                 dplyr::left_join(pottery, by = "POTTERY") %>%
                 dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
               aes(x = Dim.1, y = Dim.2, fill = POTTERY, shape = SITE), size = 4) + 
    scale_shape_manual(values = c(24, 22, 25)) + 
    guides(fill = guide_legend(override.aes=list(shape = 22))) + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::filter(labels %in% filt2) %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    
    scale_x_continuous(paste0("Dim 1 (", round(pca.res$eig[1,2],2), "%)"), position = "top") + 
    scale_y_continuous(paste0("Dim 2 (", round(pca.res$eig[2,2],2), "%)"),
                       breaks = seq(-6, 6, 2), position = "right") + 
    ggthemes::theme_base() + 
    theme(legend.position = "none", 
          plot.background = element_blank()),
  rel_widths = c(.5, 1), 
  nrow = 1)

# run only filtered samples

m.pct.cb[is.na(m.pct.cb)] <- 0

pca.res <- m.pct.cb %>% 
  dplyr::filter(row.names(m.pct.cb) %in% filt2) %>%
  FactoMineR::PCA(graph = F)

pca.res.samples <- pca.res[["ind"]][["coord"]] %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(s, by = "labels")

p.rowB <- cowplot::plot_grid(
  FactoMineR::plot.PCA(pca.res, choix = "var") + 
    theme_void() + 
    theme(plot.title = element_blank(), 
          plot.background = element_rect(color = NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()),
  ggplot(pca.res.samples %>%
           dplyr::left_join(pottery, by = "POTTERY") %>%
           dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")), 
         aes(x = Dim.1, y = Dim.2, label = labels, 
             fill = POTTERY, shape = SITE)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(size = 4) + 
    geom_text_repel(bg.r = .15, bg.color = "white") + 
    scale_fill_manual(values = c(pca.res.samples %>%
                                   dplyr::distinct(POTTERY) %>% 
                                   dplyr::left_join(pottery %>% dplyr::select(POTTERY, COL), by = "POTTERY") %>%
                                   dplyr::mutate(COL = replace(COL, COL == "" | is.na(COL), "#a9a9a9")) %>%
                                   dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                   dplyr::arrange(POTTERY) %>% 
                                   dplyr::pull(COL))) + 
    scale_shape_manual(values = c(24, 22, 25)) + 
    scale_x_continuous(paste0("Dim 1 (", round(pca.res$eig[1,2],2), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(pca.res$eig[2,2],2), "%)"), position = "right") + 
    ggthemes::theme_base() + 
    theme(legend.position = "none", 
          plot.background = element_blank()),
  rel_widths = c(.5, 1), 
  nrow = 1)

p <- cowplot::plot_grid(
  cowplot::plot_grid(
    p.rowA, 
    p.rowB, 
    ncol = 1, 
    align = "v", axis = "lr", labels = c("C", "D")
  ), 
  leg, 
  nrow = 1, 
  rel_widths = c(4, 1)
)
ggsave("output/xrf_pca_luilaka.jpg", p, width = 12, height = 12)
ggsave("Fig_XRF_pca_luilaka.pdf", p, width = 10, height = 7)

### 5.2.2 Clustering ----

xrf.PCA.hcpc <- FactoMineR::HCPC(
  pca.res,
  nb.clust = 0,
  graph = FALSE,
  iter.max = 100,
  min = 3,
  max = NULL)

factoextra::fviz_cluster(xrf.PCA.hcpc) + 
  ggthemes::theme_base() + 
  theme(plot.background = element_blank())
ggsave("output/xrf_pca_luilaka_cluster.jpg", width = 6, height = 6)

sink(file = "output/xrf_pca_luilaka_cluster.jpg.txt")
xrf.PCA.hcpc$desc.var
sink(file = NULL)

xrf.luilaka.PCA.hcpc.clust <- xrf.PCA.hcpc$data.clust %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(samples, by = "labels") %>% 
  dplyr::select(labels, clust)  

# 6 Varia ----

## 6.1 Samples from PIK 87/1 by depth ----

# GET nwCongo Data
library(RSQLite)
tempo <- tempfile()
utils::download.file("https://github.com/dirkseidensticker/nwCongo/raw/master/data/base/nwCongoDB.sqlite", tempo, mode = "wb", quiet = TRUE)
con <- dbConnect(SQLite(), dbname = tempo)


m.pct %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::left_join(s %>% dplyr::select(ID, CODE, FEAT, nwCongoID, POTTERY), by = "ID") %>%
  dplyr::filter(FEAT == "87/1") %>%
  dplyr::left_join(
    dbGetQuery(con, "
      SELECT 
        objID AS nwCongoID, 
        Tiefe
      FROM t_Obj"), 
      by = "nwCongoID") %>%
  reshape2::melt(id = c("ID", "CODE", "FEAT", "nwCongoID", "POTTERY", "Tiefe")) %>%
  dplyr::mutate(Tiefe = as.numeric(Tiefe)) %>%
  ggplot(aes(x = Tiefe, y = value)) + 
    geom_smooth(method = "lm") +
    geom_point(aes(color = POTTERY)) + 
    facet_wrap(variable ~ ., scales = "free_y") + 
    theme_base() + 
    theme(plot.background = element_blank())
ggsave("output/xrf_PIK87_1.jpg", width = 12, height = 10)

# 6.2 Comparision element differences PKM sherds PIK vs MUN ----

m.pct.pik.mun <- m.pct %>%
  tibble::rownames_to_column("ID") %>%
  dplyr::mutate(ID = as.numeric(ID)) %>%
  dplyr::left_join(s %>% dplyr::select(ID, SITE, POTTERY), by = "ID") %>% 
  dplyr::filter(SITE %in% c("Pikunda", "Munda")) %>%
  dplyr::mutate(POTTERY = replace(POTTERY, POTTERY == "indet", "indet/modern")) %>%
  dplyr::mutate(POTTERY = replace(POTTERY, POTTERY == "modern", "indet/modern")) %>%
dplyr::mutate(label = paste0(SITE, ": ", POTTERY)) %>%
  #dplyr::select(-c(SITE, POTTERY)) %>%
  tibble::column_to_rownames("ID") %>%
  reshape2::melt(id.vars = c("label", "SITE", "POTTERY")) %>%
  dplyr::left_join(
    pottery %>% 
      dplyr::select(POTTERY, COL), 
    by = "POTTERY"
  ) %>%
  dplyr::mutate(COL = replace(COL, COL == "", "#a9a9a9")) %>%
  dplyr::mutate(COL = replace(COL, is.na(COL), "#a9a9a9"))

m.pct.pik.mun.col <- m.pct.pik.mun %>% 
  dplyr::distinct(POTTERY, COL) %>% 
  dplyr::arrange(POTTERY)

# order of elements from most to least abundant
e.lst.order <- m.pct.pik.mun %>% 
  dplyr::group_by(variable) %>%
  dplyr::summarise(m = median(value)) %>%
  dplyr::arrange(desc(m)) %>%
  dplyr::pull(variable) %>%
  as.character()

ggplot(
  m.pct.pik.mun %>%
    dplyr::mutate(
      POTTERY = factor(POTTERY, levels = m.pct.pik.mun.col$POTTERY), 
      variable = factor(variable, levels = e.lst.order)),
  aes(
    x = label, 
    y = value)
  ) + 
  geom_vline(xintercept = 2.5, linetype = "dotted") + 
  geom_boxplot(
    aes(color = POTTERY,
        fill = POTTERY), 
    alpha = .5) + 
  geom_point(
    aes(shape = SITE,
        fill = POTTERY),
    size = 2) + 
  scale_shape_manual(values = c(24, 25)) + 
  scale_color_manual(values = m.pct.pik.mun.col$COL) + 
  scale_fill_manual(values = m.pct.pik.mun.col$COL) + 
  scale_y_continuous("%") + 
  guides(fill = guide_legend(override.aes = list(shape = 21)), 
         shape = guide_legend(override.aes = list(fill = "black"))) + 
  facet_wrap(variable ~ ., 
             scales = "free_y") + 
  theme_base() + 
  theme(plot.background = element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
ggsave("Fig_XRF_pik-mun.pdf", width = 12, height = 8)
ggsave("output/xrf_pik-mun_elements.jpg", width = 12, height = 8)


# 6.3 PCA separated by presence/absence of sponge spicules ----

petro <- data.table::fread("input/TransGenTN_Petrography_Excerpt.csv", 
                           encoding = "UTF-8", na.strings = c(NA_character_, "")) %>%
  dplyr::left_join(data.table::fread("input/TransGenTN_Samples_Excerpt.csv", encoding = "UTF-8") %>%
                     dplyr::filter(!is.na(tsID)) %>%
                     dplyr::select(tsID, labels), 
                   by = "tsID") %>%
  tibble::column_to_rownames("labels")

# seperate origin of clays by presence/absence of sponge spicules:
petro.clay.orig <- rbind(
  petro %>%
    dplyr::filter(VAR18.1.Sponge.spicules != "") %>%
    dplyr::select(tsID) %>%
    dplyr::mutate(`Sponge spicules` = "present"),
  petro %>%
    dplyr::filter(VAR18.1.Sponge.spicules == "") %>%
    dplyr::select(tsID) %>%
    dplyr::mutate(`Sponge spicules` = "absent")) %>%
  tibble::rownames_to_column("labels")

pca.res <- m.pct.cb %>%
  FactoMineR::PCA(graph = F)

pca.res.samples <- pca.res[["ind"]][["coord"]] %>% 
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(petro.clay.orig, by = "labels")

cowplot::plot_grid(
  FactoMineR::plot.PCA(pca.res, choix = "var") + 
    theme_void() + 
    theme(plot.title = element_blank(), 
          plot.background = element_rect(color = NA), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank()),
  ggplot(pca.res.samples, 
         aes(x = Dim.1, y = Dim.2, label = labels)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_point(color = "grey", shape = 16) + 
    geom_point(data = pca.res.samples %>%
                 dplyr::filter(!is.na(tsID)) %>%
                 dplyr::mutate(
                   `Sponge spicules` = factor(
                     `Sponge spicules`,
                     levels =  unique(.$`Sponge spicules`))), 
               aes(x = Dim.1, y = Dim.2, fill = `Sponge spicules`), shape = 21, size = 4) + 
    scale_shape_manual(values = c(24, 22, 25)) + 
    guides(fill = guide_legend(override.aes=list(shape = 22))) + 
    scale_x_continuous(paste0("Dim 1 (", round(pca.res$eig[1,2],2), "%)"), position = "top") + 
    scale_y_continuous(paste0("Dim 2 (", round(pca.res$eig[2,2],2), "%)"),
                       breaks = seq(-6, 6, 2), position = "right") + 
    ggthemes::theme_base() + 
    theme(legend.position = "bottom", 
          #legend.title = element_blank(),
          plot.background = element_blank()),
  rel_widths = c(.5, 1), 
  nrow = 1)
ggsave("output/xrf_clay_origin.jpg", width = 8, height = 6)
ggsave("Fig_XRF_pca_clay_origin.pdf", width = 8, height = 6)


# 7 values for text ----

# samples in general survey
s.all %>%
  dplyr::filter(
    ID %in% c(m.id %>% 
                dplyr::distinct(ID) %>% 
                pull(ID)))

# samples in study:
nrow(xrf.wCB.PCA.hcpc.clust) + nrow(xrf.luilaka.PCA.hcpc.clust)

# samples in case study I:
nrow(xrf.wCB.PCA.hcpc.clust)

# samples in case study II:
nrow(xrf.luilaka.PCA.hcpc.clust)

