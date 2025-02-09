# petrofarbics

source("scripts/header.R")

# 1 Setup ----

petro <- data.table::fread("input/TransGenTN_Petrography_Excerpt.csv", 
                           encoding = "UTF-8", na.strings = c(NA_character_, "")) %>%
  dplyr::left_join(data.table::fread("input/TransGenTN_Samples_Excerpt.csv", encoding = "UTF-8") %>%
                     dplyr::filter(!is.na(tsID)) %>%
                     dplyr::select(tsID, labels), 
                   by = "tsID") %>%
  tibble::column_to_rownames("labels")


# Modal analysis ----

library(ggtern)

# since ggtern is broken in v3.5.0, fall back to v.3.4.4
#devtools::install_version("ggplot2", version = "3.4.4", repos = "http://cran.us.r-project.org")
#library(ggplot2)

ggtern::ggtern(
  data = petro %>% 
    dplyr::left_join(samples %>% dplyr::mutate(tsID = as.numeric(tsID)), by = "tsID") %>%
    dplyr::mutate(M = jitter(M, 3), # jitter dots to avoid over-plotting
                  I = jitter(I, 3),
                  V = jitter(V, 3)), 
  aes(x = V, y = M, z = I, color = SITE, shape = POTTERY)) + 
  geom_point() + 
  #scale_fill_brewer(palette = "Set1") + 
  #scale_shape_manual(values = seq(21,25,1)) + 
  scale_shape_manual(values = seq(1,length(unique(samples$POTTERY)),1)) +
  #guides(fill = guide_legend(override.aes=list(shape=21))) + 
  #labs(x = "Voids", y="Matrix", z="Inclusions") + 
  Tlab("") + Llab("") + Rlab("") + 
  Tarrowlab("Clay Matrix (%)") + Larrowlab("Voids (%)") + Rarrowlab("Inclusions (%)") + 
  theme_showarrows() + 
  theme(panel.background = element_blank(), 
        #panel.grid = element_line(colour = "grey"),
        panel.border = element_rect(colour = "black"), 
        legend.key = element_rect(fill = "white"))
ggsave("output/petro_modalAnalysis.jpg", width = 8, height = 5)

petro <- petro %>% 
  dplyr::select(-c(M, I, V, NOTES, SHERD, tsID))
  
petro[petro == ''] <- NA

# 2 MCA ----

## 2.1 western Congo Basin ----

filt1 <- samples %>%
  dplyr::filter(!is.na(tsID) & SITE %in% c("Pikunda", "Munda")) %>% dplyr::pull(labels)

### 2.1.1 iteratively removing all rows and columns having at least two entries ----

petro.filt1 <- petro %>%
  dplyr::filter(rownames(.) %in% filt1) %>%
  replace(., !is.na(.), "1") %>%
  replace(., is.na(.), "0") %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% # reduce the entries to numerical
  quantAAR::itremove(minnumber = 2)

# selected samples:
rownames(petro.filt1)

# selected features:
names(petro.filt1)

petro %>%
  dplyr::filter(rownames(.) %in% filt1) %>% 
  tibble::rownames_to_column("labels") %>%
  reshape2::melt(id.vars = "labels") %>%
  dplyr::distinct(variable, value) %>%
  dplyr::filter(!is.na(value))

### 2.1.2 MCA ----

petro.MCA.res <- petro %>%
  dplyr::filter(rownames(.) %in% filt1) %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::filter(labels %in% rownames(petro.filt1)) %>%
  tibble::column_to_rownames("labels") %>%
  dplyr::select(names(petro.filt1)) %>%
  FactoMineR::MCA(graph = F)

factoextra::fviz_screeplot(petro.MCA.res)

# coords für plot

# features:
petro.MCA.res.var <- petro.MCA.res$var$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var") %>%
  dplyr::filter(!grepl(".NA", var))

# samples:
petro.MCA.res.df <- petro.MCA.res$ind$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(samples, by = "labels")

### 2.1.3 clustering ----

petro.MCA.hcpc <- FactoMineR::HCPC(
  petro.MCA.res,
  nb.clust = 0,
  graph = FALSE,
  iter.max = 100,
  min = 3,
  max = NULL)

petro.MCA.hcpc$desc.ind

sink(file = "output/petrofarbics_wCB.jpg.txt")
petro.MCA.hcpc$desc.var
sink(file = NULL)

petro.MCA.clust <- petro.MCA.hcpc$data.clust %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::select(labels, clust) %>%
  dplyr::left_join(petro.MCA.res.df)

petro.MCA.clust.ch12 <- petro.MCA.clust %>%
  dplyr::group_by(clust) %>%
  dplyr::slice(chull(x = `Dim 1`, y = `Dim 2`))


### 2.1.4 plot ----

p <- cowplot::plot_grid(
  petro.MCA.res.df %>% 
    ggplot(aes(x = `Dim 1`, y = `Dim 2`, 
               label = labels, color = POTTERY)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_polygon(data = petro.MCA.clust.ch12, aes(group = clust), fill = "lightgrey", color = "grey", alpha = 0.3) + 
    geom_text() + 
    geom_text(data = petro.MCA.clust.ch12 %>% 
                dplyr::group_by(clust) %>% 
                dplyr::summarize(`Dim 1` = mean(`Dim 1`), 
                                 `Dim 2` = mean(`Dim 2`)) %>%
                dplyr::mutate(POTTERY = ""), 
              aes(x = `Dim 1`, y = `Dim 2`, label = clust), color = "black"
    ) + 
    scale_x_continuous(paste0("Dim 1 (", round(petro.MCA.res$eig[1,2], 1), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(petro.MCA.res$eig[2,2], 1), "%)")) + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(),
          aspect.ratio = 1, 
          legend.position = c(0, 0),
          legend.justification = c("left", "bottom"),
          legend.background = element_rect(fill = alpha("white", 0.5)),
          #legend.box = "horizontal", 
          legend.title = element_blank()),
  ggplot(petro.MCA.res.var, aes(x = `Dim 1`, y = `Dim 2`, label = var)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    #geom_point() + 
    geom_text_repel() + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(), 
          aspect.ratio = 1,
          legend.title = element_blank())
)
ggsave("Fig_Petrofarbics_wCB.pdf", p, width = 16, height = 8)
ggsave("output/petrofarbics_wCB.jpg", p, width = 14, height = 7)

p <- cowplot::plot_grid(
  petro.MCA.res.df %>% 
    ggplot(aes(x = `Dim 1`, y = `Dim 2`, 
               label = labels, color = POTTERY)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_text() + 
    scale_x_continuous(paste0("Dim 1 (", round(petro.MCA.res$eig[1,2], 1), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(petro.MCA.res$eig[2,2], 1), "%)")) + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(),
          aspect.ratio = 1, 
          legend.position = c(.1, .1), 
          legend.box = "horizontal", 
          legend.title = element_blank()),
  ggplot(petro.MCA.res.var, aes(x = `Dim 1`, y = `Dim 2`, label = var)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    #geom_point() + 
    geom_text_repel() + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(), 
          aspect.ratio = 1,
          legend.title = element_blank())
)
ggsave("output/petrofarbics_wCB_wocluster.jpg", p, width = 16, height = 8)

petro.wBC.clust.export <- petro.MCA.clust %>%
  dplyr::select(clust, labels, tsID, SITE, POTTERY) %>%
  dplyr::arrange(clust, tsID)
xlsx::write.xlsx2(petro.wBC.clust.export, "output/petro_wCB_clusters.xlsx")

## 2.2 Luilaka ----

filt2 <- samples %>%
  dplyr::filter(!is.na(tsID) & SITE %in% c("Wafanya", "Monkoto", "Salonga")) %>% dplyr::pull(labels)

### 2.2.1 iteratively removing all rows and columns having at least two entries ----

petro.filt2 <- petro %>%
  dplyr::filter(rownames(.) %in% filt2) %>%
  replace(., !is.na(.), "1") %>%
  replace(., is.na(.), "0") %>% 
  dplyr::mutate_if(is.character, as.numeric) %>% # reduce the entries to numerical
  quantAAR::itremove(minnumber = 2)

# selected samples:
rownames(petro.filt2)

# selected features:
names(petro.filt2)

petro %>%
  dplyr::filter(rownames(.) %in% filt2) %>% 
  tibble::rownames_to_column("labels") %>%
  reshape2::melt(id.vars = "labels") %>%
  dplyr::distinct(variable, value) %>%
  dplyr::filter(!is.na(value))

### 2.2.2 MCA ----

petro.MCA.res <- petro %>%
  dplyr::filter(rownames(.) %in% filt2) %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::filter(labels %in% rownames(petro.filt2)) %>%
  tibble::column_to_rownames("labels") %>%
  dplyr::select(names(petro.filt2)) %>%
  FactoMineR::MCA(graph = F)

factoextra::fviz_screeplot(petro.MCA.res)

# coords für plot

# features:
petro.MCA.res.var <- petro.MCA.res$var$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("var") %>%
  dplyr::filter(!grepl(".NA", var))

# samples:
petro.MCA.res.df <- petro.MCA.res$ind$coord %>%
  as.data.frame() %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::left_join(samples, by = "labels")

### 2.2.3 clustering ----

petro.MCA.hcpc <- FactoMineR::HCPC(
  petro.MCA.res,
  nb.clust = 0,
  graph = FALSE,
  iter.max = 100,
  min = 3,
  max = NULL)

petro.MCA.hcpc$desc.ind

sink(file = "output/petrofarbics_luilaka.jpg.txt")
petro.MCA.hcpc$desc.var
sink(file = NULL)

petro.MCA.clust <- petro.MCA.hcpc$data.clust %>%
  tibble::rownames_to_column("labels") %>%
  dplyr::select(labels, clust) %>%
  dplyr::left_join(petro.MCA.res.df)

petro.MCA.clust.ch12 <- petro.MCA.clust %>%
  dplyr::group_by(clust) %>%
  dplyr::slice(chull(x = `Dim 1`, y = `Dim 2`))


### 2.2.4 plot ----

p <- cowplot::plot_grid(
  petro.MCA.res.df %>% 
    ggplot(aes(x = `Dim 1`, y = `Dim 2`, 
               label = labels, color = POTTERY)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_polygon(data = petro.MCA.clust.ch12, aes(group = clust), fill = "lightgrey", color = "grey", alpha = 0.3) + 
    geom_text() + 
    geom_text(data = petro.MCA.clust.ch12 %>% 
                dplyr::group_by(clust) %>% 
                dplyr::summarize(`Dim 1` = mean(`Dim 1`), 
                                 `Dim 2` = mean(`Dim 2`)) %>%
                dplyr::mutate(POTTERY = ""), 
              aes(x = `Dim 1`, y = `Dim 2`, label = clust), color = "black"
    ) + 
    scale_x_continuous(paste0("Dim 1 (", round(petro.MCA.res$eig[1,2], 1), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(petro.MCA.res$eig[2,2], 1), "%)")) + 
    guides(color = guide_legend(override.aes=list(shape = 22))) + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(),
          aspect.ratio = 1, 
          legend.position = c(0.01, 0.01), 
          legend.justification = c("left", "bottom"),
          #legend.box = "horizontal", 
          legend.title = element_blank()),
  ggplot(petro.MCA.res.var, aes(x = `Dim 1`, y = `Dim 2`, label = var)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    #geom_point() + 
    geom_text_repel() + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(), 
          aspect.ratio = 1,
          legend.title = element_blank())
)
ggsave("Fig_Petrofarbics_Luilaka.pdf", p, width = 16, height = 8)
ggsave("output/petrofarbics_luilaka.jpg", p, width = 14, height = 7)

p <- cowplot::plot_grid(
  petro.MCA.res.df %>% 
    ggplot(aes(x = `Dim 1`, y = `Dim 2`, 
               label = labels, color = POTTERY)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    geom_text() + 
    scale_x_continuous(paste0("Dim 1 (", round(petro.MCA.res$eig[1,2], 1), "%)")) + 
    scale_y_continuous(paste0("Dim 2 (", round(petro.MCA.res$eig[2,2], 1), "%)")) + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(),
          aspect.ratio = 1, 
          legend.position = c(.1, .1), 
          legend.box = "horizontal", 
          legend.title = element_blank()),
  ggplot(petro.MCA.res.var, aes(x = `Dim 1`, y = `Dim 2`, label = var)) +
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed") + 
    #geom_point() + 
    geom_text_repel() + 
    coord_equal() + 
    theme_base() + 
    theme(plot.background = element_blank(), 
          aspect.ratio = 1,
          legend.title = element_blank())
)
ggsave("output/petrofarbics_luilaka_wocluster.jpg", p, width = 16, height = 8)


petro.luilaka.clust.export <- petro.MCA.clust %>%
  dplyr::select(clust, labels, tsID, SITE, POTTERY) %>%
  dplyr::arrange(clust, tsID)
xlsx::write.xlsx2(petro.luilaka.clust.export, "output/petro_luilaka_clusters.xlsx")

