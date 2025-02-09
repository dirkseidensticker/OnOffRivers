# Synthesis

source("scripts/header.R")

# load existing files:

source("scripts/petrofabrics.R")

source("scripts/pxrf.R")

# Chrono ----

bayes <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/PikundaMunda_BatalimoMaluba_AAR/main/tbl/tbl_bayesphases_comparison.csv", encoding = "UTF-8") %>%
  dplyr::filter(`Pottery Group` != "Ilambi") %>%
  dplyr::select(1, 3, 5) %>%
  dplyr::rename("POTTERY" = "Pottery Group", 
                "FROM" = "Bayesian Start", 
                "TO" = "Bayesian End")
pottery <- rbind(
  bayes %>% 
    dplyr::left_join(pottery %>% dplyr::select(POTTERY, REGION, COL), by = "POTTERY"),
  pottery %>%
    dplyr::filter(!POTTERY %in% c(bayes %>% dplyr::pull(POTTERY))) %>%
    dplyr::select(POTTERY,FROM,TO,REGION, COL))


# fabric 1 (fluvial clays)
fab1 <- petro %>% dplyr::filter(VAR18.1.Sponge.spicules == "freq") %>% dplyr::pull(tsID)

# fabric 2 (terretrial clays)
fab2 <- petro %>% dplyr::filter(VAR18.1.Sponge.spicules == "" & grepl("PIK", SHERD)) %>% dplyr::pull(tsID)

# fabric 3a (grog & plant)
fab3a <- c(44, 51, 52)

# fabric 3b (grog)
fab3b <- c(42, 45, 46)

# fabric 3c (plant)
fab3c <- c(48, 96)

samples.fab.chrono <- rbind(
  samples %>% dplyr::filter(tsID %in% fab1 | tsID == 50) %>% dplyr::mutate(FABRIC = "1: sponge spicules rich"), 
  samples %>% dplyr::filter(tsID %in% fab2) %>% dplyr::mutate(FABRIC = "2: no sponge spicules"), 
  samples %>% dplyr::filter(tsID %in% fab3a) %>% dplyr::mutate(FABRIC = "3a: grog & organics"), 
  samples %>% dplyr::filter(tsID %in% fab3b) %>% dplyr::mutate(FABRIC = "3b: grog"), 
  samples %>% dplyr::filter(tsID %in% fab3c) %>% dplyr::mutate(FABRIC = "3c: organics")
) %>%
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::mutate(SITE = factor(SITE, 
                              levels = c("Pikunda", "Munda", "Wafanya", "Monkoto", "Salonga")), 
                AGE = (FROM + TO) / 2)

# set dates manually:
# set Lusako & Ngbanja samples from PIK 87/1 to same age as PKM style:
samples.fab.chrono[POTTERY == "Lusako","AGE"] <- 171.5
samples.fab.chrono[POTTERY == "Ngbanja","AGE"] <- 171.5
# modern sherd from PIK 87/2:
samples.fab.chrono[is.na(AGE) & CODE == "PIK","AGE"] <- 1950
# indet sherd from Monkoto assign to same age as Monkoto style, which 75% of sherds from this site:
samples.fab.chrono[is.na(AGE) & CODE == "MON","AGE"] <- 159
# indet sherds from SNP, cf. RICH-25317
samples.fab.chrono[is.na(AGE) & CODE == "SNP","AGE"] <- 830
# replace old estimate for Bekongo style with date from RICH-33240
samples.fab.chrono[POTTERY == "Bekongo","AGE"] <- 937.5
# set ages for modern pottery to 1950
samples.fab.chrono[AGE == 1995,"AGE"] <- 1950
# set indet colours to grey
samples.fab.chrono[is.na(COL) | COL == "","COL"] <- "#a9a9a9"

ggplot(samples.fab.chrono,
  aes(x = SITE, 
        y =  AGE, 
        shape = FABRIC, 
        fill = POTTERY)) + 
  ggpointgrid::geom_pointrect(scale_x = .25, 
                              scale_y = 100,
                              size = 4) + 
  scale_y_continuous("Years cal CE", limits = c(0, 2000)) + 
  scale_fill_manual(values = c(samples.fab.chrono %>% 
                                 dplyr::distinct(POTTERY, COL) %>%
                                 dplyr::mutate(POTTERY = factor(POTTERY)) %>%
                                 dplyr::arrange(POTTERY) %>%
                                 dplyr::pull(COL))) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) + 
  guides(fill = guide_legend(override.aes=list(shape = 22, size = 4))) + 
  facet_grid(. ~ SITE, scales = "free_x") + 
  theme_base() + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        plot.background = element_blank())
ggsave("Fig_Synthesis.pdf", width = 8, height = 5)
ggsave("output/synthesis.jpg", width = 8, height = 5)


# propostion sponge spicules in samples per pottery style: ----

petro %>%
  dplyr::left_join(samples, by = "tsID") %>%
  dplyr::left_join(pottery, by = "POTTERY") %>%
  dplyr::mutate(clay.source = dplyr::case_when(
    VAR18.1.Sponge.spicules != "" ~ "Fluvial",
    VAR18.1.Sponge.spicules == "" ~ "Terrestrial",
    TRUE ~ "other")) %>%
  reshape2::dcast(POTTERY ~ clay.source)

# MCA ----

# 1 western Congo Basin ----

shaping <- rbind(
  # Pikunda-Munda: drawing of a ring
  samples %>% 
    dplyr::filter(POTTERY %in% c("Pikunda-Munda")) %>%
    dplyr::mutate(shap = "dr") %>%
    dplyr::select(labels, shap),
  # Ebambe: drawing of a lump
  samples %>% 
    dplyr::filter(POTTERY %in% c("Ebambe")) %>%
    dplyr::mutate(shap = "dl") %>%
    dplyr::select(labels, shap),
  # Mandombe, Konda: coiling
  samples %>% 
    dplyr::filter(POTTERY %in% c("Mandombe", "Konda", "modern")) %>%
    dplyr::mutate(shap = "c") %>%
    dplyr::select(labels, shap))

# grouping from petrography
pfabric <- petro.wBC.clust.export %>% 
  dplyr::rename(pfabric = clust) %>%
  dplyr::select(labels, pfabric)

# grouping from pXRF
chem <- xrf.wCB.PCA.hcpc.clust %>%
  dplyr::rename(chem = clust)

comb <- samples %>%
  dplyr::select(labels, POTTERY) %>%
  dplyr::left_join(shaping, by = "labels") %>%
  dplyr::left_join(pfabric, by = "labels") %>%
  dplyr::left_join(chem, by = "labels") %>%
  dplyr::filter(!is.na(pfabric) | !is.na(chem)) %>%
  tibble::column_to_rownames("labels")

## 1.1 MCA
mca.res <-   FactoMineR::MCA(comb, graph = F, quali.sup = which(colnames(comb)=="POTTERY"))

factoextra::fviz_mca_biplot(mca.res, repel = T) + 
  coord_equal() +
  ggthemes::theme_base() + 
  theme(aspect.ratio = 1, 
        plot.background = element_blank(), 
        plot.title = element_blank())
ggsave("Fig_Synthesis_wCB.pdf", width = 6, height = 6)
ggsave("output/synthesis_wCB.jpg", width = 10, height = 10)

# 2 Luilaka ----

# grouping from petrography
pfabric <- petro.luilaka.clust.export %>% 
  dplyr::rename(pfabric = clust) %>%
  dplyr::select(labels, pfabric)

# grouping from pXRF
chem <- xrf.luilaka.PCA.hcpc.clust %>%
  dplyr::rename(chem = clust)

comb <- samples %>%
  dplyr::select(labels, POTTERY) %>%
  #dplyr::left_join(shaping, by = "labels") %>%
  dplyr::left_join(pfabric, by = "labels") %>%
  dplyr::left_join(chem, by = "labels") %>%
  dplyr::filter(!is.na(pfabric) | !is.na(chem)) %>%
  tibble::column_to_rownames("labels")

## 1.1 MCA
mca.res <-   FactoMineR::MCA(comb, graph = F, quali.sup = which(colnames(comb)=="POTTERY"))

factoextra::fviz_mca_biplot(mca.res, repel = T) + 
  #coord_equal() + 
  ggthemes::theme_base() + 
  theme(aspect.ratio = 1, 
        plot.background = element_blank(), 
        plot.title = element_blank())
ggsave("Fig_Synthesis_luilaka.pdf", width = 6, height = 6)
ggsave("output/synthesis_luilaka.jpg", width = 10, height = 10)
