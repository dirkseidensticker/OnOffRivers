library(ggplot2)
library(tidyr)
library(sf)

source("scripts/myfct.R")

pottery <- read.csv(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv",
  encoding = "UTF-8")
# styleschrono$POSc <- as.character(styleschrono$POS)
pottery$FROM <- as.numeric(pottery$FROM)
pottery$TO <- as.numeric(pottery$TO)

regions <- geojsonsf::geojson_sf("input/gis/regions.geojson")

sites <- read.csv(
  "https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/sites.csv",
  encoding = "UTF-8") %>%
  st_as_sf(crs = 4326, 
           coords = c("LONG", 
                      "LAT"), 
           remove = FALSE, 
           na.fail = F)

# Chronology ----
c14 <- rbind(
  data.table::fread(
    "https://raw.githubusercontent.com/dirkseidensticker/aDRAC/master/aDRAC.csv", 
    encoding = "UTF-8"),
  data.table::fread(
    "input/aDRAC_new.csv", 
    encoding = "UTF-8")
) %>%
  dplyr::mutate(C14AGE = as.numeric(C14AGE), 
                C14STD = as.numeric(C14STD), 
                LAT = as.numeric(LAT), 
                LONG = as.numeric(LONG))

# rcarbon ####
library(rcarbon)
library(parallel)
ncores = (detectCores() - 1)

bayes <- data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/PikundaMunda_BatalimoMaluba_AAR/main/tbl/tbl_bayesphases_comparison.csv", encoding = "UTF-8") %>%
  dplyr::filter(`Pottery Group` != "Ilambi") %>%
  dplyr::select(1, 3, 5) %>%
  dplyr::rename("POTTERY" = "Pottery Group", 
                "FROM" = "Bayesian Start", 
                "TO" = "Bayesian End")


chrono.plt <- function(SEL){
  pottery.sel <- rbind(
    bayes %>% 
      dplyr::left_join(pottery %>% dplyr::select(POTTERY, REGION, COL), by = "POTTERY"),
    pottery %>%
      dplyr::filter(!POTTERY %in% c(bayes %>% dplyr::pull(POTTERY))) %>%
      dplyr::select(POTTERY,FROM,TO,REGION, COL)) %>%
    dplyr::filter(POTTERY %in% SEL)
  
  # manually alter age range for Monkoto style back as Bayes run (cf. https://github.com/dirkseidensticker/aSCAC/blob/master/scripts/BayesPhases.R) produced unreasonably long age range for this style; so back to values from Wotzka 1995 (cf. https://github.com/dirkseidensticker/aSCAC/blob/master/potterygroups.csv)
  
  pottery.sel[pottery.sel$POTTERY == "Monkoto",]$FROM <- -100
  pottery.sel[pottery.sel$POTTERY == "Monkoto",]$TO <- 200
  
  # LOOP ----
  # All dates/styles ----
  datalist <- list()
  filterlist <- list()
  a.sel.list <- list()
  cnt <- 1
  
  a <- c14 %>% 
    sf::st_as_sf(coords = c("LONG", "LAT"), # convert to sf
                 crs = 4326, 
                 remove = F, 
                 na.fail = F) %>% 
    #sf::st_filter(regions %>% # filter only those in region E (Sangha/Likwala-aux-Herbes)
    #                dplyr::filter(id == "E") %>% 
    #                sf::st_union()) %>%
    dplyr::filter(C14AGE > 71 & 
                  C14AGE < 6000 & 
                  (POTTERY != '' &  POTTERY != 'indet' &  POTTERY != '(indet)' & POTTERY != '-') & 
                  CLASS %in% c("Ia", "Ib", "Ic"))
  
  #a <- dplyr::filter(c14, 
  #                   C14AGE > 71 & 
  #                   C14AGE < 6000 &
  #                   (POTTERY != '' &  POTTERY != 'indet' &  POTTERY != '(indet)' & POTTERY != '-') & 
  #                   CLASS %in% c("Ia", "Ib", "Ic", "IIc")
  #)
  
  
  styles <- pottery.sel %>% dplyr::pull(POTTERY) # styles in region
  
  for(j in 1:length(styles)){
    print(paste("[", j, "/", length(styles), "] -", styles[j]))
    
    # > FILTER DATES ---- 
    d <- dplyr::filter(a, grepl(styles[j], a$POTTERY)) # filter for dates related to style
    d <- dplyr::filter(d, !grepl(paste0("\\(" , styles[j], "\\)"), d$POTTERY))# remove cases in parantheses
    
    if (nrow(d) != 0) {
      
      res <- rcarbonsum(d, oxcalnorm = TRUE)
      
      res[[1]]$median <- list(res[[2]])
      res[[1]]$start <- res[[3]]
      res[[1]]$style <- styles[j]
      res[[1]]$label <- paste0(res[[1]]$style, " (", nrow(d), ")")
      #res[[1]]$region <- id.lst[i]
      
      dat <- res[[1]]
      
      datalist[[cnt]] <- dat
      a.sel.list[[cnt]] <- a
      
      cnt <- cnt + 1
    }
  } # end of loop through styles
  
  styleprob <- do.call(rbind, datalist)
  #sites <- do.call(rbind, a.sel.list)
  
  styleprob.med <- unique(styleprob[, c("style", "median")])
  styleprob.med <- tidyr::unnest(styleprob.med, median)
  
  names(styleprob.med)[names(styleprob.med) == "median"] <- "TO"
  
  styleschrono <- pottery.sel %>%
    dplyr::filter(POTTERY %in% c(pottery.sel %>% dplyr::pull(POTTERY)))
  
  ## MERGE WITH STYLECHRONO
  styleprob <- merge(
    x = styleprob, 
    by.x = "style", 
    y = styleschrono[,c("POTTERY", "FROM", "TO", "COL")], 
    by.y = "POTTERY", 
    sort = FALSE, all.x = TRUE)
  
  styleprob$rel <- TRUE
  
  # add not merged groups (what is left?)
  B <- unique(styleprob$style)
  A <- unique(styleschrono$POTTERY)
  missing <- A[which(!A %in% B)]
  
  style.m <- subset(styleschrono, POTTERY %in% missing) %>%
    dplyr::select(POTTERY, FROM, TO, REGION, COL)
  
  style.m <- style.m[,c("POTTERY", "FROM", "TO", "REGION", "COL")]
  
  names(style.m)[names(style.m) == "POTTERY"] <- "style"
  names(style.m)[names(style.m) == "REGION"] <- "region"
  
  style.m$label <- style.m$style
  
  style.m$rel <- FALSE
  
  style.m$grid.calBP <- NA
  style.m$grid.PrDens <- NA
  style.m$median <- NA
  style.m$start <- NA
  
  style.m <- style.m[,c("style", "grid.calBP", "grid.PrDens", "median", "start", "label", "FROM", "TO", "rel", "COL")]
  
  styleprob <- rbind(styleprob, style.m)
  
  style.m.lab <- style.m[,c("style", "FROM", "TO", "label", "COL")]
  
  style.m.lab$mean <- (style.m.lab$FROM + style.m$TO) / 2
  
  style.m.lab <- style.m.lab[,c("style", "mean", "label", "COL")]
  style.m.lab
  
  style.max <- styleprob %>% 
    dplyr::group_by(label) %>% 
    dplyr::slice(which.max(grid.calBP))
  names(style.max)[2] <- "max"
  
  style.min <- styleprob %>% 
    dplyr::group_by(label) %>% 
    dplyr::slice(which.min(grid.calBP))
  names(style.min)[2] <- "min"
  
  styleprob.lab <- merge(x = style.min, y = style.max[,c("style", "max")], by = "style")
  styleprob.lab$mean <- (styleprob.lab$max + styleprob.lab$min) / 2
  
  styleprob.lab <- styleprob.lab[,c("style", "mean", "label", "COL")]
  
  styleprob.lab1 <- rbind(styleprob.lab, style.m.lab)
  
  colnames(styleprob.lab1)[2] <- "TO"
  
  style.box <- unique(styleprob[c("style", "FROM", "TO", "start", "rel", "COL")])
  style.box
  
  # manually change values for Bekongo style:
  if(nrow(style.box %>% dplyr::filter(style == "Bekongo")) != 0){
    style.box$FROM[style.box$style == "Bekongo"] <- 750
    style.box$TO[style.box$style == "Bekongo"] <- 950
  }
  
  # plot chrono ----
  p.chrono <- ggplot(data = style.box, 
         aes(x = FROM, 
             y = reorder(style, FROM), 
             xend = TO, 
             yend = style)) + 
    #geom_segment(aes(linetype = rel), alpha = 0) + 
    geom_segment(aes(linetype = rel, color = COL), 
                 size = 3, alpha = 0.6) + 
    scale_linetype_manual(values = c("11", "solid")) + 
    scale_color_identity() + 
    ggnewscale::new_scale_color() + 
    geom_line(data = styleprob, 
              aes(x = grid.calBP,
                  y = style,
                  color = grid.PrDens), 
              linewidth = 1.5) + 
    geom_point(data = styleprob.med, 
               aes(x = TO, y = style), 
               color = "black", fill = "white", shape = 21, size = 1) +   
    geom_label(aes(label = style), 
               #angle = 90, 
               hjust = 1, nudge_x = -50, 
               size = 2, label.size = NA, 
               fontface = "bold", 
               label.padding = unit(0.1, "lines")) + 
    scale_colour_gradient(low = "white", 
                          high = "black") + 
    scale_x_continuous("cal BCE/CE", 
                       limits = c(-700,2000), 
                       breaks = c(seq(-1400,1800,200), 1950), 
                       expand = c(0,0)) + 
    #scale_y_discrete(limits = rev) + 
    theme_bw() + 
    theme(panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          axis.title.y = element_blank(), 
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none", 
          strip.text.y = element_text(angle = 0), 
          strip.background = element_blank())
  return(p.chrono)
}

# pottery @Pikunda
pik.plt <- chrono.plt(SEL = sites %>% dplyr::filter(SITE == "Pikunda") %>% dplyr::distinct(POTTERY) %>% dplyr::pull())

snp.plt <- chrono.plt(SEL = sites %>% dplyr::filter(SITE %in% c("Monkoto", "Wafanya")) %>% dplyr::distinct(POTTERY) %>% dplyr::pull())

cowplot::plot_grid(
  NULL,
  pik.plt,
  NULL,
  snp.plt,
  ncol = 1, rel_heights = c(.15, 1, .15, 1),
  labels = c("A", "", "B", "")
)
ggsave("Fig_PotteryChrono.pdf", width = 8, height = 4)
ggsave("output/Fig_PotteryChrono.jpg", width = 8, height = 4)

# Plot individual dates: ----

c14.cal <- c14 %>%
  dplyr::filter(SITE %in% c("Pikunda", "Munda", "Wafanya", "SNP", "Monkoto")) %>% 
  dplyr::filter(!(LABNR %in% c("Hv-12611", "Hv-12612"))) %>%
  dplyr::rename(c14age = C14AGE, c14std = C14STD) %>%
  c14bazAAR::as.c14_date_list() %>%
  c14bazAAR::calibrate(choices = "calprobdistr") %>% # calibration
  tidyr::unnest(cols = c("calprobdistr"))

c14.cal %>%
  #dplyr::filter(c14age < 4000) %>%
  dplyr::arrange(-c14age) %>% 
  dplyr::mutate_at(vars(LABNR), dplyr::funs(factor(., levels=unique(.)))) %>%
  dplyr::mutate(SITE = factor(SITE, levels = c("Pikunda", "Munda", "SNP", "Wafanya"))) %>%
  ggplot() + 
  ggridges::geom_ridgeline(
    aes(x = -calage + 1950, 
        y = LABNR, 
        height = density),
    scale = 50) + 
  facet_grid(SITE ~ ., 
             #facet_grid(SITE + FEATURE ~ ., 
             scales = "free", 
             space = "free", 
             switch = "y") + 
  scale_x_continuous("cal BCE/CE", expand = c(0, 0), 
                     breaks = c(seq(-400, 1800, 200), 1950),
                     labels = c("400", "200", seq(0, 1800, 200), "1950"),
                     limits = c(-400, 1950)) + 
  scale_y_discrete(position = "right") +
  ggthemes::theme_few() +
  theme(strip.placement = "left",
        strip.text.y.left = element_text(angle = 0), 
        axis.title.y = element_blank())
ggsave("Fig_c14.pdf", width = 8, height = 6)
