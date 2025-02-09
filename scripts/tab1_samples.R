# table of samples:

source("scripts/header.R")

t <- samples %>%
  dplyr::select(ID, tsID, labels, SITE, CODE, FEAT, IND, POTTERY, TYPE) %>%
  dplyr::mutate(OBJ = paste0(CODE, " ", FEAT, ":", IND)) %>%
  dplyr::select(-c(CODE, IND)) %>%
  dplyr::left_join(samples %>%
                     dplyr::filter(!is.na(tsID)) %>%
                     dplyr::select(ID) %>%
                     dplyr::mutate(P =  "X"), 
                   by = "ID") %>% 
  dplyr::left_join(data.table::fread("input/meas.csv", encoding = "UTF-8") %>% # measurments in long format
                     dplyr::left_join(xlsx::read.xlsx2("input/meas-ID.xlsx", sheetIndex = 1) %>%
                                        dplyr::mutate(MEAS = as.numeric(MEAS), 
                                                      ID = as.numeric(ID)), by = "MEAS") %>%
                     dplyr::filter(ID %in% c(samples %>% 
                                               dplyr::pull(ID))) %>%
                     dplyr::distinct(ID) %>%
                     dplyr::left_join(samples %>%
                                        dplyr::select(ID),
                                      by = "ID") %>%
                     dplyr::mutate(C = "X"),
                   by = "ID") %>% 
  dplyr::filter(P == "X" | C == "X") %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ replace(.x, is.na(.x), ""))) %>%
  dplyr::mutate(SITE = factor(SITE, 
                              levels = c("Pikunda", "Munda", "Wafanya", "Salonga", "Monkoto")),
                TYPE = dplyr::recode(TYPE, 
                                     'W' = 'Wall', 
                                     'R' = 'Rim', 
                                     'B' = 'Base', 
                                     'G' = 'Vessel')) %>%
  dplyr::rename(Label = labels, 
                Site = SITE, 
                Sample = OBJ,
                Style = POTTERY, 
                Sherd = TYPE) %>%
  dplyr::mutate(ID = as.numeric(ID), 
                tsID = as.numeric(tsID)) %>%
  dplyr::arrange(Site, FEAT, tsID, ID) %>%
  dplyr::select(Label, Site, Sample, Style, Sherd, P, C)

t

xlsx::write.xlsx2(t, "output/Tab_Samples_L.xlsx", row.names = F)

# cumulative table:

t.w <- t %>%
  dplyr::mutate(
    Style = factor(
      Style, 
      levels = c(
        "Pikunda-Munda", 
        "Ngbanja", 
        "Lusako",
        "Monkoto",
        "Bokuma",
        "Bekongo",
        "Longa",
        "Mandombe",
        "Ebambe", 
        "modern",
        "indet"
        ))) %>%
  reshape2::dcast(Style ~ Site)

t.w[t.w == 0] <- ""
t.w
xlsx::write.xlsx2(t.w, "output/Tab_Samples_W.xlsx", row.names = F)

t.w %>%
  tibble::column_to_rownames("Style") %>%
  dplyr::mutate_all(function(x) as.numeric(as.character(x))) %>%
  rowSums(na.rm = T)

t.w %>%
  tibble::column_to_rownames("Style") %>%
  dplyr::mutate_all(function(x) as.numeric(as.character(x))) %>%
  colSums(na.rm = T)
