# PIK 87/1

source("scripts/header.R")

# GET nwCongo Data
library(RSQLite)
tempo <- tempfile()
utils::download.file("https://github.com/dirkseidensticker/nwCongo/raw/master/data/base/nwCongoDB.sqlite", tempo, mode = "wb", quiet = TRUE)
con <- dbConnect(SQLite(), dbname = tempo)

samples %>%
  dplyr::filter(SITE == "Pikunda" & !is.na(tsID)) %>%
  dplyr::arrange(POTTERY, tsID) %>%
  dplyr::select(tsID, FEAT, IND, POTTERY)

pik81.1.po.wgt <- dbGetQuery(con, 
"SELECT
  t_Obj.objID,
  t_Ort.ort_name,
  t_Komplex.bef_nr,
  t_Obj.Individuum,
  t_Obj.Anzahl,
  t_Obj.Gewicht,
  t_Obj.Fabric,
  t_Obj.Typ,
  t_Obj.Tiefe
FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
  INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
WHERE t_Obj.komplexID = 173") %>%
  dplyr::filter(Tiefe != "") %>%
  dplyr::mutate(Tiefe = as.numeric(Tiefe)) %>%
  dplyr::filter(!is.na(Typ)) %>%
  dplyr::mutate(Typ = substr(Typ, 0, 3)) %>%
  dplyr::left_join(data.frame(
    Typ = c("PKM", "MDB", "LUS", "MAT", "MBJ", "NGB", "EBA", "PDM", "KON"),
    POTTERY = c("Pikunda-Munda", "Mandombe", "Lusako", "Matoto", "Mbenja", "Ngbanja", "Ebambe", "Pandama", "Konda")), 
    by = "Typ") %>% 
  reshape2::dcast(Tiefe ~ POTTERY, value.var = "Gewicht", fun.aggregate = sum) %>%
  reshape2::melt(id.vars = c("Tiefe")) %>%
  dplyr::filter(value > 0)

pik81.1.po.wgt %>%
  dplyr::left_join(data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", encoding = "UTF-8") %>% 
                     dplyr::select(POTTERY, COL) %>% dplyr::rename(variable = POTTERY), by = "variable") %>% 
  dplyr::mutate(variable = factor(
    variable, 
    levels = data.table::fread("https://raw.githubusercontent.com/dirkseidensticker/aSCAC/master/potterygroups.csv", encoding = "UTF-8") %>% 
      dplyr::select(-DESCRIPTION) %>% dplyr::mutate(AGE = ((FROM + TO) / 2)) %>%
      dplyr::filter(POTTERY %in% c(pik81.1.po.wgt %>% dplyr::distinct(variable) %>% dplyr::pull())) %>%
      dplyr::arrange(AGE) %>%
      dplyr::pull(POTTERY))) %>%
  dplyr::mutate(value = value / 1000) %>% 
  ggplot(aes(x = Tiefe, y = value, fill = COL)) + 
  geom_bar(stat = "identity", color = "black", width = 15) + 
  scale_fill_identity() + 
  scale_x_reverse("Depth [cm]") + 
  scale_y_continuous("Weight [kg]") + 
  #scale_y_continuous("Weight [kg]", limits = c(0, 5.3), expand = c(0, 0)) + 
  coord_flip() + 
  facet_grid(. ~ variable) + 
  theme_few()
ggsave("Fig_PIK87-1_Po_Wgt.pdf", width = 11, height = 4)
