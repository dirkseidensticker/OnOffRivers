# varia

library(tidyverse)

# GET nwCongo Data
library(RSQLite)
tempo <- tempfile()
utils::download.file("https://github.com/dirkseidensticker/nwCongo/raw/master/data/base/nwCongoDB.sqlite", tempo, mode = "wb", quiet = TRUE)
con <- dbConnect(SQLite(), dbname = tempo)


# MUN 87/2-1-1 Gefäßeinheiten

#mun211 <- 
dbGetQuery(con, 
"SELECT
  t_Obj.objID,
  t_Ort.ort_name,
  t_Komplex.bef_nr,
  t_Obj.Individuum,
  t_Obj.GefEinheit,
  t_Obj.Anzahl,
  t_Obj.Gewicht,
  t_Obj.Gr_Clist,
  t_Obj.Fabric,
  t_Obj.Typ,
  t_Obj.Form_Gef,
  t_Obj.Tiefe
FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
  INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
WHERE t_Obj.komplexID = 154") %>% 
  dplyr::mutate(Tiefe = as.numeric(Tiefe), 
                GefEinheit = as.numeric(GefEinheit)) %>%
  dplyr::arrange(Tiefe) %>%
  dplyr::filter(!is.na(GefEinheit)) %>%
  View()

# MUN 87/2-1-3 Gefäßeinheiten

mun213 <- dbGetQuery(con, 
"SELECT
  t_Obj.objID,
  t_Ort.ort_name,
  t_Komplex.bef_nr,
  t_Obj.Individuum,
  t_Obj.GefEinheit,
  t_Obj.Anzahl,
  t_Obj.Gewicht,
  t_Obj.Gr_Clist,
  t_Obj.Fabric,
  t_Obj.Typ,
  t_Obj.Form_Gef,
  t_Obj.Tiefe
FROM (t_Ort INNER JOIN t_Komplex ON t_Ort.ortID = t_Komplex.ortID)
  INNER JOIN t_Obj ON t_Komplex.komplexID = t_Obj.komplexID
WHERE t_Obj.komplexID = 238") %>%
  dplyr::mutate(Tiefe = as.numeric(Tiefe), 
                GefEinheit = as.numeric(GefEinheit)) %>%
  dplyr::arrange(Tiefe) %>%
  dplyr::filter(!is.na(GefEinheit))

View(mun213)

mun213 %>%
  dplyr::group_by(Tiefe) %>%
  dplyr::summarise(n = sum(Anzahl)) %>%
  ggplot(aes(x = Tiefe, y = n)) +
    geom_bar(stat = "identity") + 
    scale_x_reverse(breaks = seq(0, 200, 10)) + 
    coord_flip()
