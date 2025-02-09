# SNP pottery analysis

snp <- data.table::fread("input/snp_pottery.csv", encoding = "UTF-8")

snp$Sherds[is.na(snp$Sherds)] <- 1

# Anzahl Scherben
sum(snp$Sherds)

# Gesamtgewicht
sum(snp$Wght)

# Art
snp %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(n = sum(Sherds))

# Art nach Fundstellen und Tiefen
snp %>%
  reshape2::dcast(Site + Depth ~ Type, value.var = "Sherds", fun.aggregate = sum)


ggplot(data = snp, aes(x = Wall)) + 
  geom_histogram(binwidth = 1, color = "white") + 
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()
