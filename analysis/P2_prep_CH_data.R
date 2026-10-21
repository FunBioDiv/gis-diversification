# Prepare the nutzung data received from Selma to simplify processing
#
# input:
#     received on 18/02/2026:
#   data/raw-data/CH/buffer_4000_lnf_hk_area_distance_nutz_60.gpkg
#     received on 20/05/2026:
#   data/raw-data/CH/buffer_3000_lnf_hk_area_distance_nutz_24to25_true.gpkg
#   data/raw-data/CH/buffer_3000_snh_2024.gpkg
#     received on 01/06/2026
#   data/raw-data/CH/buffer_4000_lnf_hk_2024.gpkg
#   data/raw-data/CH/buffer_3000_snh_2024.gpkg
#
# output:
#   data/raw-data/nuzung/nutz_YYYY.gpkg
#   data/raw-data/nuzung/hedges_2023.gpkg
#

library(terra)
library(here)

dirfile <- here("data", "raw-data", "CH")
outfile <- here("data", "raw-data", "nutzung")

# get crop reference
ref <- read.csv(here("data", "derived-data", "classes_rpg_nutzung_clc.csv"))

colNUTZ <- c("objectid", "nutzung_fr")

## 1. Split nutzung for 2019 - 2023 --------------------------------------------

file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")
# super heavy (2.4Gb)
rpg_ch <- vect(file)

# split the file per year (400Mb)
for (i in 2019:2023) {
  chi <- rpg_ch[rpg_ch$year == i, ]
  # could also select the relevant column to make the data smaller
  writeVector(chi, file.path(outfile, paste0("nutz_", i, ".gpkg")))
}


## 2. Clean 2025 nutzung -------------------------------------------------------

rpg25 <- vect(file.path(
  dirfile,
  "buffer_3000_lnf_hk_area_distance_nutz_24to25_true.gpkg"
))

rpg25$nutzungsidentifikator <- paste(
  rpg25$kanton,
  rpg25$bezugsjahr,
  rpg25$bezugsjahr,
  rpg25$qgis_id,
  sep = "_"
)

# table(rpg25$year, useNA = "ifany") # 401436
# table(rpg25$Hauptkategorie_FR)
# table(duplicated(data.frame(rpg25)))
#remove duplicates
rpg25 <- rpg25[!duplicated(data.frame(rpg25)), ]
rpg25$nutzung_fr <- gsub("Riz en culture seche", "Riz", rpg25$nutzung_fr)
# get numeric id
m_rpg <- match(rpg25$nutzung_fr, ref$name)
# table(is.na(m_rpg))
writeVector(rpg25, file.path(outfile, "nutz_2025.gpkg"), overwrite = TRUE)


## 3. Clean 2024 nutzung -------------------------------------------------------

rpg24 <- vect(file.path(
  dirfile,
  "buffer_4000_lnf_hk_2024.gpkg"
))
# table(rpg24$year, useNA = "ifany") # 401436
# Encoding(rpg24$nutzung_fr)
rpg24$nutzung_fr <- iconv(
  rpg24$nutzung_fr,
  from = "UTF-8",
  to = "ASCII//TRANSLIT"
)
rpg24$nutzungsidentifikator <- rpg24$id_kanton_agis

# simplify classes
rpg24$nutzung_fr <- gsub("surface agricole utile", "SAU", rpg24$nutzung_fr)
rpg24$nutzung_fr <- gsub("surface d'estivage", "SE", rpg24$nutzung_fr)
rpg24$nutzung_fr <- gsub(
  "Surfaces dont l'affectation principale n'est pas l'exploitation agricole (terrains a batir equipes et surfaces comprises dans les terrains de golf et de camping, aerodromes et terrains d'entrainement ",
  "Surfaces dont l'affectation principale n'est pas l'exploitation agricole (terrains a batir equipes et surfaces comprises dans les terrains de golf et de camping, aerodromes et terrains d'entrainement militaire, surfaces delimitees des bas-cotes des lignes de chemins de fer, de routes publiques, de cours et de plans d'eau)",
  rpg24$nutzung_fr,
  fixed = TRUE
)

# get numeric id
m_rpg <- match(rpg24$nutzung_fr, ref$name)
# table(is.na(m_rpg))
# unique(rpg24$nutzung_fr[is.na(m_rpg)])

writeVector(
  rpg24,
  file.path(outfile, "nutz_2024.gpkg"),
  overwrite = TRUE
)

## 4. Check hedges -------------------------------------------------------------
# semi naturel elements in 2023
snh23 <- vect(file.path(
  dirfile,
  "buffer_4000_snh_2023_60.gpkg"
))

# table(snh23$type_source) # semi_nat : 943975
# table(snh23$year) # 2023
# table(snh23$element_typ) # not sure what does that mean
# unique(snh23$typ_flaechen) # more interesting
h23 <- snh23[grep("Hecken", snh23$typ_flaechen), ]

# semi naturel elements in 2024
snh24 <- vect(file.path(
  dirfile,
  "buffer_3000_snh_2024.gpkg"
))
# table(snh24$type_source) # semi_nat : 530788
# table(snh24$year) # 2024
# table(snh24$typ_flaechen) # more interesting
h24 <- snh24[grep("Hecken", snh24$typ_flaechen), ]

# plot(h23, col = "red")
# plot(h24, col = "blue", add=TRUE)
# d34 <- symdif(h23, h24)

## => complete the hedgerow dataset for 2023
rpg23 <- vect(file.path(dirfile, "nutz_2023.gpkg"))
h1 <- rpg23[grep("Haies", rpg23$nutzung_fr), ]
# table(h1$nutzung_fr)
# simplify
h1 <- aggregate(h1)

snh23 <- vect(
  file.path(dirfile, "buffer_4000_snh_2023_60.gpkg")
)
h2 <- snh23[grep("Hecken", snh23$typ_flaechen), ]
table(h2$typ_flaechen)
# simplify
h2 <- aggregate(h2)

hedge <- rbind(h1, h2)
# simplify
hedge <- aggregate(hedge)
# save
writeVector(hedge, file.path(dirfile, paste0("Hedges_2023.gpkg")))
