library(terra)
library(here)

dirfile <- here("data", "raw-data", "CH")
file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")

# get crop reference
ref <- read.csv(here("data", "derived-data", "classes_rpg_nutzung_clc.csv"))

colNUTZ <- c("objectid", "nutzung_fr")
# super heavy (2.4Gb)
rpg_ch <- vect(file)

# split the file per year (400Mb)
for (i in 2019:2023) {
  chi <- rpg_ch[rpg_ch$year == i, ]
  # could also select the relevant column to make the data smaller
  writeVector(chi, file.path(dirfile, paste0("nutz_", i, ".gpkg")))
}


# zoomin <- ext(c(2508265, 2509047, 1119836, 1121222)) + 2000
# v <- vect(file.path(dirfile, "nutz_2019.gpkg"), extent = zoomin)
# mapview::mapview(v)

# rpg19 <- vect(file.path(dirfile, "nutz_2019.gpkg"))

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
# only 2025 : to be checked with Selma
# table(rpg25$Hauptkategorie_FR)
# table(duplicated(data.frame(rpg25)))
#remove duplicates
rpg25 <- rpg25[!duplicated(data.frame(rpg25)), ]
rpg25$nutzung_fr <- gsub("Riz en culture seche", "Riz", rpg25$nutzung_fr)
# get numeric id
m_rpg <- match(rpg25$nutzung_fr, ref$name)
# table(is.na(m_rpg))
# unique(rpg25$nutzung_fr[is.na(m_rpg)])

# table(rpg25$nutzung_fr)
writeVector(rpg25, file.path(dirfile, "nutz_2025.gpkg"), overwrite = TRUE)


rpg24 <- vect(file.path(
  dirfile,
  "buffer_4000_lnf_hk_2024.gpkg"
))
# table(rpg24$year, useNA = "ifany") # 401436
# head(rpg24)
Encoding(rpg24$nutzung_fr)
rpg24$nutzung_fr <- iconv(
  rpg24$nutzung_fr,
  from = "UTF-8",
  to = "ASCII//TRANSLIT"
)
rpg24$nutzungsidentifikator <- rpg24$id_kanton_agis

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
  file.path(dirfile, "nutz_2024.gpkg"),
  overwrite = TRUE
)

colNUTZ <- c("nutzungsidentifikator", "nutzung_fr")
colNUTZ %in% names(rpg24)
names(rpg19)
names(rpg24) #different than
names(rpg25)

rpg23 <- vect(file.path(dirfile, "nutz_2023.gpkg"))
table(duplicated(data.frame(rpg23)))

table(rpg25$year, useNA = "ifany") # 202042
table(rpg24$year, useNA = "ifany") # 202042
table(rpg24$hauptkategorie_fr)
plot(rpg24)
plot(rpg25, col = "red", add = TRUE)

zoomin <- ext(c(2508265, 2509047, 1119836, 1121222)) + 2000
v4 <- crop(rpg24, zoomin)
v5 <- crop(rpg25, zoomin)
mapview::mapview(v4) +
  mapview::mapview(v5)


# semi naturel elements
snh23 <- vect(file.path(
  dirfile,
  "buffer_4000_snh_2023_60.gpkg"
))

table(snh23$type_source) # semi_nat : 943975
table(snh23$year) # 2023
table(snh23$element_typ) # not sure what does that mean
unique(snh23$typ_flaechen) # more interesting
h23 <- snh23[grep("Hecken", snh23$typ_flaechen), ]


# semi naturel elements
snh24 <- vect(file.path(
  dirfile,
  "buffer_3000_snh_2024.gpkg"
))
table(snh24$type_source) # semi_nat : 530788
table(snh24$year) # 2024
table(snh24$typ_flaechen) # more interesting
h24 <- snh24[grep("Hecken", snh24$typ_flaechen), ]

# plot(h23, col = "red")
# plot(h24, col = "blue", add=TRUE)
d34 <- symdif(h23, h24)


## Hecken 2023
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

zoomin <- ext(c(2508265, 2509047, 1119836, 1121222)) + 2000
v <- crop(hedge, zoomin)
mapview::mapview(v)


rot <- readxl::read_xlsx(
  file.path(dirfile, "PSM_2017-2019.xlsx")
)

rot$head(rot)
table(rot$Culture %in% rpg_ch$nutzung_fr)

df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# project in EPSG 2056
pts <- project(pts, "EPSG:2056")

# select only observation in Switzerland
keep <- pts$Study_ID %in% "PestiRed" & !is.na(df$Lat)
ch <- df[keep, ]

id_split <- strsplit(ch$Plot_ID, "\\.")
firstid <- sapply(id_split, function(x) x[[1]])
firstid %in% rot$ID_CODE
duplicated(rot$ID_CODE)
View(rot)
