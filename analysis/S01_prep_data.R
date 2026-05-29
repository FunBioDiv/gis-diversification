library(terra)
library(here)

dirfile <- here("data", "raw-data", "CH")
file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")

# super heavy (2.4Gb)
rpg_ch <- vect(file)

# split the file per year (400Mb)
for (i in 2019:2023) {
  chi <- rpg_ch[rpg_ch$year == i, ]
  # could also select the relevant column to make the data smaller
  writeVector(chi, file.path(dirfile, paste0("nutz_", i, ".gpkg")))
}

zoomin <- ext(c(2508265, 2509047, 1119836, 1121222)) + 2000
v <- vect(file.path(dirfile, "nutz_2019.gpkg"), extent = zoomin)
mapview::mapview(v)


rpg2 <- vect(file.path(
  dirfile,
  "buffer_3000_lnf_hk_area_distance_nutz_24to25_true.gpkg"
))
table(f24$year, useNA = "ifany")
# only 2025 : to be checked with Selma
# split the file per year (400Mb)
# for (i in 2014:2025) {
#   chi <- rpg2[rpg2$year == i, ]
#   # could also select the relevant column to make the data smaller
#   writeVector(chi, file.path(dirfile, paste0("nutz_", i, ".gpkg")))
# }

# semi naturel elements
snh23 <- vect(file.path(
  dirfile,
  "buffer_4000_snh_2023_60.gpkg"
))

table(snh23$type_source) # semi_nat
table(snh23$year) # 2023
table(snh23$element_typ) # not sure what does that mean
unique(snh23$typ_flaechen) # more interesting
head(snh23)
h23 <- snh23[grep("Hecken", snh23$typ_flaechen), ]


# semi naturel elements
snh24 <- vect(file.path(
  dirfile,
  "buffer_3000_snh_2024.gpkg"
))

table(snh24$type_source) # semi_nat
table(snh24$year) # 2023
table(snh24$element_typ) # not sure what does that mean
table(snh24$typ_flaechen) # more interesting
h24 <- snh24[grep("Hecken", snh24$typ_flaechen), ]
plot(h24, add = TRUE, border = "blue")


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
