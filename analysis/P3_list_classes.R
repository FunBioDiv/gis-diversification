# List all categories in CLCplus, Nutzung and RPG dataset
#
# input:
#   data/raw-data/clcplus/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif
#   data/raw-data/CH/buffer_4000_lnf_hk_area_distance_nutz_60.gpkg
#   data/raw-data/RPG_Explorer_Annexe2.csv
#
# output:
#   data/derived-data/classes_clc.csv
#   data/derived-data/classes_nutzung.csv
#   data/derived-data/classes_rpg.csv

library(terra)
devtools::load_all()

datafolder <- here::here("data", "raw-data")
outfolder <- here::here("data", "derived-data")

# 1. List classes in clcplus ---------------------------------------------------
clc18 <- rast(file.path(
  datafolder,
  "clcplus",
  "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif"
))
# clc21 <- rast(file.path(
#   clcfolder,
#   "CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1.tif"
# ))
# clc23 <- rast(file.path(
#   clcfolder,
#   "CLMS_CLCplus_RASTER_2023_010m_eu_03035_V1_1.tif"
# ))

info_clc <- cats(clc18)[[1]][, 1:2]
write.csv(
  info_clc,
  file.path(outfolder, "classes_clc.csv"),
  row.names = FALSE
)

# 2. List classes in Nutzung ---------------------------------------------------
nutzfile <- file.path(
  datafolder,
  "CH",
  "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg"
)
# super heavy (2.4Gb)
nutz <- vect(nutzfile)

nutz_df <- table(nutz$nutzung_fr) |> as.data.frame()
names(nutz_df)[1] <- "nutzung_fr"
nutz_df$Hauptkategorie_FR <- nutz$Hauptkategorie_FR[
  match(nutz_df$nutzung_fr, nutz$nutzung_fr)
]
nutz_df <- nutz_df[
  order(nutz_df$Hauptkategorie_FR, nutz_df$nutzung_fr),
  c("nutzung_fr", "Hauptkategorie_FR", "Freq")
]

write.csv(
  nutz_df,
  file.path(outfolder, "classes_nutzung.csv"),
  row.names = FALSE
)

# 3. List classes in RPG -------------------------------------------------------
# copy the Annexe 2 from
# https://rpgexplorer.fr/wp-content/uploads/2025/03/Notice-mise-a-jour_v1.10.93-1.pdf
rpgc <- read.csv(file.path(datafolder, "RPG_Explorer_Annexe2.csv"))
write.csv(
  rpgc,
  file.path(outfolder, "classes_rpg.csv"),
  row.names = FALSE
)
