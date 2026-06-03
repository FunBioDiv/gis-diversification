# Get clcplus data from WEKEO platform
#
# input:
#   data/raw-data/coordinates_year_crop.csv
# output:
#   data/raw-data/clcplus (land cover of crop fields)
#
# access WeKEO platform for CLCplus data
# from https://help.wekeo.eu/en/articles/7035318-how-to-use-the-hdar-package-for-accessing-the-wekeo-hda-api-in-r
library(terra)
library(hdar)
client <- Client$new()


# For the French dataset, we will download the interesting part from the API directly
# using happign package: https://paul-carteron.github.io/happign
# library(happign)
# might not be needed if F1 is run beforehand
#
# if there is an error, make sure authentification is saved in .hdarc
query <- '{
  "dataset_id": "EO:EEA:DAT:CLC-PLUS",
  "bbox": [
    XMIN,
    YMIN,
    XMAX,
    YMAX
  ],
  "productType": "Raster Layer",
  "resolution": "10m",
  "year": "YYYY",
  "itemsPerPage": 200,
  "startIndex": 0
}'
clcyear <- c(2018, 2021, 2023)

buffer_fields <- c(500, 1000, 1500) #in m

# Load home made functions
devtools::load_all()

datafolder <- here::here("data", "raw-data")
# directory to save raw Corine Land cover data
zipfolder <- file.path(here("data", "zip"))
clcfolder <- file.path(file.path(datafolder, "clcplus"))

# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")

# create buffer
buffi <- buffer(pts, max(buffer_fields))

queri <- gsub("XMIN", ext(buffi)[1], query)
queri <- gsub("XMAX", ext(buffi)[2], queri)
queri <- gsub("YMIN", ext(buffi)[3], queri)
queri <- gsub("YMAX", ext(buffi)[4], queri)

for (i in max(clcyear)) {
  queri <- gsub("YYYY", i, queri)
  matches <- client$search(queri)
  matches$download(clcfolder)
}
# The year 2018 and 2021 are large rasters

# handle the multiple files from 2023 CLCplus
clc23 <- list.files(zipfolder, "S2023", full.names = TRUE)
for (i in clc23) {
  # extract only the COMMUNE.shp
  archive::archive_extract(i, dir = zipfolder)
}

tif23 <- list.files(zipfolder, ".tif$", full.names = TRUE)
clc23_list <- lapply(tif23, rast)
clc23_mos <- mosaic(sprc(clc23_list))

out <- file.path(clcfolder, "CLMS_CLCplus_RASTER_2023_010m_eu_03035_V1_1.tif")

writeRaster(clc23_mos, out)
# then clean by hand (lazy to do it in R)

# check clcplus layer
# /home/rfrelat/Documents/gis-diversification/data/raw-data/clcplus/CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif
clc18 <- rast(file.path(
  clcfolder,
  "CLMS_CLCplus_RASTER_2018_010m_eu_03035_V1_1.tif"
))
clc21 <- rast(file.path(
  clcfolder,
  "CLMS_CLCplus_RASTER_2021_010m_eu_03035_V1_1.tif"
))
clc23 <- rast(file.path(
  clcfolder,
  "CLMS_CLCplus_RASTER_2023_010m_eu_03035_V1_1.tif"
))

info_clc <- cats(clc18)[[1]][, 1:2]
write.csv(
  info_clc,
  file.path(datafolder, "CLCplus_classes.csv"),
  row.names = FALSE
)

# rpg and clcplus classes
ref <- read.csv(here("data", "derived-data", "rpg_classes.csv"))
ref$code.culture[duplicated(ref$code.culture)]
apply(table(ref$code.groupe.intermédiaire, ref$id_group) > 0, 2, sum)

cla <- read.csv2(here("data", "raw-data", "REF_CULTURES_2024.csv"))
cla <- read.csv2(here(
  "data",
  "raw-data",
  "REF_CULTURES_GROUPES_CULTURES_2024.csv"
))
cla[!cla$CODE_CULTURE %in% ref$code.culture, ]


# rpg and clcplus classes
library(terra)
library(here)

dirfile <- here("data", "raw-data", "CH")
file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")

# super heavy (2.4Gb)
rpg_ch <- vect(file)

# table(nutz$Hauptkategorie_FR, useNA="ifany")
rmCat <- c(
  "Forêt",
  "Haies, bosquets et berges boisées ",
  "Surfaces en dehors de la SAU"
)
colNUTZ <- c("nutzungsidentifikator", "nutzung_fr", "Hauptkategorie_FR")

write.csv(table(rpg_ch$nutzung_fr), "nutzung.csv")

ta <- data.frame(rpg_ch)[, c("nutzung_fr", "Hauptkategorie_FR")]
ta <- ta[!duplicated(ta), ]
ta <- ta[order(ta$Hauptkategorie_FR, ta$nutzung_fr), ]
write.csv(ta, "nutzung_cl.csv")


# classes
ref0 <- read.csv(file.path(outfolder, "rpg_nutzung_clc.csv"))
ref1 <- read.csv(file.path(outfolder, "nutzung_classes.csv"))
# fmt: skip
names(ref1) <- c("id", "name", "Other", "grp1", "code_grp1", "name_grp1", "code_grp2", "name_grp2", "grp2")
ref1$source <- "nutzung"
ref1$code <- NA
ref1$keep_ag <- TRUE
# names(ref0) %in% names(ref1)
ref1 <- ref1[, names(ref0)]

ref <- rbind(ref0, ref1[!ref1$name %in% ref0$name[ref0$source == "nutzung"], ])
ref <- ref[order(ref$id), ]
write.csv(ref, file.path(outfolder, "rpg_nutzung_clc.csv"), row.names = FALSE)
