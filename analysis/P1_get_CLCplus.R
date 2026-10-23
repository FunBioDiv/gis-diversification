# Get clcplus data from WEKEO platform
#
# input:
#   data/derived-data/coordinates_year_crop.csv
# output:
#   data/raw-data/clcplus (land cover of crop fields)
#
# access WeKEO platform for CLCplus data
#   from https://help.wekeo.eu/en/articles/7035318-how-to-use-the-hdar-package-for-accessing-the-wekeo-hda-api-in-r
#
library(terra)
library(hdar)
client <- Client$new()

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
clcfolder <- file.path(datafolder, "clcplus")

# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here::here("data", "derived-data", "coordinates_year_crop.csv"))
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

# Years 2018 and 2021 are large rasters
# Year 2023 is made of multiple files

# Handle the multiple tiles for 2023
clc23 <- list.files(zipfolder, "S2023", full.names = TRUE)
# unzip the tiles
for (i in clc23) {
  archive::archive_extract(i, dir = zipfolder)
}
# list all geotiffs
tif23 <- list.files(zipfolder, ".tif$", full.names = TRUE)
# load the rasters
clc23_list <- lapply(tif23, rast)
# make a mosaic of the multiple tiles
clc23_mos <- mosaic(sprc(clc23_list))
# save the output in a single raster file
out <- file.path(clcfolder, "CLMS_CLCplus_RASTER_2023_010m_eu_03035_V1_1.tif")
writeRaster(clc23_mos, out)
# then clean the folder by hand
