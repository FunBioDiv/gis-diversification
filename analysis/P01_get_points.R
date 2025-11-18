# Read the metadata and get the coordinates of the field observation
# The input file is originally at :
# https://docs.google.com/spreadsheets/d/1Lz-IBQAPd8RykPj57Nf1Tutb_fQlXg4fG461i29BYIM
# Creates one files
#   all_points.gpkg : shapefile with coordinates per year and per dataset

# load the needed package and functions
devtools::load_all()
library(terra)
# make sure no one is logged in from Google Account
googlesheets4::gs4_deauth()

# B.1 Load GPS coordinates
url0 <- "https://docs.google.com/spreadsheets/d/1Lz-IBQAPd8RykPj57Nf1Tutb_fQlXg4fG461i29BYIM/"

gis <- googlesheets4::read_sheet(url0, sheet = 2, skip = 2, col_types = "c", )

keepC <- c("Study_ID", "Site", "Year", "X", "Y")
gis <- gis[, keepC]

# correct study_id name
gis$Study_ID <- gsub("SEBIOPAG _BVD", "SEBIOPAG_BVD", gis$Study_ID)


# B.2 Clean the messy coordinates
gis$X <- as.numeric(gsub(",", ".", gis$X))
gis$Y <- as.numeric(gsub(",", ".", gis$Y))

# invert latitude / longitude in projects that wrongly entered coordinates
# table(gis$X>40, gis$Study_ID)
inv_coo <- c(
  "SEBIOPAG_VcG",
  "OSCAR",
  "LepiBats",
  "MUESLI",
  "SEBIOPAG_Plaine de Dijon",
  "SEBIOPAG_BVD",
  "DURUM_MIX_GM",
  "FRAMEwork_BVD",
  "PestiRed"
)
gis$longitude <- ifelse(gis$Study_ID %in% inv_coo, unlist(gis$Y), unlist(gis$X))
gis$latitude <- ifelse(gis$Study_ID %in% inv_coo, gis$X, gis$Y)

# issue some are not in WGS84, but in EPSG 2154 (LAMB93)
proj <- ifelse(gis$longitude > 180, "LAMB93", "WGS84")

# transform LAMB93 to WGS84
lamb93 <- gis[proj %in% "LAMB93", c("longitude", "latitude")]
shp_2154 <- st_as_sf(lamb93, coords = c("longitude", "latitude"), crs = 2154)
shp_4326 <- st_transform(shp_5698, crs = 4326)
coo_4326 <- st_coordinates(shp_4326)
gis[proj %in% "LAMB93", c("longitude", "latitude")] <- coo_4326
# plot(gis[, c("longitude", "latitude")])

# B.3 Add lines per site and year

# clean years
gis$Year <- gsub(" à ", ",", gis$Year)
gis$Year <- gsub("-", ",", gis$Year)
gis$Year <- gsub(" ", "", gis$Year)

# make a new data.frame with lines per year
splityears <- strsplit(gis$Year, ",")
nyears <- sapply(splityears, length)
gis_year <- data.frame(
  "Study_ID" = rep(gis$Study_ID, nyears),
  "Site" = rep(gis$Site, nyears),
  "Year" = unlist(splityears),
  "longitude" = rep(gis$longitude, nyears),
  "latitude" = rep(gis$latitude, nyears)
)

# B.4 Formating the output
# remove missing coordinates
gis_year <- gis_year[complete.cases(gis_year), ]

# Remove PestiRed for now (CH)
gis_year <- gis_year[gis_year$Study_ID != "PestiRed", ]

# remove duplicates
gis_year <- gis_year[!duplicated(gis_year), ]

# order
gis_year <- gis_year[order(gis_year$Study_ID, gis_year$Site, gis_year$Site), ]
dim(gis_year) # 1560, 5
# head(gis_year)
shp <- st_as_sf(gis_year, coords = c("longitude", "latitude"), crs = 4326)
# check visualization
mapview::mapview(shp, zcol = "Study_ID")


# B5. Export shapefile
st_write(
  shp,
  here::here("data", "raw-data", "fields_FR.gpkg"),
  append = FALSE
)
