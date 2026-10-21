# Read, aggregate, and clean the fields coordinates
#  updated with the multiple files received by participants
#
# input:
#     input file was sent by Axelle on 27/01/2026:
#   data/raw-data/coordinates/dataset_coordinates_wCropSpecies.csv
#     update from Sylvie Ladet (Sebiopag_VcG) on 17/12/2025:
#   data/raw-data/coordinates/XYpoint1_transect1_17parcelles_Sebiopag_Toulouse_L93.shp
#     update from Frederic Fabre (OSCAR) on 03/04/2026:
#   data/raw-data/coordinates/OSCAR_gps_manquant_VF.xlsx
#     Framework_BVD data from Axelle Tortosa on 05/05/2026:
#   data/raw-data/coordinates/FRAMEwork_BVD.csv
#     Agrim data from Cendrine Mony on 31/05/2026:
#   data/raw-data/coordinates/Parcelles_Agrim.gpkg
#     data from Antoine Gardarin sent on 05/06/2026:
#   data/raw-data/coordinates/Seree_2022_Data_pucerons.csv
#   data/raw-data/coordinates/Pigot_2023_Comptages GA 14.06.22 - JP - STATS.xlsx
#   data/raw-data/coordinates/Herrera_2026_Correspondance codes parcelles.xlsx
#     PestiRed updates from Selma on 18/06/2026 and on 02/09/2026:
#   data/raw-data/coordinates/PestiRed_all_siteData.xlsx
#   data/raw-data/coordinates/PestiRed_missing_metrics.xlsx
#     Agro4st data from Roberti Giotto on 18/06/2026 (not included yet)
#   data/raw-data/coordinates/Flaechen_Monitoring_Selma.xlsx
#
# output:
#   data/derived-data/coordinates_year_crop.csv (coordinates per year and crop)
#   data/derived-data/Fields_Unique.gpkg (spatial points with fields)
#
# Run in < 5sec

# load the needed package and functions
devtools::load_all()
library(sf) |> suppressWarnings()
datafolder <- here::here("data", "raw-data", "coordinates")
outfolder <- here::here("data", "derived-data")


# 1. Load the original data file from Axelle -----------------------------------
meta <- read.csv2(file.path(datafolder, "dataset_coordinates_wCropSpecies.csv"))

# Create an ID per site
ID <- paste(meta$Study_ID, meta$Plot_ID, meta$Year, sep = "_")

# remove duplicates : SEBIOPAG_VcG 2022 and 2023
meta <- meta[!duplicated(ID), ]

# guess the coordinate system
proj <- ifelse(meta$Long > 180, "LAMB93", "WGS84")
# transform LAMB93 to WGS84
lamb93 <- meta[proj %in% "LAMB93", c("Long", "Lat")]
shp_5698 <- st_as_sf(lamb93, coords = c("Long", "Lat"), crs = 5698)
shp_4326 <- st_transform(shp_5698, crs = 4326)
coo_4326 <- st_coordinates(shp_4326)
meta[proj %in% "LAMB93", c("Long", "Lat")] <- coo_4326
# plot(meta$Long, meta$Lat)

# 2. update from Sylvie Ladet (Sebiopag_VcG) on 17/12/2026 ---------------------
sebiopagF <- st_read(
    file.path(
        datafolder,
        "XYpoint1_transect1_17parcelles_Sebiopag_Toulouse_L93.shp"
    )
)
vcg_4326 <- st_transform(sebiopagF, crs = 4326)
coo_vcg <- data.frame(
    st_coordinates(vcg_4326),
    "Site" = substr(vcg_4326$NOMPLOT, 1, 3)
)

# Replace SEBIOPAG_VcG coordinates
# but not for T08, T18 and T19 which doesn't have coordinates in Sylvie's dataset
sel <- meta$Study_ID == "SEBIOPAG_VcG" & meta$Plot_ID %in% coo_vcg$Site
m0 <- match(meta$Plot_ID[sel], coo_vcg$Site)
meta$Lat[sel] <- coo_vcg$Y[m0]
meta$Long[sel] <- coo_vcg$X[m0]


# 3. update from Frederic Fabre (OSCAR) on 03/04/2026 --------------------------
oscar <- readxl::read_xlsx(file.path(datafolder, "OSCAR_gps_manquant_VF.xlsx"))
rm <- oscar$plot[oscar$latitude %in% c("à supprimer", "Plot arraché")]
oscar <- oscar[!oscar$plot %in% rm, ]
sel <- meta$Study_ID == "OSCAR" & meta$Plot_ID %in% oscar$plot
m1 <- match(meta$Plot_ID[sel], oscar$plot)
meta$Lat[sel] <- as.numeric(oscar$latitude[m1])
meta$Long[sel] <- as.numeric(oscar$longitude[m1])
meta <- meta[!(meta$Study_ID == "OSCAR" & meta$Plot_ID %in% rm), ]


# 4. add Framework_BVD from Axelle on 05/05/2026 -------------------------------
frame <- read.csv2(file.path(datafolder, "FRAMEwork_BVD.csv"))
# again x and y have been inverted
names(frame)[c(2, 4:6)] <- c("Plot_ID", "Lat", "Long", "Crop_species")
frame <- frame[, names(meta)]
# table(frame$Year) # only 2021
# issue of duplicates, especially 236_b2, and 236_c2 with different coordinates
# because only one year
frame <- frame[!duplicated(frame[, 1:4]), ]
meta <- rbind(meta, frame)


# 5. add Agrim from Cendrine Mony on 31/05/2026 --------------------------------
agrim <- st_read(file.path(datafolder, "Parcelles_Agrim.gpkg"))
# select the centroid of the parcel
agrim_pts <- st_centroid(agrim)
# transform to lat/long
agrim_pts <- st_transform(agrim_pts, "EPSG:4326")
agrim_coo <- st_coordinates(agrim_pts)
agrim_df <- data.frame(
    "Study_ID" = "Agrim2019",
    "Plot_ID" = agrim_pts$AGRIM_2019,
    "Year" = 2019,
    "Crop_species" = agrim_pts$LIB_2019,
    "Lat" = agrim_coo[, "Y"],
    "Long" = agrim_coo[, "X"]
)
meta <- rbind(meta, agrim_df)

# 6. add data from Antoine Gardarin on 05/06/2026 ------------------------------
seree <- read.csv2(file.path(datafolder, "Seree_2022_Data_pucerons.csv"))
# https://doi.org/10.1016/j.agee.2022.107902
seree_df <- data.frame(
    "Study_ID" = "Seree2022",
    "Plot_ID" = seree$Parcelle,
    "Year" = seree$annee,
    "Crop_species" = seree$culture,
    "Lat" = seree$Latitude,
    "Long" = seree$Longitude
)
seree_df <- seree_df[!duplicated(seree_df), ]
meta <- rbind(meta, seree_df)

pigot <- readxl::read_xlsx(
    file.path(datafolder, "Pigot_2023_Comptages GA 14.06.22 - JP - STATS.xlsx"),
    sheet = "GPS"
)
# https://theses.hal.science/tel-04453600v1
pigot_df <- data.frame(
    "Study_ID" = "Pigot2023",
    "Plot_ID" = paste(pigot$Parcelle, pigot$Quadrat, sep = "_"),
    "Year" = substr(pigot$Campagne, 6, 9),
    "Crop_species" = NA,
    "Lat" = pigot$`Somme de Latitude N`,
    "Long" = pigot$`Somme de Longitude E`
)
# table(duplicated(pigot_df))
pigot_df <- pigot_df[!duplicated(pigot_df), ]
meta <- rbind(meta, pigot_df)

herrera <- readxl::read_xlsx(
    file.path(datafolder, "Herrera_2026_Correspondance codes parcelles.xlsx")
)
# https://doi.org/10.57745/HPEMYW
herrera_df <- data.frame(
    "Study_ID" = "Herrera2026",
    "Plot_ID" = paste(
        herrera$Site_corrected,
        herrera$`Code parcelle`,
        herrera$ID_data_paper,
        sep = "_"
    ),
    "Year" = herrera$Annee_corrected,
    "Crop_species" = herrera$Especee_cultivee_corrected,
    "Lat" = herrera$Latitude,
    "Long" = herrera$Longitude
)
herrera_df <- herrera_df[!duplicated(herrera_df), ]
meta <- rbind(meta, herrera_df)

# 7. updates PestiRed data from Selma on 18/06/2026 ----------------------------
pesti <- readxl::read_xlsx(
    file.path(datafolder, "PestiRed_all_siteData.xlsx")
)
pesti_df <- data.frame(
    "Study_ID" = "PestiRed",
    "Plot_ID" = pesti$ID_tech,
    "Year" = pesti$year,
    "Crop_species" = pesti$culture_code,
    "Lat" = pesti$latitude,
    "Long" = pesti$longitude
)

pesti2 <- readxl::read_xlsx(
    file.path(datafolder, "PestiRed_missing_metrics.xlsx")
)
pesti2_df <- data.frame(
    "Study_ID" = "PestiRed",
    "Plot_ID" = pesti2$Plot_ID,
    "Year" = pesti2$Year,
    "Crop_species" = NA,
    "Lat" = pesti2$latitude,
    "Long" = pesti2$longitude
)

pesti_df <- rbind(pesti_df, pesti2_df)
# remove duplicates
pesti_df <- pesti_df[!duplicated(pesti_df[, c("Plot_ID", "Year")]), ]
# remove Pestired data in meta
# table(meta$Study_ID == "PestiRed") # : 259
meta <- rbind(meta[meta$Study_ID != "PestiRed", ], pesti_df)
table(duplicated(meta)) # 2655


# 8. add Agro4st data from Roberti Giotto on 18/06/2026 ------------------------
# https://www.agroforst.ch/daten-agro4esterie-2022/
# agro4st <- readxl::read_xlsx(
#     "data/Agro4st/Flaechen_Monitoring_Selma.xlsx"
# )
# coo <- strsplit(agro4st$`Coordinates (WGS)`, ", ")
# a22_df <- data.frame(
#     "Study_ID" = "Agro4st",
#     "Plot_ID" = paste(
#         agro4st$ID,
#         agro4st$Canton,
#         agro4st$Mode,
#         agro4st$Zone,
#         sep = "_"
#     ),
#     "Year" = 2022,
#     "Crop_species" = agro4st$`AF system`,
#     "Lat" = sapply(coo, function(x) as.numeric(x[1])),
#     "Long" = sapply(coo, function(x) as.numeric(x[2]))
# )
# a23_df <- a22_df
# a23_df$Year <- 2023
# meta <- rbind(meta, a22_df, a23_df)

# table(is.na(meta$Lat), meta$Study_ID, useNA = "ifany")
write.csv(
    meta,
    file.path(outfolder, "coordinates_year_crop.csv"),
    row.names = FALSE
)

# 9. create unique field spatial data ------------------------------------------
# meta <- read.csv(file.path(outfolder, "coordinates_year_crop.csv"))
# dim(meta) # 2655
# length(unique(paste0(meta$Long, meta$Lat, sep = "_"))) # 1141

meta$ID <- paste(meta$Study_ID, meta$Plot_ID, sep = "@")
uID <- sort(unique(meta$ID))
sid <- strsplit(uID, "@")
mid <- match(uID, meta$ID)
sort_unique <- function(x) paste(sort(unique(x[!is.na(x)])), collapse = "; ")
length_unique <- function(x) length(unique(x[!is.na(x)]))
umeta <- data.frame(
    "ID" = uID,
    "Study_ID" = sapply(sid, function(x) x[[1]]),
    "Plot_ID" = sapply(sid, function(x) x[[2]]),
    "Lat" = as.numeric(meta$Lat[mid]), #tapply(meta$Lat, meta$ID, median, na.rm = TRUE),
    "Long" = as.numeric(meta$Long[mid]), #tapply(meta$Long, meta$ID, median, na.rm = TRUE)
    "Years" = tapply(meta$Year, meta$ID, sort_unique),
    "N_years" = tapply(meta$Year, meta$ID, length_unique),
    "Crops" = tapply(meta$Crop_species, meta$ID, sort_unique),
    "N_crops" = tapply(meta$Crop_species, meta$ID, length_unique)
)

# remove missing coordinates
umeta <- umeta[!is.na(umeta$Lat), -1]

# export as csv
# write.csv(umeta, file.path(outfolder, "Fields_Unique.csv"), row.names = FALSE)

# export as geopackage file
sp_meta <- terra::vect(umeta, geom = c("Long", "Lat"), crs = "EPSG:4326")
terra::writeVector(
    sp_meta,
    file.path(outfolder, "Fields_Unique.gpkg"),
    overwrite = TRUE
)
