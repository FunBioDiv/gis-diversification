# Get all plots around the fields coordinates in a buffer
# considering the year of the measurement
#
# input:
#   data/raw-data/coordinates_year_crop.csv
#   data/raw-data/CH/nutz_20XX.gpkg (2019-2023) (2.3Gb)
#   data/raw-data/happign (2015-2024) (532Mb)
#   data/raw-data/clcplus (downloaded from F00_get_rawdata.R)
#   data/raw-data/classes_rpg_nutzung_clc.csv homogenized classes of RPG, Nutzung and CLCplus
#   data/raw-data/VergersVignes/CVI_Parcelles_France_Entiere_2021.gpkg vignes from Fred
# output:
#   data/derived-data/metrics_lulc.csv (metrics)
#   data/derived-data/lulc (overlaid land cover, 86 Mb)

library(terra)
library(here)
# Load home made functions
devtools::load_all()

period <- 2015:2025
buffer_fields <- 1000 #in m

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")
tempfolder <- file.path(outfolder, "lulc")

# get reference on classes
ref <- read.csv(file.path(outfolder, "classes_rpg_nutzung_clc.csv"))

# directory with rpg data from happign
happignfolder <- file.path(datafolder, "happign")

# key column id_cultu, code_cultu
colRPG <- c("id_parcel", "code_cultu", "id_cultu")

# remove non agricultural fields from RPG
# rmRPG <- c("BFP", "BFS", "BOR", "BTA", "SBO", "SNA")
rmRPG <- sort(unique(ref$code[ref$keep_ag == "0" & ref$source == "rpg"]))

# file with nutz dataset
nutz_layer <- file.path(datafolder, "CH", "nutz_XXXX.gpkg")
# remove non agricultural fields from NUTZ
rmNUTZ <- sort(unique(ref$name[ref$keep_ag == "0" & ref$source == "nutzung"]))

# directory with Corine Land cover data
clcfile <- file.path(
  datafolder,
  "clcplus",
  "CLMS_CLCplus_RASTER_YYYY_010m_eu_03035_V1_1.tif"
)
clcyear <- c(2018, 2021, 2023)

# extra dataset to complete orchards and vineyards
vvfolder <- file.path(datafolder, "VergersVignes")

# vine from Adrien Rusch 19/05/2026
# orchard from Claire Lavigne 19/05/2026
# already included in F01_get_field_metrics

# vine from Frederic Fabre  26/05/2026
vineFr <- vect(file.path(vvfolder, "CVI_Parcelles_France_Entiere_2021.gpkg"))
vineFr <- project(vineFr, "EPSG:3035")
# set to code 411
vineFr$gp <- ref$id[ref$source == "Fred"]

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# project to EPSG:3035 to match clcplus crs
pts <- project(pts, "EPSG:3035")
# table(pts$Year)

# select only observation in France between 2015 and 2024
keep <- pts$Year %in% period & !is.na(df$Lat) & pts$Study_ID != "Agro4st"
# table(keep) # 2597 points

df_out <- list() # save the metrics
# for testing: sample(which(keep), 10)
# for processing: which(keep)
for (i in which(keep)) {
  cat(i)

  # select point i
  pti <- pts[i]

  # set file name
  outi <- file.path(tempfolder, paste0("LULC_", pti$ID, ".tif"))
  # check if file exist
  if (file.exists(outi)) {
    alli <- rast(outi)
  } else {
    # compute it from scratch
    # create maximum buffer
    buffi <- buffer(pti, max(buffer_fields))

    if (pti$Study_ID %in% "PestiRed") {
      # project buffer for NUTZ layer
      buffi_2056 <- project(buffi, "EPSG:2056")
      # load nutz data within buffer i
      rpgi <- vect(gsub("XXXX", pti$Year, nutz_layer), ext = buffi_2056)

      # remove non agricultural fields
      rpgi <- rpgi[!rpgi$nutzung_fr %in% rmNUTZ, ]
      # get numeric id
      m_rpg <- match(rpgi$nutzung_fr, ref$name)
      if (sum(is.na(m_rpg)) > 0) {
        # fmt:skip
        print(paste("Missing values for NUTZ", i, ":",
          unique(rpgi$nutzung_fr[is.na(m_rpg)])
        ))
      }
      rpgi$gp <- ref$id[m_rpg]
    } else {
      rpgbi <- file.path(happignfolder, paste0("RPGbuf_", pti$ID, ".gpkg"))
      rpgci <- file.path(happignfolder, paste0("RPGcomp_", pti$ID, ".gpkg"))
      if (file.exists(rpgci)) {
        rpgi <- vect(rpgci)
      } else {
        rpgi <- vect(rpgbi)
      }

      # remove non agricultural fields
      rpgi <- rpgi[!rpgi$code_cultu %in% rmRPG, ]
      rpgi$gp <- rpgi$id_cultu
    }
    # project to 3035
    rpgi <- project(rpgi, "EPSG:3035")

    # load clc
    yi <- clcyear[which.closest(clcyear - pti$Year)]
    clcfi <- gsub("YYYY", yi, clcfile)
    clci <- rast(clcfi) #, ext(buffi))
    # crop to the buffer area
    clci <- crop(clci, buffi) #, mask = TRUE)
    clci <- as.numeric(clci) + 1000

    # Rasterize rpg
    if (nrow(rpgi) > 0) {
      # transform code cultivated with correct id
      rpgi_r <- rasterize(rpgi, clci, "gp")

      # select vineyard around i
      vini <- crop(vineFr, ext(buffi) + 0.01)
      # crop for all even if no vine
      # is.related(ext(vineFr), buffi, "intersects")

      if (nrow(vini) > 0) {
        # rasterize vignes
        vini_r <- rasterize(vini, clci, "gp")

        # Combine Vineyards+RPG+CLC
        alli <- merge(vini_r, rpgi_r, clci, first = TRUE)
      } else {
        # Combine RPG*+CLC
        alli <- merge(rpgi_r, clci, first = TRUE)
      }
    } else {
      alli <- clci
    }
    # export
    writeRaster(alli, filename = outi)
  }

  cat(".")
  # Calculate land cover proportion
  out_i <- data.frame("ID" = pti$ID)
  # proportion of land cover per buffer size
  for (f in max(buffer_fields)) {
    buf_if <- st_as_sf(buffer(pti, f))
    buf_if <- st_transform(buf_if, crs(alli))
    lulc_if <- exactextractr::exact_extract(
      alli,
      buf_if,
      fun = "frac",
      progress = FALSE
    )
    names(lulc_if) <- gsub("^frac_", paste0("frac", f, "_"), names(lulc_if))
    out_i <- cbind(out_i, round(lulc_if * 100, 2))
  }

  # Calculate edge density => too complicated
  # cat(".")
  # to be defined
  df_out[[i]] <- out_i
}

df_out <- dplyr::bind_rows(df_out)
df_out[is.na(df_out)] <- 0

catN <- as.numeric(sapply(strsplit(colnames(df_out)[-1], "_"), last))
df_out <- df_out[, c(1, order(catN) + 1)]

write.csv(
  df_out,
  file.path(outfolder, "metrics_lulc.csv"),
  row.names = FALSE
)

# apply(df_out[-1], 2, sum)
