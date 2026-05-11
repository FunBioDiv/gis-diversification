# Get all plots around the fields coordinates in a buffer
# considering the year of the measurement
#
# input:
#   data/raw-data/coordinates_year_crop.csv
#   data/raw-data/CH/nutz_20XX.gpkg (2019-2023) (2.3Gb)
#   data/raw-data/happign (2015-2024) (570Mb)
#   data/raw-data/clcplus (downloaded from F00_get_rawdata.R)
#   data/derived-data/rpg_classes.csv
#   data/derived-data/nutzung_classes.csv
# output:
#   data/derived-data/metrics_lulc.csv (metrics)
#   data/derived-data/lulc (overlaid land cover, 86 Mb)

library(terra)
library(sf)
library(here)


# Load home made functions
devtools::load_all()

period <- 2015:2024
buffer_fields <- c(500, 1000, 1500) #in m

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")
tempfolder <- file.path(outfolder, "lulc")

# directory with rpg data from happign
happignfolder <- file.path(datafolder, "happign")
# remove non agricultural fields from RPG
rmRPG <- c("BFP", "BFS", "BOR", "BTA", "SNA")

# file with nutz dataset
nutz_layer <- file.path(datafolder, "CH", "nutz_XXXX.gpkg")
# remove non agricultural fields from NUTZ
rmNUTZ <- c(
  "Forêt",
  "Haies, bosquets et berges boisées",
  "Surfaces en dehors de la SAU"
)

# directory with Corine Land cover data
clcfile <- file.path(
  datafolder,
  "clcplus",
  "CLMS_CLCplus_RASTER_YYYY_010m_eu_03035_V1_1.tif"
)
clcyear <- c(2018, 2021, 2023)

# nutz classes
ref_rpg <- read.csv(file.path(outfolder, "rpg_classes.csv"))
ref_nutz <- read.csv(file.path(outfolder, "nutzung_classes.csv"))

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
keep <- pts$Year %in% period & !is.na(df$Lat)
# table(keep) # 1931 points

df_out <- list() # save the metrics
# for testing: sample(which(keep), 10)
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
      rpgi <- rpgi[!rpgi$Hauptkategorie_FR %in% rmNUTZ, ]
      # get numeric id
      m_rpg <- match(rpgi$nutzung_fr, ref_nutz$nutzung_fr)
      rpgi$gp <- ref_nutz$id[m_rpg]
    } else {
      rpgfi <- file.path(happignfolder, paste0("RPGbuf_", pti$ID, ".gpkg"))
      rpgi <- vect(rpgfi)
      # remove non agricultural fields
      rpgi <- rpgi[!rpgi$code_cultu %in% rmRPG, ]
      # get numeric id
      m_rpg <- match(rpgi$code_cultu, ref_rpg$code.culture)
      if (sum(is.na(m_rpg)) > 0) {
        print(paste(
          "Missing values for RPG",
          i,
          ":",
          rpgi$code_cultu[is.na(m_rpg)]
        ))
      }
      rpgi$code_cultu[is.na(rpgi$gp)]
      rpgi$gp <- ref_rpg$id[m_rpg]
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

    # Rasterize NUTZ
    # transform code cultivated with correct id
    rpgi_r <- rasterize(rpgi, clci, "gp")

    # Combine NUTZ+CLC
    alli <- merge(rpgi_r, clci, first = TRUE)

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

apply(df_out[-1], 2, sum)
