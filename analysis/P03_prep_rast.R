# Combine OSO and RPG data as raster file based on classes
# run rouglhy in 9h for 2016:2023 (1h per year)
# input:
#   data/derived-data/RPG-OSO_classes.csv
#   data/raw-data/RPG (45 Gb)
#   data/raw-data/OSO (6.6Gb)
# output:
#   data/derived-data/RPG-OSO_XXXX.tif
#   data/derived-data/Landcover_RPG-OSO_2016_2023.tif

suppressWarnings({
  library(terra)
  library(sf)
  library(here)
})

print("1. Load data -----")
# load data
datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# rpg and oso classes
ref <- read.csv(here(outfolder, "RPG-OSO_classes.csv"))
# make small correspondance table
ref$fullname <- paste(ref$source, ref$nom, sep = "_")
miniref <- ref[, c("new_code", "fullname")]
miniref <- miniref[!duplicated(miniref$new_code), ]
# dim(miniref) # 359 factors !!

# rpg file
rpg_files <- list.files(here(datafolder, "RPG"), full.names = TRUE)
# select by extension
rpg_files <- rpg_files[c(grep("shp$", rpg_files), grep("gpkg$", rpg_files))]

# oso file
oso_files <- list.files(here(datafolder, "OSO"), full.names = TRUE)
# select by extension
oso_files <- oso_files[grep("tif$", oso_files)]

# parameters
period <- 2016:2023

print("2. Loop on years -----")
all <- rast()
for (i in period) {
  print(i) # around 10 min per year
  # check if available
  outi <- paste0("RPG-OSO_", i, ".tif")
  # test if already existing
  if (file.exists(here(outfolder, outi))) {
    all_i <- rast(here(outfolder, outi))
  } else {
    # Load OSO
    oso_i <- rast(oso_files[grep(i, oso_files)])
    # Load RPG
    rpg_i <- vect(rpg_files[grep(i, rpg_files)])

    print("   Rasterize RPG")
    # project RPG
    # not sure if needed, is IGNF:LAMB93 different from EPSG:2154?
    rpg_i <- project(rpg_i, crs(oso_i))

    # transform code cultivated with correct id
    m_rpg <- match(rpg_i$CODE_CULTU, ref$original_code)
    # check if missing values
    if (sum(is.na(m_rpg)) > 0) {
      warning(paste("Missing values for RPG", i))
    }
    rpg_i$id <- ref$new_code[m_rpg]
    rpg_r <- rasterize(rpg_i, oso_i, "id")
    # needed ?
    # rpg_r <- clamp(rpg_r, 1, max(ref$new_code, na.rm = TRUE), values = FALSE)

    print("   Classify OSO")
    # Re-classify OSO
    if (i < 2018) {
      newclass <- ref[ref$source == "OSO_old", c("original_code", "new_code")]
    } else {
      newclass <- ref[ref$source == "OSO", c("original_code", "new_code")]
    }
    # make sure everything is numeric
    newclass <- sapply(newclass, as.numeric)
    oso_r <- classify(oso_i, newclass)

    # MERGE
    print("   Combine RPG+OSO")
    all_i <- merge(rpg_r, oso_r, first = TRUE)
    # set factor values
    set.cats(all_i, value = miniref)

    # export
    writeRaster(all_i, filename = here(outfolder, outi))
  }

  # add to time series
  all <- c(all, all_i)
  names(all)[nlyr(all)] <- i
}

print("3. Export all -----")

outfile <- "Landcover_RPG-OSO_2016_2023.tif"
# export
writeRaster(
  all,
  filename = here(outfolder, outfile),
  overwrite = TRUE
)
