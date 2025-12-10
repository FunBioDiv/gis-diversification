# simplify and extract a subset of indicator values
# for Sebiopag_BVD
suppressWarnings({
  library(terra)
  library(sf)
  library(here)
  library(exactextractr)
})
devtools::load_all()

# rpg and oso classes
ref <- read.csv(here("data", "derived-data", "RPG-OSO_classes.csv"))

# load the indicator values
indr <- read.csv("data/derived-data/raster_indicators.csv")
# select the plots of interest
sebvd <- indr[indr$Study_ID == "SEBIOPAG_BVD", ]

ref$fullname <- paste(ref$source, ref$nom, sep = "_")
miniref <- ref[, c("new_code", "fullname")]
miniref <- miniref[!duplicated(miniref$new_code), ]
miniref <- miniref[!is.na(miniref$new_code), ]
miniref$fullname <- gsub("OSO_old_", "OSO_", miniref$fullname)

# transform with class names
lulc_id <- sebvd[, grep("lulc_", names(sebvd))]
lulc_class <- sapply(lulc_id, function(x) {
  miniref$fullname[match(x, miniref$new_code)]
})

# get the most commun classes at 500m buffer
buff_id <- sebvd[, grep("^frac500_", names(sebvd))]
buff_id <- buff_id[, colSums(buff_id, na.rm = TRUE) > 0.1]
# fmt: skip
buff_lab <- paste0(
  "buffer500_",
  miniref$fullname[match(gsub("frac500_", "", names(buff_id)), miniref$new_code)])

names(buff_id) <- buff_lab
buff_id <- round(buff_id * 100, 2)
buff_id[is.na(buff_id)] <- 0

info <- sebvd[, 1:3]
info$Site <- sapply(strsplit(info$ID, split = "_"), function(x) x[[3]])
out <- cbind(info, lulc_class, buff_id)
View(out)

write.csv(
  out,
  "data/derived-data/SEBIOPAG_BVD_landcover.csv",
  row.names = FALSE
)
