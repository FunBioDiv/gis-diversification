# Get indicators from rasters RPG+OSO 10m
# input:
#   data/derived-data/Landcover_RPG-OSO_2016_2023.tif (29Gb)
#   data/data/raw-data/fields_FR.gpkg
# output:
#   data/derived-data/raster_indicators.csv

suppressWarnings({
  library(terra)
  library(sf)
  library(here)
  library(exactextractr)
})

## 1. Load dataset -----------------------------
# Load home made functions
devtools::load_all()

# RPG+OSO raster
# from P03_prep_rast.R
rpgoso_files <- list.files(
  here("data", "derived-data"),
  pattern = "^RPG-OSO_.*\\.tif$",
  full.names = TRUE
)
rpgoso <- rast(rpgoso_files)
names(rpgoso) <- gsub("[^0-9]", "", rpgoso_files)


# rpg and oso classes
ref <- read.csv(here("data", "derived-data", "RPG-OSO_classes.csv"))
# make small correspondance table
ref$fullname <- paste(ref$source, ref$nom, sep = "_")
miniref <- ref[, c("new_code", "fullname")]
miniref <- miniref[!duplicated(miniref$new_code), ]
miniref <- miniref[!is.na(miniref$new_code), ]
# dim(miniref) # 359 factors !!

# get the coordinates from the points
# from P01_get_points.R
pts <- vect(here("data", "raw-data", "fields_FR.gpkg"), "fields_FR")

# Create an ID per site
pts$ID <- paste(pts$Study_ID, pts$Site, pts$Year, sep = "_")

# remove duplicates : FRAMEwork_BVD 236_b2 and 236_c2 : not same coordinates => to be checked !
# are there any duplicates, why?
# crds(pts[pts$ID %in% pts$ID[duplicated(pts$ID)], ])
pts <- pts[!duplicated(pts$ID)]

# project points to Lambert93
pts <- project(pts, crs(rpgoso))

buffer_fields <- c(500, 1000, 1500)

## 2. Get load dataset in time -----------------------------
pt_lulc <- extract(rpgoso, pts, ID = FALSE)

# get land cover at year N to N-5
# identify the matching layer corresponding to the time of observation
t_obs <- match(pts$Year, names(pt_lulc))
# unique(pts$Year[is.na(t_obs)]) # 2014, 2015, 2024

# get the values at time N
lulc_id <- data.frame(
  "lulc_N" = pt_lulc[cbind(1:nrow(pts), t_obs)],
  "lulc_N1" = pt_lulc[cbind(1:nrow(pts), ifelse(t_obs > 1, t_obs - 1, NA))],
  "lulc_N2" = pt_lulc[cbind(1:nrow(pts), ifelse(t_obs > 2, t_obs - 2, NA))],
  "lulc_N3" = pt_lulc[cbind(1:nrow(pts), ifelse(t_obs > 3, t_obs - 3, NA))],
  "lulc_N4" = pt_lulc[cbind(1:nrow(pts), ifelse(t_obs > 4, t_obs - 4, NA))],
  "lulc_N5" = pt_lulc[cbind(1:nrow(pts), ifelse(t_obs > 5, t_obs - 5, NA))]
)

# transform with class names
lulc_class <- sapply(lulc_id, function(x) {
  miniref$fullname[match(x, miniref$new_code)]
})

# sort(unique(as.character(unlist(lulc_class))))

## 3. Get land cover in buffer -----------------------------
all <- NULL
for (yr in c(2016:2023)) {
  print(yr)
  # get the points of year i
  pts_yr <- pts[pts$Year %in% yr, ]

  buf_yr <- data.frame("ID" = pts_yr$ID)
  for (f in buffer_fields) {
    buf_pts <- st_as_sf(buffer(pts_yr, f))

    rpg_buf <- exactextractr::exact_extract(
      rpgoso[[as.character(yr)]],
      buf_pts,
      fun = "frac",
      progress = FALSE
    )
    names(rpg_buf) <- gsub("^frac_", paste0("frac", f, "_"), names(rpg_buf))
    buf_yr <- cbind(buf_yr, rpg_buf)
  }
  # merge all
  if (is.null(all)) {
    all <- buf_yr
  } else {
    all <- merge(all, buf_yr, all = TRUE)
  }
}

all <- all[, c("ID", sort(names(all)[-1]))]
dim(all[match(pts$ID, all$ID), ])

out <- data.frame(
  "ID" = pts$ID,
  lulc_id,
  all[match(pts$ID, all$ID), -1]
)

write.csv(
  out,
  here("data", "derived-data", "raster_indicators.csv"),
  row.names = FALSE
)

# lab <- sort(unique(sapply(strsplit(names(all)[-1], "_"), function(x) x[[2]])))
# lab_class <- miniref$fullname[match(lab, miniref$new_code)]

# all$ID[which(all$frac1500_24 > 0)]
# all$ID[which(all$frac1500_30000 > 0)]
