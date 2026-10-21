# Get indicators from rasters RPG+OSO 10m
# run roughly in 1h
# input:
#   data/derived-data/Landcover_RPG-OSO_2016_2023.tif (29Gb)
#   data/data/raw-data/fields_FR.gpkg
# output:
#   data/derived-data/raster_indicators.csv
#   data/derived-data/raster_edges.csv

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
t0_obs <- match(pts$Year, names(pt_lulc))
t1_obs <- match(as.character(as.numeric(pts$Year) - 1), names(pt_lulc))
t2_obs <- match(as.character(as.numeric(pts$Year) - 2), names(pt_lulc))
t3_obs <- match(as.character(as.numeric(pts$Year) - 3), names(pt_lulc))
t4_obs <- match(as.character(as.numeric(pts$Year) - 4), names(pt_lulc))
t5_obs <- match(as.character(as.numeric(pts$Year) - 5), names(pt_lulc))
# unique(pts$Year[is.na(t_obs)]) # 2014, 2015, 2024

# get the values at time N
lulc_id <- data.frame(
  "lulc_N" = pt_lulc[cbind(1:nrow(pts), t0_obs)],
  "lulc_N1" = pt_lulc[cbind(1:nrow(pts), t1_obs)],
  "lulc_N2" = pt_lulc[cbind(1:nrow(pts), t2_obs)],
  "lulc_N3" = pt_lulc[cbind(1:nrow(pts), t3_obs)],
  "lulc_N4" = pt_lulc[cbind(1:nrow(pts), t4_obs)],
  "lulc_N5" = pt_lulc[cbind(1:nrow(pts), t5_obs)]
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
  "Study_ID" = pts$Study_ID,
  "Year" = pts$Year,
  lulc_id,
  all[match(pts$ID, all$ID), -1]
)

write.csv(
  out,
  here("data", "derived-data", "raster_indicators.csv"),
  row.names = FALSE
)

lab <- sort(unique(sapply(strsplit(names(all)[-1], "_"), function(x) x[[2]])))
lab_class <- miniref$fullname[match(lab, miniref$new_code)]

## 4. Get edge density -----------------------------
buff <- buffer(pts, 1500)

# SNC
newclass <- ref[, c("new_code", "class_id")]
newclass <- newclass[!duplicated(newclass) & complete.cases(newclass), ]
labclass <- ref[, c("class_id", "class_label")]
labclass <- labclass[!duplicated(labclass) & complete.cases(labclass), ]

# RPG
labref <- ref[, c("new_code", "nom")]
labref <- labref[!duplicated(labref$new_code) & complete.cases(labref), ]
labrm <- ref$nom[ref$class_label != "agriculture"]

density <- c()
# loop over the buffers
for (i in 1:length(buff)) {
  if (i %% 100 == 0) {
    print(i)
  }
  bi <- buff[i]
  yri <- bi$Year
  if (yri %in% names(rpgoso)) {
    ri <- crop(rpgoso[yri], bi, mask = TRUE)
    # crop - SNH
    ci <- classify(ri, newclass)
    set.cats(ci, value = labclass)
    e_SNC_10m <- edges(as.polygons(ci), rm = "Impermeable", out = "perim")
    c2 <- aggregate(ci, 2, fun = "modal")
    e_SNC_20m <- edges(as.polygons(c2), rm = "Impermeable", out = "perim")
    c5 <- aggregate(ci, 5, fun = "modal")
    e_SNC_50m <- edges(as.polygons(c5), rm = "Impermeable", out = "perim")

    # crops RPG
    set.cats(ri, value = labref)
    e_RPG_10m <- edges(as.polygons(ri), rm = labrm, out = "perim")
    r2 <- aggregate(ri, 2, fun = "modal")
    e_RPG_20m <- edges(as.polygons(r2), rm = labrm, out = "perim")
    r5 <- aggregate(ri, 5, fun = "modal")
    e_RPG_50m <- edges(as.polygons(r5), rm = labrm, out = "perim")

    #fmt: skip
    outi <- c(unlist(values(bi)), 
              e_SNC_10m, e_SNC_20m, e_SNC_50m, e_RPG_10m, e_RPG_20m, e_RPG_50m)
    density <- rbind(density, outi)
  }
}
colnames(density)[5:10] <- c(
  "e_SNC_10m",
  "e_SNC_20m",
  "e_SNC_50m",
  "e_RPG_10m",
  "e_RPG_20m",
  "e_RPG_50m"
)
write.csv(
  density,
  here("data", "derived-data", "raster_edges.csv"),
  row.names = FALSE
)
