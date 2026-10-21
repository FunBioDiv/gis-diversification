# Get indicators from vectorial RPG and BD Haie
# run rouglhy 1h for 2016:2023
# input:
#   data/raw-data/fields_FR.gpkg
#   data/raw-data/RPG (45 Gb)
#   data/raw-data/haie_2-0.gpkg (6.8Gb)
# output:
#   data/derived-data/vector_indicators.csv
#   data/derived-data/vector_parcels.gpkg

suppressWarnings({
  library(terra)
  library(sf)
  library(here)
  library(mapview)
})

# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")

# get the coordinates from the points
# from P01_get_points.R
pts <- vect(here(datafolder, "fields_FR.gpkg"), "fields_FR")

# Create an ID per site
pts$ID <- paste(pts$Study_ID, pts$Site, pts$Year, sep = "_")

# remove duplicates : FRAMEwork_BVD 236_b2 and 236_c2
pts <- pts[!duplicated(pts$ID)]

pts <- project(pts, "IGNF:LAMB93")

# project
rpg_files <- list.files(
  path = here(datafolder, "RPG"),
  full.names = TRUE
)
# select by extension
rpg_files <- rpg_files[c(grep("shp$", rpg_files), grep("gpkg$", rpg_files))]

buffer_fields <- c(500, 1000, 1500)

# load BD haie
haie <- vect(here(datafolder, "haie_2-0.gpkg"))
buffer_haie <- c(0, 5, 10)

start <- Sys.time()
out <- c()
shp <- vect()
for (yr in c(2016:2023)) {
  print(yr)
  # get the points of year i
  pts_yr <- pts[pts$Year %in% yr, ]
  # get the rpg of year i
  rpgi <- vect(rpg_files[grep(yr, rpg_files)])
  # extract
  # system.time({
  pt_rpg <- extract(rpgi, pts_yr)
  #}) # ~ 40 sec
  # get the matching parcels
  sub_rpg <- rpgi[rpgi$ID_PARCEL %in% pt_rpg$ID_PARCEL, ]
  # dim(sub_rpg) #84 parcels
  sub_rpg$Perim_m <- perim(sub_rpg)
  sub_rpg$Area_ha <- expanse(sub_rpg) * 0.0001
  # get the haie density per buffer size
  for (b in buffer_haie) {
    lab <- paste0("Length_haie_", b, "_m")
    buf_rpg <- buffer(sub_rpg, b)
    haie_buf <- intersect(haie, buf_rpg)
    length_haie_buf <- tapply(perim(haie_buf), haie_buf$ID_PARCEL, sum)
    #fmt: skip
    m_length <- length_haie_buf[match(sub_rpg$ID_PARCEL, names(length_haie_buf))]
    m_length[is.na(m_length)] <- 0
    sub_rpg[, lab] <- as.numeric(m_length)
  }
  df_sub <- data.frame(sub_rpg)
  m_0 <- match(pt_rpg$ID_PARCEL, sub_rpg$ID_PARCEL)
  out_i <- data.frame(
    data.frame(pts_yr),
    df_sub[m_0, -which(names(df_sub) %in% c("CULTURE_D1", "CULTURE_D1"))]
  )

  # get the average field size per buffer
  for (f in buffer_fields) {
    lab <- paste0("Mean_fieldsize_", f, "m_ha")
    buf_pts <- buffer(pts_yr, f)
    #system.time({
    rpg_buf <- relate(rpgi, buf_pts, "intersects")
    fsize <- rpgi$SURF_PARC * rpg_buf
    fsize[fsize == 0] <- NA
    mean_fsize <- apply(fsize, 2, mean, na.rm = TRUE)
    out_i[, lab] <- mean_fsize
    #}) # 61 sec
  }
  # save output
  # data.frame
  out <- rbind(out, out_i)
  # vector
  shp <- rbind(shp, sub_rpg)
}
end <- Sys.time()
print(end - start)

# export
write.csv(
  out,
  here("data", "derived-data", "vector_indicators.csv"),
  row.names = FALSE
)
writeVector(
  shp,
  here("data", "derived-data", "vector_parcels.gpkg"),
  overwrite = TRUE
)
