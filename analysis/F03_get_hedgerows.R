# Get hedgerow density around crop field
# considering hedgerows as constant within the time period
#
# input:
#   data/raw-data/coordinates_year_crop.csv
#   data/raw-data/planet_canopy_cover_30m_v0.1.tif" # cover
#   data/raw-data/planet_agb_30m_v0.1.tif # biomass density (=cover*height) in kg/ha
#   data/derived-data/rpg_fields.gpkg
#   data/derived-data/nutz_fields.gpkg
# output:
#   data/derived-data/metrics_hedgerows.csv (metrics)

library(terra)
library(sf)
library(here)

# Load home made functions
devtools::load_all()

buffer_haie <- 10 #in m

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# Load Liu 2023 dataset
#fmt:skip
liucov <- rast(file.path(datafolder, "Liu2023", "planet_canopy_cover_30m_v0.1.tif"))
liubio <- rast(file.path(datafolder, "Liu2023", "planet_agb_30m_v0.1.tif"))

# load rpg fields
rpg <- vect(file.path(outfolder, "rpg_fields.gpkg"))
# load nutz fields
nutz <- vect(file.path(outfolder, "nutz_fields.gpkg"))

# project and select only ID
rpg_3035 <- project(rpg, crs(liucov))
rpg_3035 <- rpg_3035[, "Funbiodiv_ID"]

nutz_3035 <- project(nutz, crs(liucov))
nutz_3035 <- nutz_3035[, "Funbiodiv_ID"]

# merge rpg and nutz
all <- rbind(rpg_3035, nutz_3035)

# add buffer of 10m
buff <- buffer(all, buffer_haie)

# Liu 2023
ext_liucov <- exactextractr::exact_extract(
  liucov,
  st_as_sf(buff),
  fun = "mean",
  progress = FALSE
)

ext_liubio <- exactextractr::exact_extract(
  liubio,
  st_as_sf(buff),
  fun = "mean",
  progress = FALSE
)

plot(ext_liucov, ext_liubio)

out <- data.frame(
  "ID" = all$Funbiodiv_ID,
  "haie_cover" = ext_liucov,
  "haie_biomass_kgperha" = ext_liubio
)


# compare with information in NUTZ
dirfile <- here("data", "raw-data", "CH")
file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")
# super heavy (2.4Gb)
rpg_ch <- vect(file)
hedge <- rpg_ch[
  rpg_ch$Hauptkategorie_FR == "Haies, bosquets et berges boisées ",
]
# table(nutz$Hauptkategorie_FR, useNA="ifany")
# remove duplicated polygons
uhedge <- unique(hedge[, "Hauptkategorie_FR"])

buffer_hedge <- intersect(uhedge, project(buff, "EPSG:2056"))
buffer_hedge$Area_ha <- expanse(buffer_hedge) * 0.0001

hedge_area <- tapply(buffer_hedge$Area_ha, buffer_hedge$Funbiodiv_ID, sum)
m0 <- match(out$ID, names(hedge_area))
out$haie_nutzung_ha <- ifelse(out$ID %in% names(hedge_area), hedge_area[m0], NA)
out$haie_nutzung_ha[
  grepl("^PestiRed_", out$ID) & is.na(out$haie_nutzung_ha)
] <- 0
out$haie_nutzung_perc <- out$haie_nutzung_ha / (expanse(buff) * 0.0001) * 100

## compare with information in BD Haies v2

haie <- vect(here(datafolder, "haie_2-0.gpkg"))

lab <- "haie_bdhaie_length_m"
haie_buf <- intersect(haie, project(buff, crs(haie)))
length_haie_buf <- tapply(perim(haie_buf), haie_buf$Funbiodiv_ID, sum)

m1 <- match(out$ID, names(length_haie_buf))
#fmt: skip
out$haie_bdhaie_length_m <- ifelse(out$ID %in% names(length_haie_buf), length_haie_buf[m0], NA)
inFrance <- !grepl("^PestiRed_", out$ID) & is.na(out$haie_bdhaie_length_m)
out$haie_bdhaie_length_m[inFrance] <- 0
#fmt: skip
out$haie_bdhaie_density_mperha <- out$haie_bdhaie_length_m / (expanse(buff) * 0.0001) * 100


# export indicators
write.csv(out, file.path(outfolder, "metrics_hedgerows.csv"), row.names = FALSE)
