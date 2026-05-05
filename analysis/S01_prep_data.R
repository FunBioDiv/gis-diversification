library(terra)
library(here)

dirfile <- here("data", "raw-data", "CH")
file <- file.path(dirfile, "buffer_4000_lnf_hk_area_distance_nutz_60.gpkg")

# super heavy (2.4Gb)
rpg_ch <- vect(file)

# split the file per year (400Mb)
for (i in 2019:2023) {
  chi <- rpg_ch[rpg_ch$year == i, ]
  # could also select the relevant column to make the data smaller
  writeVector(chi, file.path(dirfile, paste0("nutz_", i, ".gpkg")))
}

zoomin <- ext(c(2508265, 2509047, 1119836, 1121222)) + 2000
v <- vect(file.path(dirfile, "nutz_2019.gpkg"), extent = zoomin)

mapview::mapview(v)
