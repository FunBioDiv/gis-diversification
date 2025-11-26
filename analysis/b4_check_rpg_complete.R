library(terra)
library(here)
ext_35 <- ext(c(360000, 365000, 6830000, 6836000))
zoom_35 <- vect(ext_35, crs = "EPSG:2154")

# RPG complete
# only 2018-2022
rpgc_files <- list.files(
  here("data", "rpgc"),
  "shp$",
  recursive = TRUE,
  full.names = TRUE
)
year_files <- substr(rpgc_files, 64, 67)
rpgc_list <- lapply(rpgc_files, vect, filter = zoom_35)
names(rpgc_list) <- year_files

# RPG 53
rpg_files <- list.files(
  here("data", "rpg"),
  "PARCELLES_GRAPHIQUES",
  recursive = TRUE,
  full.names = TRUE
)
rpg_files <- rpg_files[c(grep("shp$", rpg_files), grep("gpkg$", rpg_files))]

year_files <- substr(rpg_files, 74, 77)
rpg_list <- lapply(rpg_files[order(year_files)], vect, filter = zoom_35)
names(rpg_list) <- sort(year_files)

# add transparency
par(mfrow = c(2, 3))
for (i in names(rpgc_list)) {
  inti <- intersect(rpgc_list[[i]], rpg_list[[i]])
  lab <- paste0(i, "- Overlap = ", round(sum(expanse(inti)) * 0.0001, 2), " ha")
  plot(rpg_list[[i]], col = "blue", border = NA, main = lab)
  plot(rpgc_list[[i]], col = "red", border = NA, add = TRUE)
  plot(inti, col = "black", border = NA, add = TRUE)
  legend("bottom", legend = lab, bty = "n")
}
