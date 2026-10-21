library(terra)
library(here)
datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# parameters
res = 2
dep = 35

print("1.Load data -----")
# load data
rpg <- vect(here(datafolder, gsub("XX", dep, "dXX.gpkg")))
ref_rpg <- read.csv(here(datafolder, "ref_rpg.csv"))

rpg_c <- vect(here(datafolder, gsub("XX", dep, "rpg_complete_2022_dXX.shp")))

oso_22 <- vect(here(datafolder, gsub("XX", dep, "OSO_2022_departement_XX.shp")))
nolab <- c("Classe", "Validmean", "Validstd", "Confidence", "Aire")
lab_oso <- names(oso_22)[!names(oso_22) %in% nolab]

ref <- read.csv(here(outfolder, "classes.csv"))
miniref <- ref[, c("info_grp_id", "nom_grp_culture")]
miniref <- miniref[!duplicated(miniref), ]

grd <- terra::rast(oso_22, res = res)

print("2. Rasterize RPG -----")
rpg$label <- ref$info_grp_id[match(rpg$cult2022, ref$code_culture)]
# rasterize RPG data on label
rpg_r <- rasterize(rpg, grd, "label")
# not sure why but there are some negative values
rpg_r <- clamp(rpg_r, 1, max(ref$info_grp_id, na.rm = TRUE), values = FALSE)


# rasterize RPG complete
print("3. Rasterize RPG complete -----")
m_rpc <- ifelse(
  is.na(match(rpg_c$culture, ref$code_culture)),
  match(rpg_c$culture, ref$nom_culture),
  match(rpg_c$culture, ref$code_culture)
)
# unique(rpg_c$culture[is.na(m_rpc)]) # none
rpg_c$label <- ref$info_grp_id[m_rpc]

# rasterize RPG complete on label
rpc_r <- rasterize(rpg_c, grd, "label")

# rasterize oso with labels
oso_22$name <- lab_oso[oso_22$Classe]

# rasterize OSO
print("4. Rasterize OSO -----")
m_oso <- ifelse(
  is.na(match(oso_22$name, ref$code_culture)),
  match(oso_22$name, ref$nom_culture),
  match(oso_22$name, ref$code_culture)
)
# unique(oso_22$name[is.na(m_oso)]) # none
oso_22$label <- ref$info_grp_id[m_oso]

oso_r <- rasterize(oso_22, grd, "label")

print("5. Combine RPG+RPGc+OSO -----")
all <- merge(rpg_r, rpc_r, oso_r, first = TRUE)
# keep label values
set.cats(all, value = miniref)

#export
fileout <- paste0("Merged_d", dep, "_2022_", res, "m.tif")
writeRaster(
  all,
  filename = here(outfolder, fileout),
  overwrite = TRUE
)

# system.time({source("analysis/b2_prep_data.R")})
#
# d33, 10m : 4 min, 42Mb
# d33, 5m : ~30 min, 124 Mb
# d33, 2m : ~2h, 530Mb
#
# d35, 10m : 3 min, 32Mb
# d35, 5m : 9 min, 98Mb
# d33, 2m : ~2h,
