library(terra)
library(here)
datafolder <- here("data", "raw-data")

## 33: Gironde -------------------------------

# Original RPG per region
system.time({
  rpg_ori <- vect(here(datafolder, "PARCELLES_GRAPHIQUES_R75.shp"))
  departments <- vect(here(datafolder, "departement.gpkg"))
  d33 <- departments[departments$code_insee == "33"]
  d33 <- project(d33, "EPSG:2154")
  in33 <- is.related(rpg_ori, d33, "intersects")
  # is.related might be better than crop
  rpg_33 <- rpg_ori[in33, ]
  rpg_33$area_2019_ha <- expanse(rpg_33) * 0.0001
  writeVector(
    rpg_33,
    here(datafolder, "RPG_2019_d33.gpkg"),
    overwrite = TRUE
  )
}) # 3mins

# RPG sequence for 2019
system.time({
  rpg_seq <- vect(here(datafolder, "d33.gpkg"))
  rpg_2019 <- rpg_seq[rpg_seq$parcel2019 != "abs", ]
  rpg_agg <- aggregate(rpg_2019, by = "parcel2019")
  nrow(rpg_agg) # 81293 parcels
  # no need to disagg() because might not touch perfectly
  # yet same field
  rpg_agg$area_2019_ha <- expanse(rpg_agg) * 0.0001
  rpg_agg <- rpg_agg[, c("parcel2019", "cult2019", "area_2019_ha")]
  writeVector(
    rpg_agg,
    here(datafolder, "RPGseq_2019_d33.gpkg"),
    overwrite = TRUE
  )
}) # 1mins

# RPG complete for 2019
system.time({
  rpg_comp <- vect(here(datafolder, "rpg_complete_2022_d33.shp"))
  rpg_2019 <- rpg_comp[!is.na(rpg_comp$rpg19_cult), ] # 19290 sub-parcels
  rpg_agg <- aggregate(rpg_2019, by = "rpg19_cult") # 58 cultures
  rpg_dis <- disagg(rpg_agg) # 7847 parcels
  rpg_dis$area_2019_ha <- expanse(rpg_dis) * 0.0001
  rpg_dis <- rpg_dis[, c("id_parc", "rpg19_cult", "area_2019_ha")]
  names(rpg_dis) <- c("parcel2019", "cult2019", "area_2019_ha")
  rpg_dis$parcel2019 <- 1:nrow(rpg_comp)
  writeVector(
    rpg_dis,
    here(datafolder, "RPGcomp_2019_d33.gpkg"),
    overwrite = TRUE
  )
}) # 4 secs

# Merge RPG sequence + RPG complete
system.time({
  rpg_seq <- vect(here(datafolder, "RPGseq_2019_d33.gpkg"))
  rpg_comp <- vect(here(datafolder, "RPGcomp_2019_d33.gpkg"))
  comp_minus_seq <- erase(rpg_comp, rpg_seq) # looooong
  new_area <- expanse(comp_minus_seq) * 0.0001
  m_area <- match(comp_minus_seq$parcel2019, rpg_comp$parcel2019)
  old_area <- rpg_comp$area_2019_ha[m_area]
  perc_new <- new_area / old_area
  # keep only parcels that are not covered by RPG for more than 75%
  keep <- perc_new > 0.25
  comp_minus_seq <- comp_minus_seq[keep, ]
  # merge RPG seq + RPG complete_minus_seq
  rpg_seq_comp <- rbind(rpg_seq, comp_minus_seq)
  rpg_seq_comp$ori <- c(
    rep("RPG sequence", nrow(rpg_seq)),
    rep("RPG complete", nrow(comp_minus_seq))
  )
  rpg_seq_comp$area_2019_ha <- expanse(rpg_seq_comp) * 0.0001
  writeVector(
    rpg_seq_comp,
    here(datafolder, "RPGseqcomp_2019_d33.gpkg"),
    overwrite = TRUE
  )
}) # 9 mins


## 35: Ille-et-Vilaine -------------------------------
# Original RPG per region
rpg_ori <- vect(here(datafolder, "PARCELLES_GRAPHIQUES_R53.shp"))
departments <- vect(here(datafolder, "departement.gpkg"))
d35 <- departments[departments$code_insee == "35"]
d35 <- project(d35, "EPSG:2154")
in35 <- is.related(rpg_ori, d35, "intersects")
# is.related might be better than crop
rpg_35 <- rpg_ori[in35, ]
rpg_35$area_2019_ha <- expanse(rpg_35) * 0.0001
writeVector(
  rpg_35,
  here(datafolder, "RPG_2019_d35.gpkg"),
  overwrite = TRUE
)

# RPG sequence for 2019
rpg_seq <- vect(here(datafolder, "d35.gpkg"))
rpg_2019 <- rpg_seq[rpg_seq$parcel2019 != "abs", ]
rpg_agg <- aggregate(rpg_2019, by = "parcel2019") # 215539 parcels
rpg_agg$area_2019_ha <- expanse(rpg_agg) * 0.0001
rpg_agg <- rpg_agg[, c("parcel2019", "cult2019", "area_2019_ha")]
writeVector(
  rpg_agg,
  here(datafolder, "RPGseq_2019_d35.gpkg"),
  overwrite = TRUE
)

# RPG complete for 2019
rpg_comp <- vect(here(datafolder, "rpg_complete_2022_d35.shp"))
rpg_2019 <- rpg_comp[!is.na(rpg_comp$rpg19_cult), ] # 8768 sub-parcels
rpg_agg <- aggregate(rpg_2019, by = "rpg19_cult") # 68 cultures
rpg_dis <- disagg(rpg_agg) # 5740 parcels
rpg_dis$area_2019_ha <- expanse(rpg_dis) * 0.0001
rpg_dis <- rpg_dis[, c("id_parc", "rpg19_cult", "area_2019_ha")]
names(rpg_dis) <- c("parcel2019", "cult2019", "area_2019_ha")
rpg_dis$parcel2019 <- 1:nrow(rpg_comp)
writeVector(
  rpg_dis,
  here(datafolder, "RPGcomp_2019_d35.gpkg"),
  overwrite = TRUE
)

# Merge RPG sequence + RPG complete
comp_minus_seq <- erase(rpg_comp, rpg_seq)
new_area <- expanse(comp_minus_seq) * 0.0001
# fmt: skip
old_area <- rpg_comp$area_2019_ha[match(comp_minus_seq$parcel2019,rpg_comp$parcel2019)]
perc_new <- new_area / old_area
# keep only parcels that are not covered by RPG for more than 75%
keep <- perc_new > 0.25
comp_minus_seq <- comp_minus_seq[keep, ]

rpg_seq_comp <- rbind(rpg_seq, comp_minus_seq)
rpg_seq_comp$ori <- c(
  rep("RPG sequence", nrow(rpg_seq)),
  rep("RPG complete", nrow(comp_minus_seq))
)
rpg_seq_comp$area_2019_ha <- expanse(rpg_seq_comp) * 0.0001
writeVector(
  rpg_seq_comp,
  here(datafolder, "RPGseqcomp_2019_d35.gpkg"),
  overwrite = TRUE
)
