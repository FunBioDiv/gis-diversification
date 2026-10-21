library(terra)
library(here)
datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# parameters
res = 10
dep = 33

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

grd <- rast(oso_22, res = res)

print("2. Loop on years -----")
all <- rast()
for (i in 2016:2022) {
  print(i)
  outi <- paste0("Merged_d", dep, "_", i, "_", res, "m.tif")
  # test if already existing
  if (file.exists(here(outfolder, outi))) {
    all_i <- rast(here(outfolder, outi))
  } else {
    # RPG
    print("  Rasterize RPG")
    m_rpg <- match(
      unlist(rpg[, paste0("cult", i), drop = TRUE]),
      ref$code_culture
    )
    # check if missing values
    missing <- unique(unlist(rpg[is.na(m_rpg), paste0("cult", i), drop = TRUE]))
    missing <- missing[!is.na(missing)]
    if (length(missing) > 0) {
      warning(paste("Missing values for RPG", i))
    }
    rpg$labi <- ref$info_grp_id[m_rpg]
    rpg_r <- rasterize(rpg, grd, "labi")
    rpg_r <- clamp(rpg_r, 1, max(ref$info_grp_id, na.rm = TRUE), values = FALSE)

    #RPG Complete
    print("  Rasterize RPG Complete")
    lay <- ifelse(
      i == "2022",
      "culture",
      paste0("rpg", substr(i, 3, 4), "_cult")
    )
    dati <- unlist(rpg_c[, lay, drop = TRUE])
    m_rpc <- ifelse(
      is.na(match(dati, ref$code_culture)),
      match(dati, ref$nom_culture),
      match(dati, ref$code_culture)
    )
    missing <- unique(dati[is.na(m_rpc)])
    missing <- missing[!is.na(missing)]
    if (length(missing) > 0) {
      warning(paste("Missing values for RPG Complete", i))
    }
    rpg_c$labi <- ref$info_grp_id[m_rpc]
    rpc_r <- rasterize(rpg_c, grd, "labi")
    rpc_r <- clamp(rpc_r, 1, max(ref$info_grp_id, na.rm = TRUE), values = FALSE)

    # OSO
    print("  Rasterize OSO")
    oso_i <- vect(here(
      datafolder,
      paste0("OSO_", i, "_departement_", dep, ".shp")
    ))
    lab_i <- names(oso_i)[!names(oso_i) %in% nolab]
    # complex finding of which class for which label
    df <- oso_i[, lab_i, drop = TRUE]
    ndf <- apply(is.na(df), 1, sum)
    mdf <- apply(df, 1, which.max)
    ct <- table(unlist(mdf), oso_i$Classe[unlist(ndf) < ncol(df)])
    conv_classes <- lab_i[as.numeric(row.names(ct)[apply(ct, 2, which.max)])]
    names(conv_classes) <- colnames(ct)
    # rasterize oso with labels
    oso_i$name <- conv_classes[as.character(oso_i$Classe)]
    # check with harmonized names
    m_oso <- ifelse(
      is.na(match(oso_i$name, ref$code_culture)),
      match(oso_i$name, ref$nom_culture),
      match(oso_i$name, ref$code_culture)
    )
    missing <- unique(oso_i$name[is.na(m_oso)])
    missing <- missing[!is.na(missing)]
    if (length(missing) > 0) {
      warning(paste("Missing values for OSO", i))
    }
    # unique(oso_22$name[is.na(m_oso)]) # none
    oso_i$labi <- ref$info_grp_id[m_oso]
    oso_r <- rasterize(oso_i, grd, "labi")

    # MERGE
    print("  Combine RPG+RPGc+OSO -----")
    all_i <- merge(rpg_r, rpc_r, oso_r, first = TRUE)
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

outfile <- paste0("Landcover_d", dep, "_2016_2022_", res, "m.tif")
# export
writeRaster(
  all,
  filename = here(outfolder, outfile),
  overwrite = TRUE
)

# remove temporary files when stopping
# terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
# system.time({source("analysis/b2_prep_stack.R")})
#
# d35, 10m : 8 min, 233Mb

# d33, 10m : 46 min, 215Mb
# d33, 5m : ~3h30 min, to be tested
# d33, 2m : ~14h, to be tested
