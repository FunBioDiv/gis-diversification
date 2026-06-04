# Get agricultural plots around the field coordinates
# considering a buffer and the year of the measurement
#
# input:
#   data/raw-data/coordinates_year_crop.csv crop field coordinates
#   data/raw-data/classes_rpg_nutzung_clc.csv homogenized classes of RPG, Nutzung and CLCplus
#   data/raw-data/VergersVignes/vergers_anonyme.gpkg vergers from Claire
#   data/raw-data/VergersVignes/OCS_2018_19_20_21_VF.shp vignes from Adrien
# output:
#   data/raw-data/happign (store downloaded rpg data : 500Mb)
#   data/derived-data/metrics_rpg.csv (metrics)
#   data/derived-data/rpg_fields.gpkg (spatial polygons of crop fields)

# For the French dataset, we will download the interesting part of the RPG from the API directly
# using happign package: https://paul-carteron.github.io/happign
library(happign)
# Run in ~2h to download all data from happign
# else it runs much faster if already downloaded (3 mins)

library(terra)
library(sf)
library(here)

# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# check out which RPG layer is available
# meta_vect <- get_layers_metadata("wfs") # all layers for altimetrie wms
# look_up <- "RPG"
# #fmt: skip
# found <- grepl(tolower(look_up), tolower(meta_vect$Name)) | grepl(tolower(look_up), tolower(meta_vect$Name))
# meta_vect$Name[found]
rpg_layer <- "RPG.XXXX:parcelles_graphiques"

period <- 2015:2024


# directory to save raw RPG data from happign
happignfolder <- file.path(datafolder, "happign")

# key column id_cultu, code_cultu
colRPG <- c("id_parcel", "code_cultu", "id_cultu")

# get reference
ref <- read.csv(file.path(outfolder, "classes_rpg_nutzung_clc.csv"))

# extra dataset to complete orchards and vineyards
vvfolder <- file.path(datafolder, "VergersVignes")

# vine from Adrien Rusch 19/05/2026 -------------------------
# buffer of 1km only
vine <- vect(file.path(vvfolder, "OCS_2018_19_20_21_VF.shp"))
vine <- project(vine, "EPSG:4326")
# rename column
vine$id_parcel <- vine$id
# select only the vineyards
vine <- vine[vine$OCS_20_21 == "Vigne"]
# set to code 410
vine$id_cultu <- 410
# add code_cultu
vine$code_cultu <- ref$code[match(vine$id_cultu, ref$id)]
# simplify the format
vine <- vine[, colRPG]

# vine from Claire Lavigne 19/05/2026 -------------------------
orchard <- vect(file.path(vvfolder, "vergers_anonyme.gpkg"))
orchard <- project(orchard, "EPSG:4326")
# rename column
orchard$id_parcel <- orchard$gid
# set to code 402: verger
orchard$id_cultu <- 401
orchard$id_cultu[orchard$ocs_fr_ %in% "olivier"] = 402
orchard$id_cultu[orchard$ocs_fr_ %in% "poirier"] = 403
orchard$id_cultu[orchard$ocs_fr_ %in% c("pommier", "pommier_ou_poirier")] = 404
# add code_cultu
orchard$code_cultu <- ref$code[match(orchard$id_cultu, ref$id)]
# simplify the format
orchard <- orchard[, colRPG]

# size of buffers
buffer_fields <- c(500, 1000) #in m
# time period for crop rotation
years <- 4

# remove non agricultural fields from RPG
# remove permante grassland and forest for the field size
# rmCat <- c("BFP", "BFS", "BOR", "BTA", "SNA", "SBO")
# information now stored in ref
# ref$code[ref$keep_ag == "0"]
rmCat <- sort(unique(ref$code[ref$keep_ag == "0"]))
# remove also jachere? J5M, J6P, J6S, JAC, JNO

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# table(pts$Year)

# select only observation in France between 2015 and 2024
keep <- pts$Year %in% period & !pts$Study_ID %in% "PestiRed"
# table(keep) # 1728 points

rpg_out <- c() # save the rpg information
df_out <- c() # save the parameters
# for testing: sample(which(keep), 10)
# for processing: which(keep)
for (i in which(keep)) {
  cat(i)

  # select point i
  pti <- pts[i]

  # create maximum buffer
  buffi <- buffer(pti, max(buffer_fields))

  # get rpg around buffer
  # check if existing
  outi <- file.path(happignfolder, paste0("RPGbuf_", pti$ID, ".gpkg"))
  if (file.exists(outi)) {
    rpgi <- vect(outi)
  } else {
    rpgi <- get_wfs(
      x = st_as_sf(buffi),
      layer = gsub("XXXX", pti$Year, rpg_layer)
    )
    if (nrow(rpgi) > 0) {
      # transform as terra SpatVect object
      rpgi <- vect(rpgi)
      # check if all code exist
      if (!all(rpgi$code_cultu %in% ref$code)) {
        warning(paste(
          "Missing code",
          rpgi$code_cultu[!rpgi$code_cultu %in% ref$code]
        ))
      }
      # add id_cultu
      rpgi$id_cultu <- ref$id[match(rpgi$code_cultu, ref$code)]
      # simplify the format
      rpgi <- rpgi[, colRPG]
      # save (in case api doesn't work anymore)
      writeVector(rpgi, outi)
    } else {
      # avoid searching absent RPG
      writeVector(vect(rpgi), outi)
    }
  }

  # remove non agricultural fields
  if ("code_cultu" %in% names(rpgi)) {
    rpgi <- rpgi[!rpgi$code_cultu %in% rmCat, ]
  }

  # complete with vine and orchard if relevant
  compi <- file.path(happignfolder, paste0("RPGcomp_", pti$ID, ".gpkg"))
  if (is.related(ext(vine), buffi, "intersects")) {
    # use ext() + 0.01 to avoid cutting edges of field
    vini <- crop(vine, ext(buffi) + 0.01)
    # keep only the RPG that don't intersect known vineyards
    rpg_nv <- rpgi[!is.related(rpgi, vini, "intersects")]
    # better than select vine that are not in RPG
    # mini <- vini[!is.related(vini, rpgi, "intersects")]

    # bind both data sources
    rpgi <- rbind(rpg_nv, vini)
    writeVector(rpgi, compi, overwrite = TRUE)
  }
  if (is.related(ext(orchard), buffi, "intersects")) {
    orci <- crop(orchard, ext(buffi) + 0.01)
    # keep only the RPG that don't intersect known orchards
    rpg_no <- rpgi[!is.related(rpgi, orci, "intersects")]

    # bind both data sources
    rpgi <- rbind(rpg_no, orci)
    writeVector(rpgi, compi, overwrite = TRUE)
  }

  # continue only if some fields in rpg
  if (nrow(rpgi) > 0) {
    # calculate geometrical characteristics
    rpgi$Perim_m <- perim(rpgi)
    rpgi$Area_ha <- expanse(rpgi) * 0.0001

    ##
    # ponctual information on the site
    cat(".")
    rei <- terra::relate(rpgi, pti, "intersects")
    if (sum(rei) == 1) {
      exi <- rpgi[rei]
      # add funbiodiv ID
      exi$Funbiodiv_ID <- pti$ID
      rpg_out <- c(rpg_out, exi)
      out_i <- data.frame(
        data.frame(pti),
        data.frame(exi)[, c(colRPG, "Perim_m", "Area_ha")]
      )
    } else {
      out_i <- data.frame(
        data.frame(pti),
        matrix(NA, ncol = length(colRPG) + 2)
      )
      #fmt: skip
      names(out_i)[(ncol(pti)+1):ncol(out_i)] <- c(colRPG, "Perim_m", "Area_ha")
    }

    ##
    # average field size in buffer
    cat(".")
    for (f in buffer_fields) {
      lab <- paste0("Median_fieldsize_", f, "m_ha")
      buf_pts <- terra::buffer(pti, f)
      rpg_buf <- terra::relate(rpgi, buf_pts, "intersects")
      out_i[, lab] <- median(rpgi$Area_ha[rpg_buf], na.rm = TRUE)
    }

    ##
    # crop rotation
    cat(".")
    outi <- file.path(happignfolder, paste0("RPGrot_", pti$ID, ".gpkg"))
    # check if the file exists already
    if (file.exists(outi)) {
      rpgi_rot <- vect(outi)
      for (y in 1:years) {
        out_i[paste0("N-", y)] <- pti$Year - y
        lab <- paste0(colRPG, "_N-", y)
        out_i[, lab] <- NA
        if ((pti$Year - y) %in% rpgi_rot$Year) {
          out_i[, lab] <- data.frame(rpgi_rot[
            rpgi_rot$Year == pti$Year - y,
            colRPG
          ])
        }
      }
    } else {
      if (sum(rei) == 1) {
        rpgi_rot <- exi[, colRPG]
        rpgi_rot$Year <- pti$Year
      } else {
        rpgi_rot <- vect()
      }
      for (y in 1:years) {
        out_i[paste0("N-", y)] <- pti$Year - y
        lab <- paste0(colRPG, "_N-", y)
        out_i[, lab] <- NA
        if (!is.na(out_i$id_cultu) & out_i$id_cultu > 400) {
          # if vignes or verges, no rotation
          rpgy <- exi
          rpgy$Year <- pti$Year - y
          out_i[, lab] <- data.frame(rpgy)[, colRPG]
          # keep it for save
          rpgi_rot <- rbind(rpgi_rot, rpgy[c(colRPG, "Year")])
        } else {
          if ((pti$Year - y) %in% period) {
            rpgy <- get_wfs(
              x = st_as_sf(pti),
              layer = gsub("XXXX", pti$Year - y, rpg_layer)
            )
            # add id_cultu
            if (nrow(rpgy) > 0) {
              rpgy <- vect(rpgy)
              rpgy$id_cultu <- ref$id[match(rpgy$code_cultu, ref$code)]
              rey <- terra::relate(rpgy, pti, "intersects")
              if (sum(rey) == 1) {
                rpgy <- rpgy[rey]
                # add information in out_i
                out_i[, lab] <- data.frame(rpgy)[, colRPG]
                # keep it for save
                rpgy$Year <- pti$Year - y
                rpgi_rot <- rbind(rpgi_rot, rpgy[c(colRPG, "Year")])
              }
            }
          }
        }
      }
      #if (nrow(rpgi_rot) > 1) {
      writeVector(rpgi_rot, outi, overwrite = TRUE)
      #}
    }

    df_out <- rbind(df_out, out_i)
  }
}

# export indicators
write.csv(df_out, file.path(outfolder, "metrics_rpg.csv"), row.names = FALSE)

# save rpg fields
rpg_all <- do.call(rbind, rpg_out)
writeVector(rpg_all, file.path(outfolder, "rpg_fields.gpkg"), overwrite = TRUE)
