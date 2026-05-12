# Get agricultural plots around the field coordinates
# considering a buffer and the year of the measurement
#
# input:
#   data/raw-data/coordinates_year_crop.csv
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
period <- 2015:2024
rpg_layer <- "RPG.XXXX:parcelles_graphiques"
colRPG <- c("id_parcel", "code_cultu", "code_group")

# directory to save raw RPG data from happign
happignfolder <- file.path(datafolder, "happign")
# directory to save simplified and aggregated RPG data
# rpgfolder <- file.path(outfolder, "rpg")

# size of buffers
buffer_fields <- c(500, 1000, 1500) #in m
# time period for crop rotation
years <- 5

# remove non agricultural fields from RPG
rmCat <- c("BFP", "BFS", "BOR", "BTA", "SNA")
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
    # transform as terra SpatVect object
    rpgi <- vect(rpgi)

    # save (in case api doesn't work anymore)
    writeVector(rpgi, outi)
  }

  # remove non agricultural fields
  rpgi <- rpgi[!rpgi$code_cultu %in% rmCat, ]

  # continue only if some fields in rpg
  if (nrow(rpgi) > 0) {
    # calculate geometrical characteristics
    rpgi$Perim_m <- perim(rpgi)
    rpgi$Area_ha <- expanse(rpgi) * 0.0001

    ##
    # ponctual information on the site
    cat(".")
    rei <- relate(rpgi, pti, "intersects")
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
      lab <- paste0("Mean_fieldsize_", f, "m_ha")
      buf_pts <- buffer(pti, f)
      rpg_buf <- relate(rpgi, buf_pts, "intersects")
      out_i[, lab] <- mean(rpgi$Area_ha[rpg_buf], na.rm = TRUE)
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
        if ((pti$Year - y) %in% period) {
          rpgy <- get_wfs(
            x = st_as_sf(pti),
            layer = gsub("XXXX", pti$Year - y, rpg_layer)
          )

          if (nrow(rpgy) > 0) {
            rpgy <- vect(rpgy)
            rey <- relate(rpgy, pti, "intersects")
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
