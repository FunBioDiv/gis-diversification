# Get all plots around the fields coordinates in a buffer
# considering the year of the measurement

# For the French dataset, we will download the interesting part from the API directly
# using happign package: https://paul-carteron.github.io/happign
library(happign)

# Run in ~2h

library(terra)
library(sf)
library(here)

# Load home made functions
devtools::load_all()

# check out which RPG layer is available
# meta_vect <- get_layers_metadata("wfs") # all layers for altimetrie wms
# look_up <- "RPG"
# #fmt: skip
# found <- grepl(tolower(look_up), tolower(meta_vect$Name)) | grepl(tolower(look_up), tolower(meta_vect$Name))
# meta_vect$Name[found]
period <- 2015:2024
rpg_layer <- "RPG.XXXX:parcelles_graphiques"
colRPG <- c("id_parcel", "code_cultu", "code_group")

buffer_fields <- c(500, 1000, 1500) #in m
years <- 5 # for crop rotation

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# remove jachere? J5M, J6P, J6S, JAC, JNO
rmCat <- c("BFP", "BFS", "BOR", "BTA", "SNA")

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# table(pts$Year)

# select only observation in France between 2015 and 2024
keep <- pts$Year %in% period & !pts$Study_ID %in% "PestiRed"
# table(keep) # 1692 points

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
  rpgi <- get_wfs(
    x = st_as_sf(buffi),
    layer = gsub("XXXX", pti$Year, rpg_layer)
  )
  # remove non agricultural fields
  rpgi <- rpgi[!rpgi$code_cultu %in% rmCat, ]

  # continue only if some fields in rpg
  if (nrow(rpgi) > 0) {
    # transform as terra SpatVect object
    rpgi <- vect(rpgi)

    # save (in case api doesn't work anymore)
    outi <- file.path(datafolder, "happign", paste0("RPGbuf_", pti$ID, ".gpkg"))
    writeVector(rpgi, outi, overwrite = TRUE)

    # calculate geometrical characteristics
    rpgi$Perim_m <- perim(rpgi)
    rpgi$Area_ha <- expanse(rpgi) * 0.0001

    ##
    # ponctual information on the site
    cat(".")
    rei <- relate(rpgi, pti, "intersects")
    if (sum(rei) == 1) {
      exi <- rpgi[rei]
      # keep in rpg_out
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
    if (sum(rei) == 1) {
      rpg_time <- exi[, colRPG]
      rpg_time$Year <- pti$Year
    } else {
      rpg_time <- vect()
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
            rpg_time <- rbind(rpg_time, rpgy[c(colRPG, "Year")])
          }
        }
      }
    }
    if (nrow(rpg_time) > 1) {
      outi <- file.path(
        datafolder,
        "happign",
        paste0("RPGrot_", pti$ID, ".gpkg")
      )
      writeVector(rpg_time, outi, overwrite = TRUE)
    }

    df_out <- rbind(df_out, out_i)
  }
}

# export indicators
write.csv(df_out, file.path(outfolder, "metrics_rpg.csv"), row.names = FALSE)

# save rpg fields
rpg_all <- do.call(rbind, rpg_out)
writeVector(rpg_all, file.path(outfolder, "rpg_fields.gpkg"))
