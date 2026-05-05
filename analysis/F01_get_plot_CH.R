# Get all plots around the fields coordinates in a buffer
# considering the year of the measurement

# For the Swiss dataset, we use data received from Selma Cadot
# Run in ~1min

library(terra)
library(sf)
library(here)

# Load home made functions
devtools::load_all()

# The data was split per year using S01_prep_data.R
# data are available for the period 2019 - 2023
period <- 2019:2023
nutz_layer <- "nutz_XXXX.gpkg"
labRPG <- c("id_parcel", "code_cultu", "code_group")
colNUTZ <- c("nutzungsidentifikator", "nutzung_fr", "Hauptkategorie_FR")

buffer_fields <- c(500, 1000, 1500) #in m
years <- 5 # for crop rotation

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# project in EPSG 2056
pts <- project(pts, "EPSG:2056")

# select only observation in Switzerland
keep <- pts$Study_ID %in% "PestiRed" & !is.na(df$Lat)
# table(keep) # 203 points
# table(pts$Year[keep])

# table(nutz$Hauptkategorie_FR, useNA="ifany")
rmCat <- c(
  "Forêt",
  "Haies, bosquets et berges boisées",
  "Surfaces en dehors de la SAU"
)

nutz_out <- c() # save the nutz information
df_out <- c() # save the parameters
# for testing: sample(which(keep), 10)
for (i in which(keep)) {
  cat(i)

  # select point i
  pti <- pts[i]

  # create maximum buffer
  buffi <- buffer(pti, max(buffer_fields))

  # get rpg around buffer
  nutzi <- vect(
    file.path(datafolder, "CH", gsub("XXXX", pti$Year, nutz_layer)),
    ext = buffi
  )

  # remove non agricultural fields
  nutzi <- nutzi[!nutzi$Hauptkategorie_FR %in% rmCat, ]
  # continue only if some fields in nutz
  if (nrow(nutzi) > 0) {
    # calculate geometrical characteristics
    nutzi$Perim_m <- perim(nutzi)
    nutzi$Area_ha <- expanse(nutzi) * 0.0001

    ##
    # ponctual information on the site
    cat(".")
    rei <- terra::relate(nutzi, pti, "intersects")

    if (sum(rei) > 0) {
      exi <- nutzi[which(rei)[1], ]
      # add funbiodiv ID
      exi$Funbiodiv_ID <- pti$ID
      # keep in nutz_out
      nutz_out <- c(nutz_out, exi)
      out_i <- data.frame(
        data.frame(pti),
        data.frame(exi)[, c(colNUTZ, "Perim_m", "Area_ha")]
      )
    } else {
      out_i <- data.frame(
        data.frame(pti),
        matrix(NA, ncol = length(colNUTZ) + 2)
      )
      #fmt: skip
      names(out_i)[(ncol(pti)+1):ncol(out_i)] <- c(colNUTZ, "Perim_m", "Area_ha")
    }

    ##
    # average field size in buffer
    cat(".")
    for (f in buffer_fields) {
      lab <- paste0("Mean_fieldsize_", f, "m_ha")
      buf_pts <- buffer(pti, f)
      nutz_buf <- relate(nutzi, buf_pts, "intersects")
      out_i[, lab] <- mean(nutzi$Area_ha[nutz_buf], na.rm = TRUE)
    }

    ##
    # crop rotation
    cat(".")
    if (sum(rei) > 0) {
      nutz_time <- exi[, colNUTZ]
      nutz_time$Year <- pti$Year
    } else {
      nutz_time <- vect()
    }
    for (y in 1:years) {
      out_i[paste0("N-", y)] <- pti$Year - y
      lab <- paste0(colNUTZ, "_N-", y)
      out_i[, lab] <- NA
      if ((pti$Year - y) %in% period) {
        nutzy <- vect(
          file.path(datafolder, "CH", gsub("XXXX", pti$Year - y, nutz_layer)),
          ext = pti
        )

        if (nrow(nutzy) > 0) {
          rey <- relate(nutzy, pti, "intersects")
          if (sum(rey) > 1) {
            nutzy <- nutzi[which(rey)[1], ]
            # add information in out_i
            out_i[, lab] <- data.frame(nutzy)[, colNUTZ]
            # keep it for save
            nutzy$Year <- pti$Year - y
            nutz_time <- rbind(nutz_time, nutzy[c(colNUTZ, "Year")])
          }
        }
      }
    }
    df_out <- rbind(df_out, out_i)
  }
}

# export indicators
write.csv(df_out, file.path(outfolder, "metrics_nutz.csv"), row.names = FALSE)

# save rpg fields
nutz_all <- do.call(rbind, nutz_out)
writeVector(nutz_all, file.path(outfolder, "nutz_fields.gpkg"))
