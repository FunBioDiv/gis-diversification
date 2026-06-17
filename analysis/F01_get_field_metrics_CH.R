# Get agricultural plots around the fields coordinates
# considering a buffer and the year of the measurement
#
# input:
#   data/raw-data/coordinates_year_crop.csv
#   data/raw-data/CH/nutz_20XX.gpkg (2019-2023)
#   data/raw-data/classes_rpg_nutzung_clc.csv homogenized classes of RPG, Nutzung and CLCplus
# output:
#   data/derived-data/metrics_rpg.csv (metrics)
#   data/derived-data/nutz_fields.gpkg (spatial polygons of crop fields)
#
# For the Swiss dataset, we use data received from Selma Cadot on 18/02/2026
#   updated for 2024-2025 in data received on 20/05/2026 (not yet)
# Run in ~1min

library(terra)
library(here)

# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# The data was split per year using S01_prep_data.R
nutz_layer <- "nutz_XXXX.gpkg"
colNUTZ <- c("nutzungsidentifikator", "nutzung_fr", "id_cultu") #, "Hauptkategorie_FR")
# do we want to keep RPG column names?
# labRPG <- c("id_parcel", "code_cultu", "id_cultu")

# data are available for the period 2019 - 2023
period <- 2019:2023
# size of buffers
buffer_fields <- c(500, 1000) #in m
# time period for crop rotation
years <- 4

# get crop reference
ref <- read.csv(file.path(outfolder, "classes_rpg_nutzung_clc.csv"))

rmCat <- sort(unique(ref$name[ref$keep_ag == "0" & ref$source == "nutzung"]))
# rmCat <- c(
#   "Forêt",
#   "Haies, bosquets et berges boisées ", #space is important !
#   "Surfaces en dehors de la SAU"
# )

rot <- readxl::read_xlsx(
  file.path(datafolder, "CH", "PestiRed_17to19_FunBioDiv.xlsx")
)
# all nutzung are in ref :)
# all(rot$nutzung_fr %in% ref$name[ref$source == "nutzung"])
rot$id_cultu <- ref$id[match(rot$nutzung_fr, ref$name)]
# create plot ID, temoin = 1, innovante = 2
rot$Plot_ID <- paste0(
  rot$ID_CODE,
  ".",
  ifelse(rot$parcelle == "innovante", 2, 1)
)
rot$nutzungsidentifikator <- "rotation1719"

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")
# project in EPSG 2056
pts <- project(pts, "EPSG:2056")

# select only observation in Switzerland
keep <- pts$Study_ID %in% "PestiRed" & !is.na(df$Lat)
# table(keep) # 203 points
# table(pts$Year[keep])

# table(nutz$Hauptkategorie_FR, useNA="ifany")
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
  nutzi <- nutzi[!nutzi$nutzung_fr %in% rmCat, ]

  nutzi$id_cultu <- ref$id[match(nutzi$nutzung_fr, ref$name)]
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
      lab <- paste0("Median_fieldsize_", f, "m_ha")
      buf_pts <- terra::buffer(pti, f)
      nutz_buf <- terra::relate(nutzi, buf_pts, "intersects")
      out_i[, lab] <- median(nutzi$Area_ha[nutz_buf], na.rm = TRUE)
    }

    ##
    # crop rotation
    cat(".")
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
          rey <- terra::relate(nutzy, pti, "intersects")
          if (sum(rey) > 1) {
            nutzy <- nutzi[which(rey)[1], ]
            # add information in out_i
            out_i[, lab] <- data.frame(nutzy)[, colNUTZ]
          }
        }
      }
      if ((pti$Year - y) %in% 2017:2019) {
        sel_rot <- rot$Plot_ID %in% pti$Plot_ID & rot$annee %in% (pti$Year - y)
        if (sum(sel_rot) >= 1) {
          out_i[, lab] <- data.frame(rot[which(sel_rot)[1], colNUTZ])
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
#fmt: skip
writeVector(nutz_all, file.path(outfolder, "nutz_fields.gpkg"), overwrite = TRUE)
