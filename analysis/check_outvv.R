library(terra)
# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

ind <- read.csv(file.path(outfolder, "metrics_lulc.csv"))

sel <- grep("BVD", ind$ID)
ind[sel, -1]
ind$ID[sel]
i = which(pts$ID == "EXCLU_BVD_194_2023")
v1 <- vect("data/raw-data/happign/RPGcomp_EXCLU_BVD_194_2023.gpkg")
r1 <- rast("data/derived-data/lulc/LULC_EXCLU_BVD_194_2023.tif")
table(r1$id_cultu)
plot(r1)
plot(project(v1, "EPSG:3035"), add = TRUE)
ind[ind$ID == "EXCLU_BVD_194_2023", ]
ref <- read.csv(file.path(outfolder, "rpg_nutzung_clc.csv"))
ind <- read.csv(file.path(outfolder, "metrics.csv"))
table(is.na(ind$Crop_N))

df <- read.csv(file.path(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")


ind$Crop_N
ind$CropGroup_N <- ref$name_grp2[match(ind$Crop_N, ref$name)]
table(ind$Crop_N, useNA = "ifany")

ind_rpg <- read.csv(file.path(outfolder, "metrics_rpg.csv"))
table(is.na(ind_rpg$id_cultu), ind_rpg$Study_ID)
ind_rpg$code_cultu


ind_rpg$ID[is.na(ind_rpg$id_cultu) & ind_rpg$Study_ID == "BIOMHE"]

"BIOMHE_39_2020"

r1 <- vect("data/raw-data/happign/RPGbuf_BIOMHE_3_2020.gpkg")
p1 <- pts[pts$ID == "BIOMHE_3_2020"]
plot(r1)
plot(p1, col = "red", add = TRUE)

rei <- terra::relate(r1, p1, "intersects")
sum(rei) == 1
exi <- r1[rei]
# add funbiodiv ID
exi$Funbiodiv_ID <- pti$ID
rpg_out <- c(rpg_out, exi)
out_i <- data.frame(
  data.frame(pti),
  data.frame(exi)[, c(colRPG, "Perim_m", "Area_ha")]
)


r2 <- vect("data/raw-data/happign/RPGcomp_BIOMHE_39_2020.gpkg")

table(ind$id_cultu > 400, ind$Study_ID, useNA = "ifany")
