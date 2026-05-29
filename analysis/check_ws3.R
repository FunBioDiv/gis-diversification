# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")
fig_folder <- here("figure")
df <- read.csv(file.path(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
# transform as spatial points
pts <- vect(df, geom = c("Long", "Lat"), crs = "EPSG:4326")

# raster indicators
ind <- read.csv(file.path(outfolder, "metrics.csv"))

# classes
ref <- read.csv(file.path(outfolder, "rpg_nutzung_clc_classes.csv"))

## Land cover ---------------------------------------

# get cover classes
m1500 <- grep("frac1500", names(ind))
l1500 <- gsub("frac1500_", "", names(ind)[m1500])
n1500 <- as.numeric(substr(l1500, 1, regexpr("_", l1500) - 1))

# how much is covered by Low-growing woody plants, Permanent herbaceous, Periodically herbaceous
keepC <- c(1005, 1006, 1007)

unwanted <- rowSums(ind[, m1500[n1500 %in% keepC]], na.rm = TRUE)
png(
  file = file.path(fig_folder, "Unwanted_clc.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  unwanted ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "Hors foret - bati - eau et RPG*"
)
dev.off()


# who has verger?
mverger <- c(
  grep("verger", tolower(names(ind))), #rpg
  grep("cultures.fruitieres", tolower(names(ind))) #nutzung
)

verger <- rowSums(ind[, mverger], na.rm = TRUE)
png(
  file = file.path(fig_folder, "Verger_RPG.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  verger ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "Verger"
)
dev.off()

# rmcat <- c(1001, 1002, 1003, 1004, 1009, 1010)
# non_ag <- rowSums(ind[, m1500[n1500 %in% rmcat]], na.rm = TRUE)
# boxplot(
#   100 - non_ag - in_rpg ~ ind$Study_ID,
#   las = 2,
#   ylab = "%",
#   xlab = "",
#   main = "Hors foret - bati - eau et RPG*"
# )

in_rpg <- rowSums(ind[, m1500[n1500 < 1000]], na.rm = TRUE)

png(
  file = file.path(fig_folder, "In_RPG.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  in_rpg ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "RPG*"
)
dev.off()


png(
  file = file.path(fig_folder, "1007_Periodically.herbaceous.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  ind$frac1500_1007_Periodically.herbaceous ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "Periodically.herbaceous"
)
dev.off()

png(
  file = file.path(fig_folder, "1006_Permanent.herbaceous.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  ind$frac1500_1006_Permanent.herbaceous ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "Permanent.herbaceous"
)
dev.off()

png(
  file = file.path(fig_folder, "1005_Low.growing.woody.plants.png"),
  width = 1200,
  height = 1000,
  res = 150
)
par(mar = c(12, 4, 1, 1))
boxplot(
  ind$frac1500_1005_Low.growing.woody.plants ~ ind$Study_ID,
  las = 2,
  ylab = "%",
  xlab = "",
  main = "Low.growing.woody.plants"
)
dev.off()

## Hedgerow ---------------------------------------
# compare the hedgrows
haie <- ind[, grep("haie", names(ind))]
names(haie) <- c(
  "liu_cover\nperc",
  "liu_biomass\nkg",
  "liu_density\nkgperha",
  "nutzung_area\nha",
  "nutzung_cover\nperc",
  "nutzung_length\nm",
  "bdhaie_length\nm",
  "bdhaie_area\nha",
  "bdhaie_cover\nperc"
)
png(
  file = file.path(fig_folder, "Haies.png"),
  width = 1500,
  height = 1500,
  res = 200
)
pairs(haie, lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off()


tapply(ind$haie_bdhaie_length_m > 0, ind$Study_ID, sum, na.rm = TRUE) /
  table(ind$Study_ID) *
  100

# PestiRed is incomplete with hedgerows
tapply(ind$haie_nutzung_ha > 0, ind$Study_ID, sum, na.rm = TRUE) /
  table(ind$Study_ID) *
  100
