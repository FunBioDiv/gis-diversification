# Load home made functions
devtools::load_all()
library(vegan)
datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
dim(df) # 2027 observation


# field metrics
rpg <- read.csv(file.path(outfolder, "metrics_rpg.csv"))
nutz <- read.csv(file.path(outfolder, "metrics_nutz.csv"))

# get reference
ref <- read.csv(file.path(outfolder, "rpg_nutzung_clc.csv"))

col <- c(
  "Perim_m",
  "Area_ha",
  "Median_fieldsize_500m_ha",
  "Median_fieldsize_1000m_ha"
)
m_rpg <- match(df$ID, rpg$ID)
m_nutz <- match(df$ID, nutz$ID)

for (i in col) {
  fri <- rpg[m_rpg, i]
  chi <- nutz[m_nutz, i]
  df[, i] <- ifelse(df$Study_ID %in% "PestiRed", chi, fri)
}

# crop rotation
code_fr <- rpg$id_cultu[m_rpg]
crop_fr <- ref$name[match(code_fr, ref$id)]
code_ch <- nutz$id_cultu[m_nutz]
crop_ch <- nutz$nutzung_fr[m_nutz]
df$CropCode_N <- ifelse(df$Study_ID %in% "PestiRed", code_ch, code_fr)
df$CropName_N <- ifelse(df$Study_ID %in% "PestiRed", crop_ch, crop_fr)
df$CropGroup_N <- ref$name_grp2[match(df$CropCode_N, ref$id)]
# table(df$Crop_N, useNA = "ifany") # 465 NA
# sum(!is.na(crop_fr)) + sum(!is.na(crop_ch)) == sum(!is.na(df$Crop_N))

# time period for crop rotation
years <- 4
labY <- "id_cultu_N.X"
for (y in 1:years) {
  cody_fr <- rpg[m_rpg, gsub("X", y, labY)]
  cody_ch <- nutz[m_nutz, gsub("X", y, labY)]
  # crpy_ch <- ref$name[match(cody, ref$id)]
  # crpy_fr <- ref$name[match(cody, ref$id)]
  # select rpg or nutz
  cody <- ifelse(df$Study_ID %in% "PestiRed", cody_ch, cody_fr)
  df[, gsub("X", y, "CropCode_NX")] <- cody
  df[, gsub("X", y, "CropName_NX")] <- ref$name[match(cody, ref$id)]
  df[, gsub("X", y, "CropGroup_NX")] <- ref$name_grp2[match(cody, ref$id)]
}
# table(df$CropGroup_N1, useNA = "ifany") # 105 NA
# sum(!is.na(crop1_fr)) + sum(!is.na(crop1_ch)) == sum(!is.na(df$Crop_N1))

# hedgerow metrics
hedge <- read.csv(file.path(outfolder, "metrics_hedgerows.csv"))
col <- !names(hedge) %in% names(df)
m_hedge <- match(df$ID, hedge$ID)

df <- cbind(df, hedge[m_hedge, col])

# lulc metrics
lulc <- read.csv(file.path(outfolder, "metrics_lulc.csv"))
ref <- read.csv(file.path(outfolder, "rpg_nutzung_clc.csv"))

codeC <- gsub("frac1000_", "", names(lulc)[-1])
# if not transformation
labC <- paste(names(lulc)[-1], ref$name[match(codeC, ref$id)], sep = "_")
# names(lulc)[-1] <- labC

grpC <- ref$name_grp2[match(codeC, ref$id)]

lulc_gp <- t(rowsum(t(lulc[, -1]), grpC, na.rm = TRUE))
lulc_gp <- as.data.frame(lulc_gp)
colnames(lulc_gp) <- paste0("frac1000_", names(lulc_gp))
lulc_gp$shannon_1000 <- round(diversity(lulc_gp * 100, "shannon"), 5)

m_lulc <- match(df$ID, lulc$ID)
df <- cbind(df, lulc_gp[m_lulc, ])


# apply(is.na(df), 2, sum)
# export indicators
write.csv(df, file.path(outfolder, "metrics.csv"), row.names = FALSE)
