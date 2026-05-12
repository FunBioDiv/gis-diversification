# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")

# get the coordinates from the points
# from shinyFunbiodiv/analysis/03_update_data.R
df <- read.csv(here(datafolder, "coordinates_year_crop.csv"))
# create an ID
df$ID <- paste(df$Study_ID, df$Plot_ID, df$Year, sep = "_")
dim(df)


# field metrics
rpg <- read.csv(file.path(outfolder, "metrics_rpg.csv"))
ref_rpg <- read.csv(file.path(outfolder, "rpg_classes.csv"))
nutz <- read.csv(file.path(outfolder, "metrics_nutz.csv"))

col <- c(
  "Perim_m",
  "Area_ha",
  "Mean_fieldsize_500m_ha",
  "Mean_fieldsize_1000m_ha",
  "Mean_fieldsize_1500m_ha"
)
m_rpg <- match(df$ID, rpg$ID)
m_nutz <- match(df$ID, nutz$ID)

for (i in col) {
  fri <- rpg[m_rpg, i]
  chi <- nutz[m_nutz, i]
  df[, i] <- ifelse(df$Study_ID %in% "PestiRed", chi, fri)
}

# crop rotation
code_fr <- rpg$code_cultu[m_rpg]
crop_fr <- ref_rpg$libelle.culture[match(code_fr, ref_rpg$code.culture)]
crop_ch <- nutz$nutzung_fr[m_nutz]
df$Crop_N <- ifelse(df$Study_ID %in% "PestiRed", crop_ch, crop_fr)
# table(df$Crop_N, useNA = "ifany") # 465 NA
# sum(!is.na(crop_fr)) + sum(!is.na(crop_ch)) == sum(!is.na(df$Crop_N))

code1 <- rpg$code_cultu_N.1[m_rpg]
crop1_fr <- ref_rpg$libelle.culture[match(code1, ref_rpg$code.culture)]
crop1_ch <- nutz$nutzung_fr_N.1[m_nutz]
df$Crop_N1 <- ifelse(df$Study_ID %in% "PestiRed", crop1_ch, crop1_fr)
table(df$Crop_N1, useNA = "ifany") # 542 NA
# sum(!is.na(crop1_fr)) + sum(!is.na(crop1_ch)) == sum(!is.na(df$Crop_N1))

code2 <- rpg$code_cultu_N.2[m_rpg]
crop2_fr <- ref_rpg$libelle.culture[match(code2, ref_rpg$code.culture)]
crop2_ch <- nutz$nutzung_fr_N.2[m_nutz]
df$Crop_N2 <- ifelse(df$Study_ID %in% "PestiRed", crop2_ch, crop2_fr)
table(df$Crop_N2, useNA = "ifany") # 686 NA

code3 <- rpg$code_cultu_N.3[m_rpg]
crop3_fr <- ref_rpg$libelle.culture[match(code3, ref_rpg$code.culture)]
crop3_ch <- nutz$nutzung_fr_N.3[m_nutz]
df$Crop_N3 <- ifelse(df$Study_ID %in% "PestiRed", crop3_ch, crop3_fr)
table(df$Crop_N3, useNA = "ifany") # 812 NA
# sum(!is.na(crop1_fr)) + sum(!is.na(crop1_ch)) == sum(!is.na(df$Crop_N1))

code4 <- rpg$code_cultu_N.4[m_rpg]
crop4_fr <- ref_rpg$libelle.culture[match(code3, ref_rpg$code.culture)]
crop4_ch <- nutz$nutzung_fr_N.4[m_nutz]
df$Crop_N4 <- ifelse(df$Study_ID %in% "PestiRed", crop4_ch, crop4_fr)
table(df$Crop_N4, useNA = "ifany") # 837 NA

# hedgerow metrics
hedge <- read.csv(file.path(outfolder, "metrics_hedgerows.csv"))
col <- !names(hedge) %in% names(df)
m_hedge <- match(df$ID, hedge$ID)

df <- cbind(df, hedge[m_hedge, col])

# lulc metrics
lulc <- read.csv(file.path(outfolder, "metrics_lulc.csv"))
ref <- read.csv(file.path(outfolder, "rpg_nutzung_clc_classes.csv"))

codeC <- gsub("frac1500_", "", names(lulc)[-1])
labC <- paste(names(lulc)[-1], ref$name[match(codeC, ref$id)], sep = "_")
names(lulc)[-1] <- labC

m_lulc <- match(df$ID, lulc$ID)
df <- cbind(df, lulc[m_lulc, -1])

head(df)
dim(df) # 2027, 377

# apply(is.na(df), 2, sum)
# export indicators
write.csv(df, file.path(outfolder, "metrics.csv"), row.names = FALSE)
