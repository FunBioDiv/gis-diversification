devtools::load_all()

rpg21 <- vect("data/raw-data/RPG/RPG_2-0_2021_PARCELLES_GRAPHIQUES.gpkg")

rvi21 <- rpg21[rpg21$CODE_CULTU == "VRC", ]
nrow(rvi21) # 479567
AreaRPG <- expanse(rvi21) * 0.0001

cvi21 <- vect(
  "data/raw-data/VergersVignes/CVI_Parcelles_France_Entiere_2021.gpkg"
)
nrow(cvi21) # 1266500
AreaCVI <- expanse(cvi21) * 0.0001

ocs18 <- vect(
  "data/raw-data/VergersVignes/OCS_2018_19_20_21_VF.shp"
)
# select only the vineyards
ocs18 <- ocs18[ocs18$OCS_20_21 == "Vigne"]
nrow(ocs18) # 4973
AreaOCS <- expanse(ocs18) * 0.0001


df <- data.frame(
  "db" = rep(
    c("RPG", "CVI", "OCS"),
    c(length(AreaRPG), length(AreaCVI), length(AreaOCS))
  ),
  "Area" = c(AreaRPG, AreaCVI, AreaOCS)
)

png(
  file = "figure/Vigne_2021_area.png",
  width = 1200,
  height = 1000,
  res = 200
)
par(mar = c(4, 4, 1, 1))
boxplot(df$Area ~ df$db, outline = FALSE, xlab = "Data source", ylab = "Area")
dev.off()


vergers <- vect(
  "data/raw-data/VergersVignes/vergers_anonyme.gpkg"
)
nrow(vergers) # 9882
AreaVergers <- expanse(vergers) * 0.0001

rve21 <- rpg21[rpg21$CODE_CULTU == "VRG", ]
nrow(rve21) # 73130
AreaRPG <- expanse(rve21) * 0.0001

df <- data.frame(
  "db" = rep(
    c("RPG", "BVD"),
    c(length(AreaRPG), length(AreaVergers))
  ),
  "Area" = c(AreaRPG, AreaVergers)
)

png(
  file = "figure/Vergers_2021_area.png",
  width = 1200,
  height = 1000,
  res = 200
)
par(mar = c(4, 4, 1, 1))
boxplot(df$Area ~ df$db, outline = FALSE, xlab = "Data source", ylab = "Area")
dev.off()
