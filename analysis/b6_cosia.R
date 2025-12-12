suppressWarnings({
  library(terra)
  library(sf)
  library(here)
  library(mapview)
  library(leafsync)
})

# Load home made functions
devtools::load_all()

datafolder <- here("data", "raw-data")
outfolder <- here("data", "derived-data")


# load matching rpg data
pts <- vect(here(datafolder, "fields_FR.gpkg"), "fields_FR")
pts$ID <- paste(pts$Study_ID, pts$Site, pts$Year, sep = "_")
rpg_fields <- vect(here("data", "derived-data", "vector_parcels.gpkg"))
indv <- read.csv(here("data", "derived-data", "vector_indicators.csv"))

departments <- vect(here(datafolder, "departement.gpkg"))
pts$Dep <- extract(departments, pts)$code_insee
indv$Dep <- pts$Dep[match(indv$ID, pts$ID)]

### 33
rpg33 <- rpg_fields[
  rpg_fields$ID_PARCEL %in% indv$ID_PARCEL[indv$Dep == "33"],
]

rpg33_buff10 <- buffer(rpg33, 10)
rpg33_buff10 <- rpg33_buff10[!duplicated(rpg33_buff10$ID_PARCEL)]

cos33 <- here("data", "poc", "COSIA_1-0__GPKG_LAMB93_D033_2021-01-01")
cos33f <- list.files(
  cos33,
  pattern = "gpkg",
  recursive = TRUE,
  full.names = TRUE
)
lfiles <- substr(cos33f, nchar(cos33f) - 18, nchar(cos33f) - 11)


out <- c()
for (i in seq_along(rpg33_buff10)) {
  if (i %% 10 == 0) {
    print(i)
  }
  bi <- rpg33_buff10[i]
  x <- unique(trunc(ext(bi)[1:2] / 10000) * 10)
  y <- unique(trunc(ext(bi)[3:4] / 10000) * 10) + 10
  lseq <- paste(rep(x, length(y)), rep(y, each = length(x)), sep = "_")
  cosfi <- cos33f[lfiles %in% lseq]
  vlist <- lapply(cosfi, vect)
  cosi <- do.call(rbind, vlist)
  cosbi <- crop(cosi, bi)
  fori <- cosbi[cosbi$classe == "Feuillu", ]
  out <- rbind(out, c(bi$ID_PARCEL, sum(expanse(fori)) * 0.0001))
}

out <- as.data.frame(out)
names(out) <- c("ID_PARCEL", "Cosia_Feuillu_Area_ha")
write.csv(out, here("data/derived-data/Cosia_d33.csv"), row.names = FALSE)


### 35
rpg35 <- rpg_fields[
  rpg_fields$ID_PARCEL %in% indv$ID_PARCEL[indv$Dep == "35"],
]

rpg35_buff10 <- buffer(rpg35, 10)
rpg35_buff10 <- rpg35_buff10[!duplicated(rpg35_buff10$ID_PARCEL)]

cos35 <- here("data", "poc", "COSIA_1-0__GPKG_LAMB93_D035_2020-01-01")
cos35f <- list.files(
  cos35,
  pattern = "gpkg",
  recursive = TRUE,
  full.names = TRUE
)
lfiles <- substr(cos35f, nchar(cos35f) - 18, nchar(cos35f) - 11)


out <- c()
for (i in seq_along(rpg35_buff10)) {
  if (i %% 10 == 0) {
    print(i)
  }
  bi <- rpg35_buff10[i]
  x <- unique(trunc(ext(bi)[1:2] / 10000) * 10)
  y <- unique(trunc(ext(bi)[3:4] / 10000) * 10) + 10
  lseq <- paste(rep(x, length(y)), rep(y, each = length(x)), sep = "_")
  cosfi <- cos35f[lfiles %in% lseq]
  vlist <- lapply(cosfi, vect)
  cosi <- do.call(rbind, vlist)
  cosbi <- crop(cosi, bi)
  fori <- cosbi[cosbi$classe == "Feuillu", ]
  out <- rbind(out, c(bi$ID_PARCEL, sum(expanse(fori)) * 0.0001))
}

out <- as.data.frame(out)
names(out) <- c("ID_PARCEL", "Cosia_Feuillu_Area_ha")
write.csv(out, here("data/derived-data/Cosia_d35.csv"), row.names = FALSE)
