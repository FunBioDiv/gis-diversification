# else old school
# https://cartes.gouv.fr/rechercher-une-donnee/dataset/INRAE_GRAIN-BOCAGER
#       https://data.geopf.fr/telechargement/download/INRAE_GRAIN-BOCAGER/GRAIN-BOCAGER_1-0__TIFF_LAMB93_D002_2021-01-01/GRAIN-BOCAGER_1-0__TIFF_LAMB93_D002_2021-01-01.7z
library(here)
url <- "https://data.geopf.fr/telechargement/download/INRAE_GRAIN-BOCAGER/GRAIN-BOCAGER_1-0__TIFF_LAMB93_D0XX_20YY-01-01/GRAIN-BOCAGER_1-0__TIFF_LAMB93_D0XX_20YY-01-01.7z"
tif <- "GRAIN-BOCAGER_1-0__TIFF_LAMB93_D0XX_20YY-01-01/grain_bocager_5m_XX_20YY.tif"
tempdir <- "GRAIN-BOCAGER_1-0__TIFF_LAMB93_D0XX_20YY-01-01"
options(timeout = 10000)
zipdir <- here("data", "zip")
datadir <- here("data", "raw-data", "GrainBocager")
dep <- ifelse(nchar(1:95) == 1, paste0("0", 1:95), 1:95)
try_catch <- function(exprs) {
  !inherits(try(eval(exprs)), "try-error")
}
for (i in dep) {
  outi <- file.path(datadir, paste0("grain_bocager_", i, ".tif"))
  if (!file.exists(outi)) {
    temp <- file.path(zipdir, paste0("grain_bocager_", i, ".7z"))
    yr <- 21
    test <- try_catch(download.file(
      gsub("YY", yr, gsub("XX", i, url)),
      temp,
      mode = "wb"
    ))
    if (!test) {
      yr <- 22
      test <- try_catch(download.file(
        gsub("YY", yr, gsub("XX", i, url)),
        temp,
        mode = "wb"
      ))
    }
    if (!test) {
      yr <- 23
      test <- try_catch(download.file(
        gsub("YY", yr, gsub("XX", i, url)),
        temp,
        mode = "wb"
      ))
    }

    # extract only the grain_bocager_5m_XX_20YY.tif
    archive::archive_extract(
      temp,
      dir = datadir,
      files = gsub("YY", yr, gsub("XX", i, tif))
    )
    # copy and rename
    file.copy(
      from = file.path(datadir, gsub("YY", yr, gsub("XX", i, tif))),
      to = outi
    )
    # remove temporary files
    file.remove(file.path(datadir, gsub("YY", yr, gsub("XX", i, tif))))
    # file.remove(temp) # keep the zip file in case of issue
    # remove temporary repository
    unlink(
      file.path(datadir, gsub("YY", yr, gsub("XX", i, tempdir))),
      recursive = TRUE
    )
  }
}
