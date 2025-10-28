library(here)
zipfolder <- here("data", "zip")
zipfile <- list.files(zipfolder, ".zip$")
# process only d35 files
zipfile <- zipfile[grep("_35.zip$", zipfile)]

for (i in seq_along(zipfile)) {
  yri <- substr(zipfile[i], 5, 8)
  unzip(here(zipfolder, zipfile[i]), exdir = here("temp"))
  fi <- list.files(here("temp"), recursive = TRUE)
  fi <- fi[grep("DATA/departement_", fi)]

  outfi <- gsub("^DATA/", paste0("OSO_", yri, "_"), fi)
  # copy and rename
  file.copy(
    from = file.path("temp", fi),
    to = file.path("data", "raw-data", outfi),
    overwrite = TRUE
  )
  # remove temporary files
  file.remove(file.path("temp", list.files(here("temp"), recursive = TRUE)))
}
