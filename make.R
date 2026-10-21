#' gis-diversification: A Research Compendium for
#'
#' @description
#' extract diversification metrics on crop fields
#'
#' @author Romain Frelat
#' @date 23th October 2026

## Install Dependencies (listed in DESCRIPTION) ----
# rdeps::add_deps() # update automatically the list of dependencies

if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")

## Load Project Addins (R Functions)
devtools::load_all()

## Run Project ---------------------------------------

##
# 1. Get and clean field coordinates
source("analysis/01_get_field_coordinates.R") # < 5 secs

# 2. coordinates metrics from field data
# from the French RPG
source("analysis/02a_get_field_metrics_FR.R") # 3 mins or 3h if download needed
# from the Swiss Nutzung
source("analysis/02b_get_field_metrics_CH.R") # < 3 mins

##
# 3 Get metrics for land cover
source("analysis/03_get_cover_metrics.R")
# 15 minutes when everything recomputed, else 8 mins with vineFR (longer)

##
# 4 Get hedgerow statistics
source("analysis/04_get_hedgerows.R") # 3 mins

##
# 5 Merge all indicators
source("analysis/05_merge_metrics.R") # 10 secs
