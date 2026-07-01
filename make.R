#' gis-diversification: A Research Compendium for
#'
#' @description
#' extract diversification metrics on crop fields
#'
#' @author Romain Frelat
#' @date 1st July 2026

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
# 1 Get metrics from field data
# from the French RPG
source("analysis/F01_get_field_metrics_FR.R") # 3mins or 3h if nothing downloaded
# from the Swiss Nutzung
source("analysis/F01_get_field_metrics_CH.R") #1 min

##
# 2 Get metrics for land cover
source("analysis/F02_get_cover_metrics.R")
# 15 minutes when everything recomputed, else 3mins
# takes longer now with vineFR to be checked for each points

##
# 3 Get hedgerow statistics
source("analysis/F03_get_hedgerows.R")

##
# 4 Merge all indicators
source("analysis/F04_merge.R")

##
# 5 Explore dataset
quarto::quarto_render("analysis/10_summary_FRCH.qmd")
