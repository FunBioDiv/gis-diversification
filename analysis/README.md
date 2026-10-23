# gis-diversification/analysis

## General

This folder contains all the R-scripts to :  

1. prepare the dataset (PX_*.R)
2. extract the metrics (0X_*.R)
3. interactively visualize the data (QX_*.qmd) 
4. additionnal checks (check_*.*)

### Dataset preparation

These scripts are stored for archive only. They are not supposed to be run again because all downloaded data are stored in the data folder (on Nextcloud).

- P1_get_CLCplus.R: download CLCplus data from WEKEO platform and format it as needed
- P2_prep_CH_data.R: pre-process the Nutzung data received from Selma to simplify subsequent analysis
- P3_list_classes.R: list all classes in RPG, Nutzung and CLCplus to prepare harmonization step

## Metrics extraction

- 01_get_field_coordinates.R: prepare and merge the field sampling coordinates
- 02a_get_field_metrics_FR.R: get the metrics on field size for French data
- 02b_get_field_metrics_CH.R: get the metrics on field size for Swiss data
- 03_get_cover_metrics.R: compute the land cover statistics within buffer
- 04_get_hedgerows.R: calculate the hedgerow density on fields
- 05_merge_metrics.R: merge all diversification metrics


## Interactive visualization

- Q1_overview_fields.qmd: visualize the field coordinates on a map
- Q2_summary_metrics.qmd: summary of the metrics extracted

## Extra checks

**These scripts are stored for archive only.** Each script answers a specific question that was raised during the analysis and was solved afterward.

- check_missing_nutzung.qmd: check the missing fields on Nutzung
- check_vergervignes.R: check the covers for vineyards and orchards
- check_ws3: additional graphs made for discussion during WS3
