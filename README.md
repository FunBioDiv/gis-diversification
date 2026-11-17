# gis-diversification

Research compendium for extracting spatial crop diversification metrics on 
France and Switzerland field samplings.   


## General

This repository is structured as follow:

- :file_folder: &nbsp;`analyses/`: contains R scripts to extract the diversification metrics;
- :file_folder: &nbsp;`data/`: contains raw and derived data;
- :file_folder: &nbsp;`docs/`: contains the [online data exploration](https://funbiodiv.github.io/gis-diversification/) files rendered automatically (do not modify the content of this folder);
- :file_folder: &nbsp;`figure/`: contains figures made during exploration stages and workshops;
- :file_folder: &nbsp;`notes/`: contains group documents and notes;
- :file_folder: &nbsp;`R/`: contains home made functions for the analysis;

> Raw data are stored in the Nextcloud repository WP2/gis-diversification/data/


## Usage

### Extraction of diversification metrics

The analysis is divided in five sequential steps:  

1. combine and clean sampling coordinates ([01_get_field_coordinates.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/01_get_field_coordinates.R))
2. extract crop field characteristics and crop rotation ([02a_get_field_metrics_FR.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/02a_get_field_metrics_FR.R) and [02b_get_field_metrics_CH.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/02b_get_field_metrics_CH.R))
3. extract land cover in a buffer around the samplings ([03_get_cover_metrics.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/03_get_cover_metrics.R))
4. extract hedgerows density around the fields ([04_get_hedgerows.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/04_get_hedgerows.R))
5. merge extracted metrics into a single database ([05_merge_metrics.R](https://github.com/FunBioDiv/gis-diversification/blob/main/analysis/05_merge_metrics.R))  

These five steps will be run automatically with this command in R (approx. 20 minutes): 

```r
source("make.R")
```

### Interactive data visualizations

Two interactive data visualization tools were developed with Quarto: 

- [Overview of the field coordinates](https://funbiodiv.github.io/gis-diversification/analysis/Q1_overview_fields.html):

```r
quarto::quarto_render("analysis/Q1_overview_fields.qmd")
```

- [Summary of the diversification metrics](https://funbiodiv.github.io/gis-diversification/analysis/Q2_summary_metrics.html): 

```r
quarto::quarto_render("analysis/Q2_summary_metrics.qmd")
```


## Material and method

To characterize the cropping practices, we used the [Registre Parcellaire Graphique](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_RPG) (RPG) for France and the [Geodienste - nutzungsflaechen](https://geodienste.ch/services/lwb_nutzungsflaechen) (Nutzung) for Switzerland.
The two datasets are vectorial spatial datasets that provide crop field boundaries and categorize crops into precise categories: 366 crops are considered in France, and 146 in Switzerland. RPG is available annually since 2015 and Nutzung since 2019 (*to be confirmed*). Because RPG is known to lack data for small vineyards and orchards (*ref needed?*), we complemented the crop data with local datasets for the samples made in regions dominated by perennial cropping systems.

We intersected the sampling coordinates with the national crop dataset corresponding to the sampling year and extracted the perimeter and the area of the fields. Additionally, we calculated the mean field size within a buffer of 1km around the sampling position. When available, we also extracted information about the crops in the four years before sampling using national crop dataset from previous years.

We complemented the crop dataset with the land-use/land-cover data from [Corine Land Cover plus](https://land.copernicus.eu/en/products/clc-backbone) (CLCplus). The dataset characterizes the land cover into 11 classes and is available for Europe with a spatial resolution of 10 m. There has been three releases: 2018, 2021, and 2023. To estimate the land cover within a 1km buffer around the sampling location, we used preferentially the national crop dataset of the sampling year filled with CLCplus data closest to the year of sampling. We grouped the land cover and crop classes into [57 classes](https://github.com/FunBioDiv/gis-diversification/blob/main/data/derived-data/classes_rpg_nutzung_clc.csv) and calculated the Shannon diversity index of the simplified land cover classes.

Hedgerow density was quantified from national datasets: [BD Haies v2](https://geoservices.ign.fr/bdhaie) for France in 2021 and [Geodienste - nutzungsflaechen](https://geodienste.ch/services/lwb_nutzungsflaechen) for Switzerland in 2023. **or** Hedgerow density was quantify using European dataset (Liu et al. 2023) providing estimates of the cover and the biomass of trees at 30m resolution for 2019. We extracted the length/area of hedgerows within the field and a 10m buffer around them. 

All analyses were performed in *R 4.4.1* (R Core Team 2024) using *terra* (Hijmans et al. 2026) and *exactextract* (Baston 2023) packages. 



## References


> Daniel Baston (2023). _exactextractr: Fast Extraction from Raster Datasets using Polygons_. R package version 0.10.0,  <https://CRAN.R-project.org/package=exactextractr>.
>
> Hijmans R, Brown A, Barbosa M (2026). _terra: Spatial Data Analysis_. R package version 1.9-1, <https://CRAN.R-project.org/package=terra>.
>
> Institut national de l'information géographique et forestière. (2024). BD Haie v2: Couche nationale de référence des haies linéaires en France métropolitaine. Last accessed October 22nd 2026, from <https://geoservices.ign.fr/bdhaie>
>
> Institut national de l'information géographique et forestière. (2025). Registre Parcellaire Graphique (RPG) Version 2.0 - Millésime 2015-2024. Last accessed October 22nd 2026, from <https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_RPG>
>
> Konferenz der kantonalen Geoinformations- und Katasterstellen. (2025). Nutzungsflächen (Landwirtschaftliche Bewirtschaftung) - MGDM ID 153.1, Datenstand 2020-2025. Last accessed October 22nd 2026, from <https://geodienste.ch/services/lwb_nutzungsflaechen>
>
> Liu, S., Brandt, M., Nord-Larsen, T., Chave, J., Reiner, F., Lang, N., ... & Fensholt, R. (2023). The overlooked contribution of trees outside forests to tree cover and woody biomass across Europe. Science Advances, 9(37), eadh4097. <https://doi.org/10.1126/sciadv.adh4097>
>
> R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.  <https://www.R-project.org/>.




<br> <br> <br> <br>


### *DEPRECATED*: Exploration of the French dataset

> Intially, the exploration was carried out for France only, and the 
> resulting methodollogical choices were different.

```r
# 1. exploration des données RPG, RPG complété et BD haies  
quarto::quarto_render("analysis/01_explore_points.qmd")

# 2. exploration du croisement RPG-OSO
quarto::quarto_render("analysis/02_land_cover.qmd")

# 3. exploration des tailles de parcelles
quarto::quarto_render("analysis/03_field_size.qmd")

# 4. exploration de la densité de bordures
quarto::quarto_render("analysis/04_bordure.qmd")

# 5. summary Fr 
quarto::quarto_render("analysis/05_summary.qmd")

# 6. comparaison des données sur les haies
quarto::quarto_render("analysis/06_haie.qmd")

# 7. identification des parcelles vers Toulouse 
quarto::quarto_render("analysis/07_fields_Toulouse.qmd")
```