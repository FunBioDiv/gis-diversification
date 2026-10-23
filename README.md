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
2. extract crop field characteristics and crop rotation
3. extract land cover in a buffer around the samplings
4. extract hedgerows density around the fields
5. merge extracted metrics into a single database   

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
The two datasets are vectorial spatial data that provide the crop field boundaries and categorize the crops into refined categories: 366 crops are considered in France, and 146 in Swizerland. RPG is available annually since 2015 while Nutzung is available since 2019. Because RPG is known to lack data for small wineyards and orchards (*ref needed?*), we complemented the crop data with local dataset for the samples made in regions dominated by permanent crops.

We intersected the sampling coordinates with the national crop dataset corresponding to the sampling year and extracted the perimeter and the area of the fields. Additionnally, we calculated the mean field size within a buffer of 1km around the sampling position. When available, we exctracted information about the cropping patterns for the 4 years before sampling.

We complemented the crop dataset with the land use land cover data from [Corine Land Cover plus](https://land.copernicus.eu/en/products/clc-backbone) (CLCplus). It caracterize the land cover into 11 classes. The dataset is available for Europe with a spatial resolution of 10 m. There has been three release: 2018, 2021, and 2023. We filled the holes of the national crop dataset with the  CLCplus information closest to the year of sampling. Then, we extracted the percentage of cover per buffer of 1km around the sampling location. We grouped the land cover and crop classes into [57 classes](https://github.com/FunBioDiv/gis-diversification/blob/main/data/derived-data/classes_rpg_nutzung_clc.csv) and calculated the Shannon index of the simplified land cover classes.

Hedgerow density was quantify from national datasets: [BD Haies v2](https://geoservices.ign.fr/bdhaie) for France in 2021 and [Geodienste - nutzungsflaechen](https://geodienste.ch/services/lwb_nutzungsflaechen) for Switzerland in 2023. **or** Hedgerow density was quantify using European dataset (Liu et al. 2023) providing estimates of the cover and the biomass of trees at 30m resolution for 2019. We extracted the length/area of hedgerows within the fieldw and a 10m buffer around them. 

All analysis were performed in R 4.4.1() using terra() and exactextract() packages. 

## Results

In total, 95 % of the fields samplings are covered with information on crop field, and 73% of the samples (N=1682) have complete 5 years crop rotation data.


## References


Daniel Baston (2023). _exactextractr: Fast Extraction from Raster Datasets using Polygons_. R package version 0.10.0,  <https://CRAN.R-project.org/package=exactextractr>.

Hijmans R, Brown A, Barbosa M (2026). _terra: Spatial Data Analysis_. R package version 1.9-1, <https://CRAN.R-project.org/package=terra>.

Institut national de l'information géographique et forestière. (2024). BD Haie v2: Couche nationale de référence des haies linéaires en France métropolitaine. Last accessed October 22nd 2026, from <https://geoservices.ign.fr/bdhaie>

Institut national de l'information géographique et forestière. (2025). Registre Parcellaire Graphique (RPG) Version 2.0 - Millésime 2015-2024. Last accessed October 22nd 2026, from <https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_RPG>

Konferenz der kantonalen Geoinformations- und Katasterstellen. (2025). Nutzungsflächen (Landwirtschaftliche Bewirtschaftung) - MGDM ID 153.1, Datenstand 2020-2025. Last accessed October 22nd 2026, from <https://geodienste.ch/services/lwb_nutzungsflaechen>

Liu, S., Brandt, M., Nord-Larsen, T., Chave, J., Reiner, F., Lang, N., ... & Fensholt, R. (2023). The overlooked contribution of trees outside forests to tree cover and woody biomass across Europe. Science Advances, 9(37), eadh4097. <https://doi.org/10.1126/sciadv.adh4097>

R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R Foundation for Statistical Computing, Vienna, Austria.
  <https://www.R-project.org/>.


#### Update the index

```r
quarto::quarto_render("index.qmd")
```

<br> <br> <br> <br>


### *DEPRECATED*: Exploration of the French dataset

> Intially, the exploration was carried out for France only, and the 
> resulting methodollogical choice were different.

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