# gis-diversification

Research compendium for extracting spatial crop diversification measures on France and Switzerland field samplings.   

Raw data are stored in the Nextcloud repository WP2/gis-diversification/data/

## Calculation and summary of diversification metrics

#### Run the calculations

The file `make.R` run all the metric calculations considering France and Switzerland. You can recompute them with:  

```r
# compute all the indicators
source("make.R")
```

#### Update the data visualizations

[Overview of the field coordinates](https://funbiodiv.github.io/gis-diversification/analysis/Q1_overview_fields.html):

```r
quarto::quarto_render("analysis/Q1_overview_fields.qmd")
```

[Summary of the diversification metrics](https://funbiodiv.github.io/gis-diversification/analysis/Q2_summary_metrics.html): 

```r
quarto::quarto_render("analysis/Q2_summary_metrics.qmd")
```

#### Update the index

```r
quarto::quarto_render("index.qmd")
```

<br> <br> <br> <br>


### *DEPRECATED*: Exploration of the French dataset

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