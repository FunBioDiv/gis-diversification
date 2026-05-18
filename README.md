# gis-diversification

Extraction des métriques de diversification depuis les coordonnées des sites de prélèvements.

Les données sont sauvegardées dans le répertoire Nextcloud WP2/gis-diversification/data/

## Exploration of the French dataset

#### 1. exploration des données RPG, RPG complété et BD haies  
```r
quarto::quarto_render("analysis/01_explore_points.qmd")
```

#### 2. exploration du croisement RPG-OSO  
```r
quarto::quarto_render("analysis/02_land_cover.qmd")
```

#### 3. exploration des tailles de parcelles

```r
quarto::quarto_render("analysis/03_field_size.qmd")
```

#### 4. exploration de la densité de bordures
```r
quarto::quarto_render("analysis/04_bordure.qmd")
```

#### 5. summary Fr 
(~5 min to render)
```r
quarto::quarto_render("analysis/05_summary.qmd")
```

#### 6. comparaison des données sur les haies

```r
quarto::quarto_render("analysis/06_haie.qmd")
```

#### 7. identification des parcelles vers Toulouse 

```r
quarto::quarto_render("analysis/07_fields_Toulouse.qmd")
```



## Calculation and summary of French+Swiss metrics

Updated calculation considering France and Switzerland. The file `make.R` run all the calculations while the file `10_summary_FRCH.qmd` provides a summary of the calculated metrics.

```r
# re compute all the indicators
source("make.R")
# or only compute the summary 
quarto::quarto_render("analysis/10_summary_FRCH.qmd")
```


#### Mise à jour de l'index:
```r
quarto::quarto_render("index.qmd")
```