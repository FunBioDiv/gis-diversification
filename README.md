# gis-diversification

Extraction des métriques de diversification depuis les coordonnées des sites de prélèvements.

Les données sont sauvegardées dans le répertoire Nextcloud WP2/gis-diversification/data

Pour le moment, ce sont seulement des documents exploratoires: 

1. exploration des données RPG, RPG complété et BD haies  
```r
quarto::quarto_render("analysis/01_explore_points.qmd")
```

2. exploration du croisement RPG-OSO  
```r
quarto::quarto_render("analysis/02_land_cover.qmd")
```

3. exploration des tailles de parcelles

```r
quarto::quarto_render("analysis/03_field_size.qmd")
```