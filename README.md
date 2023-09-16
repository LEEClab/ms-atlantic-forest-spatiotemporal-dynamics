# The Atlantic Forest of South America: spatiotemporal dynamic of remaining vegetation and implications for conservation

This repository shares the code used on the manuscript by Vancine et al. *in prep*.

## code

All analyses were performed in [R language](https://www.r-project.org/) and [GRASS GIS](https://grass.osgeo.org/) through [*rgrass*](https://rsbivand.github.io/rgrass/) R package. This folder contains all R code files used: 

- `01_01_download_limits.R`: download Atlantic Forest, countries, states, and municipalities limits
- `01_02_download_mapbiomas.R`: download [MapBiomas Brazil v07](https://brasil.mapbiomas.org/) and [MapBiomas Bosque Atlántico v02](https://bosqueatlantico.mapbiomas.org/) LULC rasters
- `01_03_download_roads_railways.R`: download roads and railways from official databases from Brazil ([IBGE](https://www.ibge.gov.br/)), Argentina ([IGN](https://www.ign.gob.ar)) and Paraguay ([INE](https://www.ine.gov.py))
- `01_04_download_protected_areas.R`: download protected areas from [Protected Planet](https://www.protectedplanet.net/en)
- `01_05_download_indigenous_territories.R`: download indigenous territories from official databases from Brazil ([FUNAI](https://www.gov.br/funai/pt-br)) and Paraguay ([Tierras Indígenas](https://www.tierrasindigenas.org))
- `02_01_grassgis_import_limits.R`: import Atlantic Forest, countries, states, and municipalities limits into GRASS GIS            
- `02_02_grassgis_import_mapbiomas.R`: import Mapbiomas LULC rasters  into GRASS GIS
- `02_03_grassgis_import_roads_railways.R`: import roads and railways into GRASS GIS
- `02_04_grassgis_import_protected_areas.R`: import protected areas into GRASS GIS
- `01_05_grassgis_import_indigenous_territories.R`: import indigenous territories into GRASS GIS
- `03_01_grassgis_prepare_data.R`: prepare data on GRASS GIS
- `03_02_grassgis_calculate_landscape_metrics.R`: calculate landscape metrics on GRASS GIS
- `04_01_tables.R`: resume data on tables 
- `04_02_figures.R`: create figures

## Citation

Vancine MH, Muylaert RL, Niebuhr BB, Oshima JEF, Tonetti V, Bernardo R, De Angelo C, Rosa MR, Grohmann CH, Ribeiro MC. The Atlantic Forest of South America: spatiotemporal dynamic of remaining vegetation and implications for conservation. *in prep.* 

You can also cite this repository, which contains both the code and data used in the publication:  

Vancine MH, Muylaert RL, Niebuhr BB, Oshima JEF, Tonetti V, Bernardo R, De Angelo C, Rosa MR, Grohmann CH, Ribeiro MC. The Atlantic Forest of South America: spatiotemporal dynamic of remaining vegetation and implications for conservation. *in prep.* (Version 1.0) [Code and data set]. OSF.   

## On the Media

No info from this study on the news yet.

## Contact

If you have questions or suggestions, do not hesitate to contact us:

+ Maurício Vancine <<mauricio.vancine@gmail.com>>
+ Renata Muylaert <<renatamuy@gmail.com>>
+ Bernardo Santos <<bernardo_brandaum@yahoo.com.br>>
+ Milton Cezar Ribeiro <<milton.c.ribeiro@unesp.br>>>
