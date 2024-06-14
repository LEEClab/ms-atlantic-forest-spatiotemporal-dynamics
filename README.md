# The Atlantic Forest of South America: spatiotemporal dynamics of vegetation and implications for conservation

This repository shares the code used on the manuscript by [Vancine et al. (2024)](https://doi.org/mjtz).

## code

All analyses were performed in [R language](https://www.r-project.org/) and [GRASS GIS](https://grass.osgeo.org/) through [*rgrass*](https://rsbivand.github.io/rgrass/) R package. This folder contains all R code files used: 

- `01_01_download_limits.R`: download Atlantic Forest, country, state, and municipality limits
- `01_02_download_mapbiomas.R`: download [MapBiomas Brazil v07](https://brasil.mapbiomas.org/) and [MapBiomas Bosque Atlántico v02](https://bosqueatlantico.mapbiomas.org/) LULC rasters
- `01_03_download_roads_railways.R`: download roads and railways from Brazil ([IBGE](https://www.ibge.gov.br/)), Argentina ([IGN](https://www.ign.gob.ar)) and Paraguay ([INE](https://www.ine.gov.py)) official databases
- `01_04_download_protected_areas.R`: download protected areas from [Protected Planet](https://www.protectedplanet.net/en)
- `01_05_download_indigenous_territories.R`: download indigenous territories from Brazil ([FUNAI](https://www.gov.br/funai/pt-br)) and Paraguay ([Tierras Indígenas](https://www.tierrasindigenas.org)) official databases
- `02_01_grassgis_import_limits.R`: import Atlantic Forest, country, state, and municipality limits into GRASS GIS            
- `02_02_grassgis_import_mapbiomas.R`: import Mapbiomas LULC rasters into GRASS GIS
- `02_03_grassgis_import_roads_railways.R`: import roads and railways into GRASS GIS
- `02_04_grassgis_import_protected_areas.R`: import protected areas into GRASS GIS
- `01_05_grassgis_import_indigenous_territories.R`: import indigenous territories into GRASS GIS
- `03_01_grassgis_prepare_data.R`: prepare data on GRASS GIS
- `03_02_grassgis_calculate_landscape_metrics.R`: calculate landscape metrics on GRASS GIS
- `04_01_tables.R`: resume data on tables on R
- `04_02_figures.R`: create figures on R

## figures

All figures in the paper in high definition.

## tables

All tables in the paper.

## Citation

Vancine, M. H., Muylaert, R. L., Niebuhr, B. B., de Faria Oshima, J. E., Tonetti, V., Bernardo, R., De Angelo, C., Rosa, M. R., Grohmann, C. H., Ribeiro, M. C. The Atlantic Forest of South America: spatiotemporal dynamics of vegetation and implications for conservation. Biological Conservation, 291: 110499. https://doi.org/10.1016/j.biocon.2024.110499

You can also cite this project, which contains both the code and data used in the publication:  

Vancine, M. H., Muylaert, R. L., Niebuhr, B. B., de Faria Oshima, J. E., Tonetti, V., Bernardo, R., De Angelo, C., Rosa, M. R., Grohmann, C. H., Ribeiro, M. C. (2024, March 02). Atlantic Forest spatiotemporal dynamic. https://doi.org/10.17605/OSF.IO/RFWBZ

## On the Media

+ [Novo estudo destaca preocupação com estado de conservação da Mata Atlântica.](https://jornal.unesp.br/2024/03/15/novo-estudo-destaca-preocupacao-com-estado-de-conservacao-da-mata-atlantica/)
+ [Pesquisa da Unesp aponta melhora na conservação da Mata Atlântica.](https://record.r7.com/recordtv-interior-sp/sp-record/pesquisa-da-unesp-aponta-melhora-na-conservacao-da-mata-atlantica-27032024/?utm_source=link_direto&utm_medium=share-bar&utm_campaign=r7-topo)
+ [Unidades de Conservação: guardiãs da Mata Atlântica](https://www.ekosbrasil.org/unidades-de-conservacao-guardias-da-mata-atlantica/)
## Contact

If you have questions or suggestions, do not hesitate to contact us:

+ Maurício Vancine <<mauricio.vancine@gmail.com>>
+ Renata Muylaert <<renatamuy@gmail.com>>
+ Bernardo Santos <<bernardo_brandaum@yahoo.com.br>>
+ Milton Cezar Ribeiro <<milton.c.ribeiro@unesp.br>>>
