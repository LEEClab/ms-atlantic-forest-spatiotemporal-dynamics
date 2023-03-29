library(terra)
library(RStoolbox)

r <- terra::rast("~/Downloads/mapas_pueblos13-12-2022__upscayl_4x_realesrgan-x4plus_modificado.tif")
r

l <- terra::vect("01_data/01_limits/01_af_limit/ma_limite_integrador_muylaert_et_al_2018_wgs84_geodesic_v1_2_0_ar.shp")
l

rl <- terra::crop(r, l, mask = TRUE)
rl

rl_pca <- RStoolbox::rasterPCA(rl)
rl_pca

plot(rl_pca$map[[2]] <= 10)

v <- as.polygons(terra::rast(rl_pca$map[[2]] <= -5))
v <- v[v$layer == 1]
plot(v)

terra::writeVector(v, "01_data/06_indigenous_territory/indigenous_territory_argentina_wgs84_geo.shp", overwrite = TRUE)
