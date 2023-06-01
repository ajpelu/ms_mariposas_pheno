library(tidyverse)
library(sf)
library(terra)
library(raster)
library(purrr)



# Leer arhivos 
transectos <- sf::st_read("raw_data/transectos_unidos_export.shp") |> 
  dplyr::select(transectid = TRANSECTID)
transectos_re <- st_transform(transectos, 25830)
v <- terra::vect(transectos_re)
  
### TMIN 
f <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MINIMA_MENSUAL/InfGeografica/InfRaster/COG" 
files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

filesss <- files[1:2]

g0 <- terra::extract(rast(files[1]), v, xy= TRUE)

for (i in files){
  g <- terra::extract(rast(i), v, xy= TRUE)
  g0 <- g0 |> inner_join(g)
}


tm2 <- g0 |> 
  relocate(ID,x,y) |> 
  pivot_longer(-c(ID, x, y)) 

write_csv(tm2, "raw_data/tmin_transect.csv")

### TMAX
f <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MAXIMA_MENSUAL/InfGeografica/InfRaster/COG/" 
files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

g0 <- terra::extract(rast(files[1]), v, xy= TRUE)

for (i in files){
  g <- terra::extract(rast(i), v, xy= TRUE)
  g0 <- g0 |> inner_join(g)
}


tm2 <- g0 |> 
  relocate(ID,x,y) |> 
  pivot_longer(-c(ID, x, y)) 

write_csv(tm2, "raw_data/tmax_transect.csv")


### TMED
f <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MEDIA_MENSUAL/InfGeografica/InfRaster/COG/" 
files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

g0 <- terra::extract(rast(files[1]), v, xy= TRUE)

for (i in files){
  g <- terra::extract(rast(i), v, xy= TRUE)
  g0 <- g0 |> inner_join(g)
}


tm2 <- g0 |> 
  relocate(ID,x,y) |> 
  pivot_longer(-c(ID, x, y)) 

write_csv(tm2, "raw_data/tmed_transect.csv")



### PREC
f <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/PRECIP_MENSUAL/InfGeografica/InfRaster/COG_500/" 
files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)

g0 <- terra::extract(rast(files[1]), v, xy= TRUE)

for (i in files){
  g <- terra::extract(rast(i), v, xy= TRUE)
  g0 <- g0 |> inner_join(g)
}


tm2 <- g0 |> 
  relocate(ID,x,y) |> 
  pivot_longer(-c(ID, x, y)) 

write_csv(tm2, "raw_data/prec_transect.csv")










