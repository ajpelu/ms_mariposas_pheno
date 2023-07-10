library(tidyverse)
library(sf)
library(terra)
library(raster)
library(purrr)


# Create a custom function to extract the data
extract_transect_data <- function(shapefile, raster_folder_path) {
  # Read the shapefile (transect)
  transectos <- sf::st_read(shapefile) |>
    dplyr::select(transectid = TRANSECTID) |>
    terra::vect()

  # Read the raster files
  files <- list.files(path = raster_folder_path, pattern = ".tif$", full.names = TRUE)

  # Read a raster to compare crs
  r <- terra::rast(files[1])

  # Check projection and transform (project)
  if (crs(r) != crs(transectos)) {
    transectos <- terra::project(transectos, r)
  }

  # Extract all data
  myF <- function(x) {
    terra::extract(terra::rast(x), transectos, xy = TRUE)
  }

  extracted_data <- files |>
    map(myF) |>
    reduce(inner_join)

  # Merge the transect shapefile and extracted data
  merged_data <- terra::merge(transectos, extracted_data) |>
    as.data.frame()

  return(merged_data)
}


transectos_shapefile <- "data/raw_data/transectos_unidos_export.shp"
tmed_path <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MEDIA_MENSUAL/InfGeografica/InfRaster/COG"
tmin_path <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MINIMA_MENSUAL/InfGeografica/InfRaster/COG"
tmax_path <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MAXIMA_MENSUAL/InfGeografica/InfRaster/COG"
prec_path <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/PRECIP_MENSUAL/InfGeografica/InfRaster/COG_500"


tmed <- extract_transect_data(transectos_shapefile, raster_folder_path = tmed_path)
tmin <- extract_transect_data(transectos_shapefile, raster_folder_path = tmin_path)
tmax <- extract_transect_data(transectos_shapefile, raster_folder_path = tmax_path)
prec <- extract_transect_data(transectos_shapefile, raster_folder_path = prec_path)



avg_transect <- function(x) {
  out <- x |>
    dplyr::select(-ID) |>
    pivot_longer(-c("transectid","x", "y")) |>
    group_by(transectid, name) |>
    summarise(value = mean(value, na.rm=TRUE)) |>
    ungroup()
  return(out)
}

tmax_transect_avg <- avg_transect(tmax)
tmed_transect_avg <- avg_transect(tmed)
tmin_transect_avg <- avg_transect(tmin)
prec_transect_avg <- avg_transect(prec)

tmax_transect_avg |> write_csv("data/raw_data/climate_transects/tmax_transect_avg.csv")
tmax |> write_csv("data/raw_data/climate_transects/tmax_transect.csv")

tmin_transect_avg |> write_csv("data/raw_data/climate_transects/tmin_transect_avg.csv")
tmin |> write_csv("data/raw_data/climate_transects/tmin_transect.csv")

tmed_transect_avg |> write_csv("data/raw_data/climate_transects/tmed_transect_avg.csv")
tmed |> write_csv("data/raw_data/climate_transects/tmed_transect.csv")

prec_transect_avg |> write_csv("data/raw_data/climate_transects/prec_transect_avg.csv")
prec |> write_csv("data/raw_data/climate_transects/prec_transect.csv")







# OLD approach
#
# # Read the files (transect)
# transectos <- sf::st_read("/Users/ajpelu/Google Drive/My Drive/MS/2023_MS_MARIPOSAS_PHENO/ms_mariposas_pheno/data/raw_data/transectos_unidos_export.shp") |>
#   dplyr::select(transectid = TRANSECTID) |>
#   terra::vect()
#
# # Read a raster to compare crs
# r <- rast("/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MAXIMA_MENSUAL/InfGeografica/InfRaster/COG/tm3_1991_01_COG.tif")
#
# # Check projection and transform (project)
# if (crs(r) != crs(transectos)) {
#   transectos <- terra::project(transectos, terra::rast(r))
#   }
#
# # Read the files
# f <- "/Users/ajpelu/Google Drive/My Drive/Clima/REDIAM/TEMP_MINIMA_MENSUAL/InfGeografica/InfRaster/COG"
# files <- list.files(path = f, pattern = ".tif$", full.names = TRUE)
#
#
#
#
#
# # Extract all data
# myF <- function(x){
#   terra::extract(terra::rast(x), transectos, xy=TRUE)
# }
#
# extracted_data <- map(files, myF) |> reduce(inner_join)
#
#
# # Generate
# # g0 <- terra::extract(rast(files[1]), transectos, xy=TRUE)
# g00 <- terra::merge(transectos, extracted_data) |> as.data.frame()
#
#
#
#
#
#
# g0 <- terra::extract(rast(files[1]), transectos, xy=TRUE)
#
# g00 <- terra::merge(v, g0)
#
# for (i in files){
#   g <- terra::extract(rast(i), v, xy= TRUE)
#   g0 <- g0 |> inner_join(g)
# }
#
#
# myF <- function(x){
#   terra::extract(rast(x), v, xy= TRUE)
#   }
#
# f |> map_dfr(files, myF) |> reduce()
#
#
#
#

