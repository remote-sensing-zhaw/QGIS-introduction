# https://gist.github.com/rCarto/02dff288244691a53b3c7b55ee44ef86

orthomap <- function(lon, lat) {
  g <- as_s2_geography(TRUE)
  co <- s2_data_countries()
  oc <- s2_difference(g, s2_union_agg(co)) # oceans
  co <- s2_difference(co, s2_union_agg(oc)) # land
  
  # visible half
  b <- s2_buffer_cells(as_s2_geography(paste0("POINT(", lon, " ", lat,")")),
                       distance = 9800000)
  
  # proj
  prj <- paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon)
  
  # visible land
  cov <- s2_intersection(b, co)
  cov <- st_transform(st_as_sfc(cov), prj)
  cov <- cov[!st_is_empty(cov)]
  cov <- suppressWarnings(st_collection_extract(cov, "POLYGON"))
  cov <- st_cast(cov, "MULTIPOLYGON")
  
  
  # visible ocean
  ocv <- s2_intersection(b, oc)
  ocv <- st_transform(st_as_sfc(ocv), prj)
  ocv <- ocv[!st_is_empty(ocv)]
  ocv <- suppressWarnings(st_collection_extract(ocv, "POLYGON"))
  ocv <- st_cast(ocv, "MULTIPOLYGON")
  
  return(list(ocean = ocv, land = cov))
}

install.packages("mapsf")
library(mapsf)
library(sf)
#> Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
library(s2)
w <- orthomap(45,45)

library(ggplot2)
library(sf)
ggplot() + 
  geom_sf(data = w[[1]], fill = "lightblue") +
  geom_sf(data = w[[2]], fill = "brown") +
  geom_sf(data = eq)

