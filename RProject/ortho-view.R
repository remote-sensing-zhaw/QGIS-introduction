
# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1/
# Download earth data first
# https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip

library(sf)
library(lwgeom)
library(dplyr)
library(ggplot2)
library(mapview)


sf::sf_use_s2(FALSE)
# Read the data
# mini_world <- read_sf('data/ne_110m_land/ne_110m_land.shp')
mini_world <- read_sf('natural_earth.gpkg', "world")

# Define the orthographic projection
# Choose lat_0 with -90 <= lat_0 <= 90 and lon_0 with -180 <= lon_0 <= 180
lat <- 45
lon <- 2
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

# Define the polygon that will help you finding the "blade"
# to split what lies within and without your projection
circle <- st_point(x = c(0,0)) %>% st_buffer(dist = 6371000) %>% st_sfc(crs = ortho)

# Project this polygon in lat-lon
circle_longlat <- circle %>% st_transform(crs = 4326)

# circle_longlat cannot be used as it is
# You must decompose it into a string with ordered longitudes
# Then complete the polygon definition to cover the hemisphere
if(lat != 0) {
  circle_longlat <- st_boundary(circle_longlat)
  
  circle_coords <- st_coordinates(circle_longlat)[, c(1,2)]
  circle_coords <- circle_coords[order(circle_coords[, 1]),]
  circle_coords <- circle_coords[!duplicated(circle_coords),]
  
  # Rebuild line
  circle_longlat <- st_linestring(circle_coords) %>% st_sfc(crs = 4326)
  
  if(lat > 0) {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = 90),
                            c(X = -180, Y = 90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  } else {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = -90),
                            c(X = -180, Y = -90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  }
  
  circle_longlat <- st_union(st_make_valid(circle_longlat), st_make_valid(rectangle))
}

# This visualization shows the visible emisphere in red
ggplot() +
  geom_sf(data = mini_world) +
  geom_sf(data = circle_longlat, color = 'red', fill = 'red', alpha = 0.3)

# A small negative buffer is necessary to avoid polygons still disappearing in a few pathological cases
# I should not change the shapes too much
visible <- st_intersection(st_make_valid(mini_world), st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)

# DISCLAIMER: This section is the outcome of trial-and-error and I don't claim it is the best approach 
# Resulting polygons are often broken and they need to be fixed
# Get reason why they're broken
broken_reason <- st_is_valid(visible, reason = TRUE)

# First fix NA's by decomposing them
# Remove them from visible for now
na_visible <- visible[is.na(broken_reason),]
visible <- visible[!is.na(broken_reason),]

# Open and close polygons
na_visible <- st_cast(na_visible, 'MULTILINESTRING') %>% 
  st_cast('LINESTRING', do_split=TRUE)
na_visible <- na_visible %>% mutate(npts = npts(geometry, by_feature = TRUE))
# Exclude polygons with less than 4 points
na_visible <- na_visible %>%
  filter(npts >=4) %>%
  select(-npts) %>%
  st_cast('POLYGON')

# Fix other broken polygons
broken <- which(!st_is_valid(visible))
for(land in broken) {
  result = tryCatch({
    # visible[land,] <- st_buffer(visible[land,], 0) # Sometimes useful sometimes not
    visible[land,] <- st_make_valid(visible[land,]) %>% 
      st_collection_extract()  
  }, error = function(e) {
    visible[land,] <<- st_buffer(visible[land,], 0)
  })
}

# Bind together the two tables
visible <- rbind(visible, na_visible)

# Final plot
ggplot() +
  geom_sf(data = circle,
          #fill = 'aliceblue') + # if you like the color
          fill = NA) +
  geom_sf(data=st_collection_extract(visible)) +
  coord_sf(crs = ortho)
