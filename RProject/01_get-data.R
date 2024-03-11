# pacman::p_install_gh("ropensci/rnaturalearthhires")

# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")


library(rnaturalearth)
library(rnaturalearthdata)

library(sf)


world_countries <- rnaturalearth::ne_countries(returnclass = "sf")

switzerland <- ne_countries(scale = 10, country = "switzerland", returnclass = "sf")

# todo: make sure all data is doenloaded locally and use this instead
write_sf(world_countries, "natural_earth.gpkg", "world")
write_sf(switzerland, "natural_earth.gpkg", "switzerland")


lakes10 <- ne_download(scale = 10, type = "lakes_europe", category = "physical",returnclass = "sf")
lakes50 <- ne_download(scale = 10, type = "lakes", category = "physical",returnclass = "sf")
lakes10 <- st_make_valid(lakes10)
lakes50 <- st_make_valid(lakes50)

lakes <- rbind(lakes10[,"name_de"], lakes50[,"name_de"])
lakes_ch <- lakes[switzerland,,]


write_sf(lakes_ch, "natural_earth.gpkg", "lakes_ch")

