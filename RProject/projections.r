

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

remote::install_github("ropensci/rnaturalearthhires")

world_countries <- rnaturalearth::ne_countries(returnclass = "sf")
switzerland <- ne_countries(scale = 10, country = "switzerland", returnclass = "sf")

write_sf(world_countries, "natural_earth.gpkg", "world")
write_sf(switzerland, "natural_earth.gpkg", "switzerland")



ullr2poly <- function(xmin, ymin, xmax, ymax){
  c(1,2,1,4,3,4,3,2,1,2) |>
    sapply(\(x)c(xmin, ymin, xmax, ymax)[x]) |>
    matrix(ncol = 2, byrow = TRUE) |>
    list() |>
    st_polygon()
}

world_box <- ullr2poly(xmin = -180, ymin = -90, xmax = 180, ymax = 90) %>% 
  st_sfc() %>% 
  st_set_crs(4326) 


x <- seq(-180, 180,20)
y <- seq(-90, 90, 20)

length(x) * length(y)
my_points  <- expand.grid(x = x, y = y) %>%
  st_as_sf(coords = c("x","y"), remove = FALSE, crs = 4326)

my_circles <- my_points %>% 
  st_buffer(500e3)

my_circles_filter <- dplyr::filter(my_circles, abs(x) != 180)
  
write_sf(my_circles_filter, "toy_data.gpkg","my_circles_filter")


st_area(my_circles_filter) %>% as.numeric() %>% (\(x) ((x/pi)^0.5)/1e3) 

mymap <- ggplot() +
  geom_sf(data = world_countries, fill = "darkgrey",color = "lightgrey") +
  # geom_sf(data = my_circles, alpha = 0.3) +
  scale_x_continuous(breaks = x) + 
  scale_y_continuous(breaks = y) +
  theme(panel.background = element_blank(), panel.grid = element_line("orange", linetype = 2),panel.ontop = TRUE) 

mymap
mymap2 <- mymap + 
  geom_sf(data = my_points) +
  geom_sf(data = my_circles_filter, alpha = 0.3)


lat <- 45
lon <- 2
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')
circle <- st_point(x = c(0,0)) %>% st_buffer(dist = 6371000) %>% st_sfc(crs = ortho)
# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1?permalink_comment_id=3106327

mymap + 
  geom_sf(data = circle, fill = NA) +
  coord_sf(crs = ortho) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.ontop = FALSE)
ggsave("../images/map1.png", height = 15, width = 30, units = "cm",scale = 2)


mymap2 + 
  geom_sf(data = circle, fill = NA) +
  coord_sf(crs = ortho) + 
  labs(title = stringr::str_to_title(proji)) +
  theme()




mymap + coord_sf(crs = "+proj=laea +lon_0=8 +lat_0=46 +ellps=WGS84 +no_defs")

mymap + geom_sf(data = my_circles, alpha = 0.3)+ coord_sf(crs = "+proj=laea +lon_0=8 +lat_0=46 +ellps=WGS84 +no_defs")


mymap + geom_sf(data = my_circles, alpha = 0.3)+ coord_sf(crs = "+proj=laea +lat_0=53 +lon_0=9 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

mymap + coord_sf(crs = "+proj=moll")
mymap + coord_sf(crs = "+proj=aeqd")

mymap + geom_sf(data = my_circles, alpha = 0.3)+ coord_sf(crs = "+proj=moll")

# does not seem to work. see https://stackoverflow.com/questions/71038065/accessing-equal-earth-projection-with-sf
mymap + coord_sf(crs = "+proj=eqearth +datum=WGS84 +wktext")
mymap + geom_sf(data = my_circles_filter, alpha = 0.3) + coord_sf(crs = "+proj=eqearth +datum=WGS84 +wktext")

projinfo <- sf_proj_info("proj")


plot_proj <- function(proji, desc, plot_it = TRUE){
  mymap2 + 
    coord_sf(crs = glue::glue("+proj={proji}")) + 
    labs(title = stringr::str_to_title(proji),caption = desc)
  
  if(plot_it)ggsave(file.path("projinfo_plots",glue::glue("{proji}.png")),scale = 2)
}


purrr::map2(projinfo$name, projinfo$description, \(proji, desc) tryCatch(plot_proj(proji, desc),error = function(e){e}))



