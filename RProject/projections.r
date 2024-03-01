

library(sf)
library(ggplot2)
library(dplyr)
library(pacman)
# pacman::p_install_gh("ropensci/rnaturalearthhires")

# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)


world_countries <- rnaturalearth::ne_countries(returnclass = "sf")

switzerland <- ne_countries(scale = 10, country = "switzerland", returnclass = "sf")

# todo: make sure all data is doenloaded locally and use this instead
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

mymap2

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





st_multilinestring(
  list(
    matrix(c(10,0,  90, 0, 10, 0),ncol = 2, byrow = TRUE)
    # matrix(c(10,0, -170, 0, 10, 0),ncol = 2, byrow = TRUE)
  )
  ) |> 
  st_sfc(crs = 4326) -> eq 




x1 <- seq(-180, 180,.1)
y2 <- seq(-90, 90, .1)

  
my_points2  <-  rbind(
  tibble(x = x1, y = 0), 
  tibble(x = 0, y = y2),
  tibble(x = 180, y = y2)
  )  %>%
  st_as_sf(coords = c("x","y"), remove = FALSE, crs = 4326) 


## WGS84
ggplot() +
  geom_sf(data = circle, fill = "#68838B") +
  geom_sf(data = world_countries, fill = "darkgrey",color = "lightgrey") +
  geom_sf(data = my_points2, size = .5) +
  coord_sf(crs = ortho) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.ontop = FALSE) 

# Now, visualize UTM Zones

# https://hub.arcgis.com/datasets/esri::world-utm-grid/explore
utm_zones <- read_sf("World_UTM_Grid_-8777149898303524843.gpkg")


utm_zones

north_letters <- LETTERS[14:23]
south_letters <- LETTERS[3:13]

utm_zones <- utm_zones |> 
  mutate(n_s = case_when(
    ROW_ %in% north_letters ~ "N",
    ROW_ %in% south_letters ~ "S",
    .default = NA_character_
  ))

utm_simplified <- utm_zones |> 
  filter(!is.na(n_s)) |> 
  group_by(ZONE, n_s) |> 
  summarise()


utm_zones <- st_make_valid(utm_zones)
ggplot() +
  geom_sf(data = circle, fill = "#68838B") +
  geom_sf(data = world_countries, fill = "darkgrey",color = "lightgrey") +
  geom_sf(data = utm_simplified, fill = NA, lwd = 2)  +
  geom_sf(data = filter(utm_simplified, ZONE == 32, n_s == "N"), fill = "#17705d")  +
  geom_sf(data = filter(utm_zones, ZONE == 32, ROW_ == "T"), fill = "#74223d") +
  coord_sf(crs = ortho) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.ontop = FALSE)


ggsave("../images/map_utm32.png", height = 30, width = 30, units = "cm",scale = 2)



world_countries |> 
  filter(name_en == "Switzerland") |> 
  st_transform(32632) |> 
  st_bbox() |> 
  (\(x)round(x/1000)*1000)()


## Maps for epsg2056 and 21781

lakes10 <- ne_download(scale = 10, type = "lakes_europe", category = "physical",returnclass = "sf")
lakes50 <- ne_download(scale = 10, type = "lakes", category = "physical",returnclass = "sf")

lakes10 <- st_make_valid(lakes10)
lakes50 <- st_make_valid(lakes50)

lakes <- rbind(lakes10[,"name_de"], lakes50[,"name_de"])
lakes_ch <- lakes[switzerland,,]


## epsg 2056

xc <- 2600000
yc <- 1200000
poix <- 2694139
poiy <- 1230462
offset <- tibble(
  x = c(poix, xc), 
  y = c(yc, poiy), 
  xend = c(poix, poix), 
  yend = c(poiy, poiy))




library(ggrepel)
ggplot() +
  geom_sf(data = switzerland, fill = "darkgrey",color = "lightgrey") +
  geom_sf(data = lakes_ch, fill = "#17705d") +
  geom_vline(xintercept = xc) +
  geom_hline(yintercept = yc) +
  # geom_segment(data = offset,aes(x,y,xend = xend,yend = yend), colour = "#17705d", lty = 3) +
  # geom_text(data = offset[1,], aes(x, yend), label = "Grüental",nudge_x = 5000, nudge_y = 10000,hjust = 0) +
  scale_x_continuous(breaks = xc,labels = scales::label_number(big.mark = "'")) +
  scale_y_continuous(breaks = yc,labels = scales::label_number(big.mark = "'")) +
  coord_sf(crs = 2056,datum = 2056) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.ontop = FALSE,
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "white",size = 30),
        axis.text.y = element_text(angle = 90, hjust = .5))


ggsave("../images/epsg_2056.svg",height = 30/1.6, width = 30, units = "cm",scale = 1)



xc <- 600000
yc <- 200000
poix <- 694139
poiy <- 230462
offset <- tibble(
  x = c(poix, xc), 
  y = c(yc, poiy), 
  xend = c(poix, poix), 
  yend = c(poiy, poiy))


ggplot() +
  geom_sf(data = switzerland, fill = "darkgrey",color = "lightgrey") +
  geom_sf(data = lakes_ch, fill = "#17705d") +
  geom_vline(xintercept = xc) +
  geom_hline(yintercept = yc) +
  # geom_segment(data = offset,aes(x,y,xend = xend,yend = yend), colour = "#17705d", lty = 3) +
  # geom_text(data = offset[1,], aes(x, yend), label = "Grüental",nudge_x = 5000, nudge_y = 10000,hjust = 0) +
  scale_x_continuous(breaks = xc,labels = scales::label_number(big.mark = "'")) +
  scale_y_continuous(breaks = yc,labels = scales::label_number(big.mark = "'")) +
  coord_sf(crs = 21781,datum = 21781) +
  theme(panel.background = element_rect(fill="transparent"),
        plot.background = element_rect(fill="transparent", color=NA),
        panel.ontop = FALSE,
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(color = "white",size = 30),
        axis.text.y = element_text(angle = 90, hjust = .5))


ggsave("../images/epsg_21781.svg",height = 30/1.6, width = 30, units = "cm",scale = 1)
