library(magrittr)

# Use states data to create a spatial polygon.
states <-
  rgdal::readOGR(here::here("data-raw/statesp010g"),
                 layer = 'statesp010g')

usethis::use_data(states,
                  overwrite = TRUE)



unzip(zipfile = here::here("data-raw/ne_110m_land.zip"),
      exdir = here::here("data-raw/ne_110m_land"))

world <-
  here::here("data-raw/ne_110m_land") %>%
  sf::read_sf() %>%
  sf::st_transform("EPSG:8857") %>%
  sf::st_union()


# Create shapefile for only the western  United States, as we will only use modern samples from the western United States.
# Here, we use the Williams and Shuman 2008 (https://doi.org/10.1016/j.quascirev.2008.01.004) eastern and western splits and
# use the maximum of all of the splits to designate the extent of the western United States.
download.file(
  url = "https://ars.els-cdn.com/content/image/1-s2.0-S0277379108000061-mmc1.zip",
  destfile = here::here("data-raw/WS.zip"))

unzip(zipfile = here::here("data-raw/WS.zip"),
      exdir = here::here("data-raw/WS"))

# Remove the zip file.
file.remove(here::here("data-raw/WS.zip"))

# Read in all the western shapefiles.
temp <-
  list.files(path = here::here("data-raw/WS"),
             pattern = "*WEST.shp$",
             full.names = TRUE)

# Then, apply the st_read function to the list of shapefiles.
myfiles <- lapply(temp, sf::st_read, crs = 4326)

# Then, set the name of each list element (each shapefile) to its respective file name.
names(myfiles) <- gsub(
  "*WEST.shp$",
  "",
  list.files(
    here::here("data-raw/WS"),
    pattern = "*WEST.shp$",
    full.names = FALSE
  ),
  fixed = TRUE
)

# Combine all the shapefiles into 1 shapefile with 1 layer.
all_west <- sf::st_as_sf(data.table::rbindlist(myfiles))
all_west <- sf::st_union(all_west$geometry)

sf::write_sf(all_west,
             dsn = here::here("data-raw/WS"),
             layer = 'west_in_states',
             driver = "ESRI Shapefile")

west_in_states <- sf::read_sf(here::here("data-raw/WS/west_in_states.shp"))

usethis::use_data(west_in_states)
