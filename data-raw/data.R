library(magrittr)

# Use states data to create a spatial polygon.
states <-
  rgdal::readOGR(here::here("data-raw/statesp010g"),
                 layer = 'statesp010g')

usethis::use_data(states,
                  overwrite = TRUE)


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

usethis::use_data(west_in_states,
                  overwrite = TRUE)


# Download Viau et al. 2006 data.
download.file(
  url = "https://www.ncei.noaa.gov/pub/data/paleo/pollen/recons/northamerica/viau2006namerica-temp.xls",
  destfile = here::here("data-raw/viau2006namerica-temp.xls"))

viau_2006 <- read_excel(here::here("data-raw/viau2006namerica-temp.xls"), sheet = 2) %>%
  dplyr::select(1:4) %>%
  dplyr::rename(time = 1, temp = 2, CI_plus = 3, CI_minus = 4) %>%
  dplyr::mutate(time = time*100)

usethis::use_data(viau_2006,
                  overwrite = TRUE)


# Download Moberg et al. 2005 data.
download.file(
  url = "https://www.ncei.noaa.gov/pub/data/paleo/contributions_by_author/moberg2005/nhtemp-moberg2005.txt",
  destfile = here::here("data-raw/nhtemp-moberg2005.txt"))

moberg_2005 <- data.table::fread(here::here("data-raw/nhtemp-moberg2005.txt"), skip = 92, header = TRUE)[, 1:3] %>%
  tibble::as_tibble() %>%
  dplyr::filter(LF > -9.89 & Year <= 2000) %>%
  dplyr::rename(date = Year)

usethis::use_data(moberg_2005,
                  overwrite = TRUE)


# Get data from Kaufman et al. 2020. Original data downloaded from https://www.ncei.noaa.gov/access/paleo-search/study/27330.
kaufman_2020 <-
  read.csv(here::here("data-raw/Kaufman 2020_Figure6Data.csv")) %>%
  tibble::as_tibble()

usethis::use_data(kaufman_2020,
                  overwrite = TRUE)


# Get data from Alley et al. 2000.
download.file(
  url = "https://www.ncei.noaa.gov/pub/data/paleo/icecore/greenland/summit/gisp2/isotopes/gisp2_temp_accum_alley2000.txt",
  destfile = here::here("data-raw/gisp2_temp_accum_alley2000.txt"))

alley_2000 <-
  data.table::fread(here::here("data-raw/gisp2_temp_accum_alley2000.txt"), skip = 61, header = FALSE, nrows = 1632) %>%
  dplyr::rename(Age = 1, Temperature = 2) %>%
  tibble::as_tibble()

usethis::use_data(alley_2000,
                  overwrite = TRUE)


# Get core top data from fossil pollen dataset.
# This was originally used to pull in the core top data; however, the SSL certificate has now expired. The data was saved and can be loaded from the .csv file. Data downloaded on October 23, 2021.
# coreTops <-
#   paleomat::get_coreTops() %>%
#   dplyr::rename(dataset.id = DatasetID,
#                 site.id = SiteID,
#                 sample.id = SampleID)

coreTops <-
  read.csv(here::here("data-raw/coreTops.csv")) %>%
  tibble::as_tibble()

usethis::use_data(coreTops,
                  overwrite = TRUE)


# Several fossil pollen sites have updated Bacon age models that were not downloaded in the original fossil pollen dataset (i.e., NAfossil_metadata_counts).
updated_age_models <-
  list.files(here::here("data-raw/updated_age_models"), full.names = TRUE) %>%
  purrr::map_dfr(read.csv) %>%
  tibble::as_tibble()

usethis::use_data(updated_age_models,
                  overwrite = TRUE)


# PRISM mean July temperature for 1961-1990.
temp.raster <- raster::raster(here::here("data-raw/temp.raster.tif"))

usethis::use_data(temp.raster,
                  overwrite = TRUE)

