extract_WS_pollen_ranges <- function() {
  # Create shapefile for only the western  United States, as we will only use modern samples from the western United States.
  # Here, we use the Williams and Shuman 2008 (https://doi.org/10.1016/j.quascirev.2008.01.004) eastern and western splits and
  # use the maximum of all of the splits to designate the extent of the western United States.
  download.file(
    "https://ars.els-cdn.com/content/image/1-s2.0-S0277379108000061-mmc1.zip",
    "./vignettes/data/spatial/WS.zip"
  )
  unzip("./vignettes/data/spatial/WS.zip", exdir = "./vignettes/data/spatial/WS")  # unzip your file

  # Remove the zip file.
  file.remove("./vignettes/data/spatial/WS.zip")

  # Read in all the western shapefiles.
  temp <-
    list.files(path = "vignettes/data/spatial/WS",
               pattern = "*WEST.shp$",
               full.names = TRUE)

  # Then, apply the st_read function to the list of shapefiles.
  myfiles <- lapply(temp, sf::st_read, crs = 4326)

  # Then, set the name of each list element (each shapefile) to its respective file name.
  names(myfiles) <- gsub(
    "*WEST.shp$",
    "",
    list.files(
      "vignettes/data/spatial/WS",
      pattern = "*WEST.shp$",
      full.names = FALSE
    ),
    fixed = TRUE
  )

  # Combine all the shapefiles into 1 shapefile with 1 layer.
  all_west <- sf::st_as_sf(data.table::rbindlist(myfiles))
  all_west <- sf::st_union(all_west$geometry)

  sf::write_sf(all_west,
               dsn = "vignettes/data/spatial/",
               layer = 'west_in_states',
               driver = "ESRI Shapefile")

}
