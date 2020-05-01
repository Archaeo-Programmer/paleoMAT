library(magrittr)
library(raster)

# Force Raster to load large rasters into memory
raster::rasterOptions(chunksize=2e+09,
                      maxmemory=2e+10)

##### BEGIN RAW DATA EXTRACTION #####

# Get list of all file names in the prism directory.
monthly <-
  "data-raw/LT81_800M/" %>%
  list.files(recursive = TRUE,
             full.names = TRUE,
             pattern = "*\\.bil$") %>%
  raster::stack(quick = TRUE)

get_normals <- function(x){
  out <-
    x %>%
    raster::stack(quick = TRUE) %>%
    raster::readAll() %>%
    raster::mean() %>%
    multiply_by(10) %>%
    round() %>%
    as.integer() %>%
    raster::readAll()
  gc()
  gc()
  return(out)
}

prism_normals <-
  tibble::tibble(name = names(monthly),
                 rast = raster::unstack(monthly)) %>%
  tidyr::separate(name,
                  into = c("cai","element","cont","us","res","ym")) %>%
  dplyr::select(element, ym, rast) %>%
  tidyr::separate(ym, into = c("year","month"), sep = 4) %>%
  dplyr::filter(year %in% 1961:1990) %>%
  dplyr::group_by(element, month) %>%
  dplyr::summarise(normal =
                     rast %>%
                     get_normals() %>%
                     list())

prism_normals$normal %<>%
  purrr::map(function(x){
    x %<>%
      raster::setValues(as.integer(x[]))
    raster::dataType(x) <- "INT2S"
    x
  })

usethis::use_data(prism_normals, version = 3, overwrite = TRUE)
