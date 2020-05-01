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

prism_normals %<>%
  dplyr::group_by(month) %>%
  dplyr::group_split() %>%
  magrittr::set_names(paste0("prism_", 1:12))

for(i in 1:length(prism_normals)) assign(names(prism_normals)[i], prism_normals[[i]])

usethis::use_data(prism_1,
                  prism_2,
                  prism_3,
                  prism_4,
                  prism_5,
                  prism_6,
                  prism_7,
                  prism_8,
                  prism_9,
                  prism_10,
                  prism_11,
                  prism_12,
                  version = 3,
                  overwrite = TRUE)
