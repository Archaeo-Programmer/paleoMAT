compile_pollen <- function(x){
  tibble(
    dataset.id = x$dataset$dataset.meta$dataset.id,
    site.id = x$dataset$site.data$site.id,
    sample.id = x$sample.meta$sample.id,
    site.name = x$dataset$site$site.name,
    depth = x$sample.meta$depth,
    lat = x$dataset$site$lat,
    long = x$dataset$site$long,
    elev = x$dataset$site$elev, 
    age = x$sample.meta$age)
}