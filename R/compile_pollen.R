<<<<<<< HEAD
# Compile function for the modern pollen data from Neotoma.
compile_pollen <- function(x){
  tibble::tibble(
=======
compile_pollen <- function(x){
  tibble(
>>>>>>> 8e24ef29ab6348fffeb13ffed298f627da9265b9
    dataset.id = x$dataset$dataset.meta$dataset.id,
    site.id = x$dataset$site.data$site.id,
    sample.id = x$sample.meta$sample.id,
    site.name = x$dataset$site$site.name,
    depth = x$sample.meta$depth,
    lat = x$dataset$site$lat,
    long = x$dataset$site$long,
<<<<<<< HEAD
    elev = x$dataset$site$elev,
    age = x$sample.meta$age)
}
=======
    elev = x$dataset$site$elev, 
    age = x$sample.meta$age)
}
>>>>>>> 8e24ef29ab6348fffeb13ffed298f627da9265b9
