# This program allows to download fossil and modern data from the Neotoma database, then compile the data so that there will be columns of taxa with counts, as well as metadata attached to each of those records/rows.

# Load necessary libraries and/or packages.
# devtools::install_github("hadley/tidyverse")
# devtools::install_cran("devtools")
library(neotoma)
library(tidyverse)
library(magrittr)

# Download all fossil pollen datasets for the VEPIIN study area. Then save as a table (.csv) file.
# Check to see if this file exists in the current directory. If it does not exist, then create the file.
if(!file.exists("./VEPIIN_fossil_pollen_datasets.csv")){
  
  # This will get the datasets associated with pollen and with the given location. Then, the data will be downloaded by using piping to apply the get_download() function to what is to the left.
  VEPIIN_fossil_pollen_datasets <- get_dataset(datasettype = "pollen",
                                               loc = c(-109.2, 37, -108, 37.7)) %>%
    get_download() %>%
    
    # Compile_downloads takes an object as the input (so what we got from get_download, which is a set of download objects part of download_list), then takes the metadata and the taxa data and counts, and puts them into a format that is usuable for our purposes.
    compile_downloads() %>%
    
    # Covnerts the data.frame into a tibble (or a data_frame) and better displays the data. The T in the piping will assign what is to the left to the variable (VEPIIN_fossil_pollen_datasets). 
    # The write_csv and print() will still happen, but won't be assinged to the variable. 
    as_tibble() %T>%
    write_csv("./VEPIIN_fossil_pollen_datasets.csv") %T>%
    print()
  
# If the file already exists, then just want to read the file and print it. 
} else {
  VEPIIN_fossil_pollen_datasets <- read_csv("./VEPIIN_fossil_pollen_datasets.csv") %T>%
    print()
}

# Generate a compile-like functions appropriate for surface pollen datasets.
compile_modern_pollen <- function(x){
  tibble(
    dataset = x$dataset$dataset.meta$dataset.id,
    site.name = x$dataset$site$site.name, 
                          depth = x$sample.meta$depth,
                          lat = x$dataset$site$lat,
                          long = x$dataset$site$long)
}


# Download all pollen surface sample (or modern pollen) datasets for the VEPIIN study area. Then save as a .csv file.
if(!file.exists("./VEPIIN_modern_pollen_datasets.csv")){
  VEPIIN_modern_pollen_datasets <- get_dataset(datasettype = "pollen surface sample",
                                               loc = c(-109.2, 37, -108, 37.7)) %>%
    get_download()
  
  # Get the modern pollen datasets in the VEPIIN study area and use lapply to apply the compile function to simply return a list of the metadata of each sample (368 and 2313).
  # Then, combine the rows from the datasets together so that they are now in one tibble.
  VEPIIN_modern_pollen_datasets_metadata <- VEPIIN_modern_pollen_datasets %>%
    lapply(compile_modern_pollen) %>%
    bind_rows()
  
  # Get the modern pollen datasets in the VEPIIN study area and use lapply to apply the function ([[), which is used to extract the information, which in this case are the "counts.
  # Then, the other effect to to turn the data frame into a tibble. Then, again bind the rows of each sample together.
  VEPIIN_modern_pollen_datasets_counts <- VEPIIN_modern_pollen_datasets %>%
    lapply("[[","counts") %>%
    lapply(as_tibble) %>%
    bind_rows()
  
  # Now, we have two tibbles that one has the metadata, and the other has the taxa and counts data. These are both in the same order, so now we can bind them together.
  VEPIIN_modern_pollen_datasets <- bind_cols(VEPIIN_modern_pollen_datasets_metadata,
                                             VEPIIN_modern_pollen_datasets_counts) %>%
    write_csv("./VEPIIN_modern_pollen_datasets.csv") %T>%
    print()
  
} else {
  VEPIIN_modern_pollen_datasets <- read_csv("./VEPIIN_modern_pollen_datasets.csv") %T>%
    print()
}




# ----------------------------------------------------
#This was for checking site locations.
test <- get_dataset(datasettype = "vertebrate fauna",
            loc = c(-109.2, 37, -108, 37.7)) %>%
  get_download() %>%
  compile_downloads() %>%
  
  
  test %<>%
  as_tibble() %>%
  dplyr::select(site.name, lat, long) %>%
  dplyr::distinct()

test

coordinates(test) <- ~ long + lat
projection(test) <- "+proj=longlat"

leaflet() %>%
  addTiles() %>%
  leaflet::addMarkers(data = test, popup = ~site.name)
  