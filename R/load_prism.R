load_prism <- function(sites, out.dir, prism.dir) {
  
  # Create an output directory
  dir.create(out.dir, showWarnings = F, recursive = T)
  
  # Load all the auxillary functions
  all.functions <- lapply(list.files("./src",full.names=T),source)
  
  # Suppress scientific notation
  options(scipen=999)
  
  # Force Raster to load large rasters into memory
  raster::rasterOptions(chunksize=2e+08,maxmemory=2e+09)
  
  # This MUST point at an original LT81 dataset available from the PRISM climate group (http://www.prism.oregonstate.edu).
  PRISM800.DIR <- prism.dir
  
  # Specify a directory for extraction
  EXTRACTION.DIR <- list.files(paste0(PRISM800.DIR), recursive=TRUE, full.names=T)
  
  # The climate parameters to be extracted
  types <- c("ppt", "tmin","tmax")
  
  ##### BEGIN RAW DATA EXTRACTION #####
  # Create data output directory if it doesn't already exist
  dir.create(".data/extraction", showWarnings = F, recursive = T)
  
  # (Down)Load the states shapefile from the National Atlas
  # if(!dir.exists("/Volumes/VILLAGE/SKOPEII/MAT/WORKING/paleomat/data/statep010")){
  #   dir.create("/Volumes/VILLAGE/SKOPEII/MAT/WORKING/paleomat/data/", showWarnings = F, recursive = T)
  #   download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Boundaries/statesp010g.shp_nt00938.tar.gz", destfile="/Volumes/VILLAGE/SKOPEII/MAT/WORKING/paleomat/data/statesp010g.shp_nt00938.tar.gz", mode='wb')
  #   untar("/Volumes/VILLAGE/SKOPEII/MAT/WORKING/paleomat/data/statesp010g.shp_nt00938.tar.gz", exdir="/Volumes/DATA/NATIONAL_ATLAS/statesp010g")
  # }
  
  # Download the states shapefile form the National Atlas
    download.file("https://prd-tnm.s3.amazonaws.com/StagedProducts/Small-scale/data/Boundaries/statesp010g.shp_nt00938.tar.gz",
                  destfile = "./data/raw_data/statesp010g.shp_nt00938.tar.gz",
                  mode='wb')
    untar("./data/raw_data/statesp010g.shp_nt00938.tar.gz",
        exdir="./data/raw_data/statesp010g")
  
  states <- readOGR("./data/raw_data/statesp010g/statesp010g.shp", layer='statesp010g')
  
  # Transform the states (spatial polygons data frame) to the Coordinate Reference System (CRS) of the PRISM data.
  states <- sp::spTransform(states, sp::CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  # Get the extent (i.e., the continental United States)
  extent.states <- raster::extent(states)
  
  # Floor the minimums, ceiling the maximums.
  extent.states@xmin <- floor(extent.states@xmin)
  extent.states@ymin <- floor(extent.states@ymin)
  extent.states@xmax <- ceiling(extent.states@xmax)
  extent.states@ymax <- ceiling(extent.states@ymax)
  
  # Get list of all file names in the prism directory.
  monthly.files <- EXTRACTION.DIR
  
  # Trim to only file names that are rasters.
  monthly.files <- grep("*\\.bil$", monthly.files, value=TRUE)
  monthly.files <- grep("spqc", monthly.files, value=TRUE, invert=T)
  monthly.files <- grep("/cai", monthly.files, value=TRUE)
  
  # Generate the raster stack.
  type.list <- raster::stack(monthly.files,native=F,quick=T)
  
  
  
  
  # Use the raster extract function to extract out the monthly values at a modern pollen location in a table.
  # You can make it run again by deleting the file.
  climate.points <- raster::extract(x = type.list, y = modern_pollen_sites(sites), df = TRUE)
  
  names <- sites$sample.id
  
  # Replace the first column of IDs with sample.id. This will make it easier to match dataframes. This helps to check the data and not assume that the rows climate data are in the same order as the pollen data.
  climate.points$ID <- names
  
  # Rename the ID column to sample.id.
  climate.points <- dplyr::rename(climate.points, sample.id = ID)
  
  # Convert climate data from wide to long format, but keep ID so that we can remember the location that each record came from. Then, we use the separate function to split apart the long prism file names, of which some of the new columns we will not need; therefore, they are labeled as garbage, and these get deleted. Finally, we separate the yearmonth into two columns. This will allow for these to be grouped together to do summarizing below. sep = 4 is the location in the string of six numbers (yearmonth) to separate.
  data_long <- melt(climate.points,
                    # sample.id variables - all the variables to keep but not split apart.
                    id.vars="sample.id") %>% 
    tidyr::separate(col = "variable", into = c("garbage1", "variable", "garbage2", "garbage3", "garbage4", "yearmonth"), sep = "_") %>% 
    dplyr::select(-starts_with("garbage")) %>% 
    separate(col = "yearmonth", into = c("year", "month"), sep = 4) 
  
  
  # Now use the long format data to create an additional variable, called Growing Degree Days (GDD). A function has been created here to convert the tmin and tmax data into GDD, using the data_long dataframe.
  
  # Calculate GDD monthly. This is an adapted function from Bocinsky et al. 2016 (calc_gdd_monthly).
  
  calc_gdd_monthly <- function(temp, t.base, t.cap=NULL, multiplier=1, to_fahrenheit=T, to_round=F){
    if(nrow(dplyr::filter(temp, variable == "tmin"))!=nrow(dplyr::filter(temp, variable == "tmax"))){
      stop("tmin and tmax must have same number of observations!")
    }
    
    tmin <- dplyr::filter(temp, variable == "tmin")
    tmax <- dplyr::filter(temp, variable == "tmax")
    
    t.base <- t.base*multiplier
    if(!is.null(t.cap)){
      t.cap <- t.cap*multiplier
    }
    
    # Floor tmax and tmin at Tbase
    tmin["value"] <- lapply(tmin["value"], function(x) { x[x<t.base] <- t.base; return(x) })
    tmax["value"] <- lapply(tmax["value"], function(x) { x[x<t.base] <- t.base; return(x) })
    
    # Cap tmax and tmin at Tut
    if(!is.null(t.cap)){
      tmin["value"] <- lapply(tmin["value"], function(x) { x[x>t.cap] <- t.cap; return(x) })
      tmax["value"] <- lapply(tmax["value"], function(x) { x[x>t.cap] <- t.cap; return(x) })
    }
    
    temp_long <- left_join(tmin, tmax, by = c("sample.id", "year", "month")) %>% 
      dplyr::select(-starts_with("variable.")) %>% 
      plyr::rename(c("value.x" = "tmin", "value.y" = "tmax"))
    
    temp_long$GDD <- (((temp_long$tmin + temp_long$tmax) / 2) - t.base)
    temp_long <- temp_long %>% dplyr::select(-starts_with("tm"))
    
    # Combine month and year column.
    temp_long$Date <- zoo::as.yearmon(paste(temp_long$year, temp_long$month), "%Y %m")
    
    # Multiply by days per month, and convert to Fahrenheit GDD
    temp_long$GDD <- temp_long$GDD * Hmisc::monthDays(temp_long$Date) / multiplier
    
    if(to_fahrenheit){
      temp_long$GDD <- temp_long$GDD * 1.8
    }
    
    if(to_round){
      temp_long$GDD <- round(temp_long$GDD)
    }
    
    temp_long <- temp_long %>% 
      dplyr::select(-starts_with("Date")) %>% 
      dplyr::mutate(variable = "GDD") %>% 
      dplyr::rename(value = GDD) %>% 
      dplyr::select(sample.id, variable, year, month, value)
    
    return(temp_long)
  }
  
  temp_long <- calc_gdd_monthly(data_long, t.base = 10, t.cap=30, multiplier=1, to_fahrenheit=F, to_round=F)
  
  # Now rbind the GDD data to the other 3 climate variables (tmin, tmax, and ppt) to create one dataframe with all of the climate variables.
  data_long <- rbind(data_long, temp_long)
  
  
  # Need to filter to 30 years prior from the publication date of each pollen surface sample site.
  # Possibly loop through for each site, determine the publication date, then take the previous 30 years of PRISM data and do avgs
  
  # for (i in 1:length(MPCT_metadata_counts$sample.id)) {
  # 
  #   
  #   
  # }
  
  # Eventually, this will be replaced to represent the 30 years prior to publication or collection date, but for now we use 1961 to 1990.
  data_30yr <- data_long %>% 
    dplyr::filter(year >= 1961 & year <= 1990)
  
  # Take the long format data, then group by ID, which is a unique location, the variable (i.e., precipitiation, min. temp., max temp., and GDD), and each month (e.g., 01 for January, 02 for February, etc.). So, we essentially get a 30 year average for each of the 12 months for the 3 variables at each unique location.
  data_avgs <- data_30yr %>%
    dplyr::group_by(sample.id, variable, month) %>% 
    dplyr::summarize(mean = mean(value))
  
  # The precipitation averages are extracted into its own dataframe.
  ppt_avgs <- data_avgs %>% 
    dplyr::filter(variable == "ppt")
  
  #The temperature (min. and max) are extracted into its own dataframe, so that we can get the temp. average for each month. We also create a new column for the variable type (i.e., tmp). Finally, we select the data that we want to keep in the dataframe.
  tmp_avgs <- data_avgs %>% 
    dplyr::filter(variable == "tmin" | variable == "tmax") %>% 
    dplyr::group_by(sample.id, month) %>% 
    dplyr::summarize(mean = mean(mean)) %>% 
    dplyr::mutate(variable = "tmp") %>% 
    dplyr::select(sample.id, variable, month, mean)
  
  # The GDD averages are extracted into its own dataframe.
  gdd_avgs <- data_avgs %>% 
    dplyr::filter(variable == "GDD")
  
  # Now, combine the precipitation, temperature, and GDD data back into one dataframe, and convert from long to wide format. Then, we can get rid of the ID column as it will be the same as the location dataframe. Then, rename all the column names. 
  clim_wide <- rbind(gdd_avgs, ppt_avgs, tmp_avgs) %>% 
    dcast(sample.id ~ variable + month, value.var="mean") %>% 
    #dplyr::select(-one_of("dataset.id")) %>% 
    set_colnames(c('sample.id', 'gjan', 'gfeb', 'gmar', 'gapr', 'gmay', 'gjun', 'gjul', 'gaug', 'gsep', 'goct', 'gnov', 'gdec', 'pjan', 'pfeb', 'pmar', 'papr', 'pmay', 'pjun', 'pjul', 'paug', 'psep', 'poct', 'pnov', 'pdec', 'tjan', 'tfeb', 'tmar', 'tapr', 'tmay', 'tjun', 'tjul', 'taug', 'tsep', 'toct', 'tnov', 'tdec'))
  
}