## A script for processing data from the global pollen database f70 dataset

getM70 <- function(data.dir, prism.dir, out.dir, label, modern.years=1961:1990, element="tavg", months=c(1:12), fun="mean"){

  all <- lapply(list.files(data.dir,full.names=T),FUN=function(x){readGPD_M70(x)})
  # Get list of files for our records
  files <- unlist(lapply(all,FUN=function(x){x$file}))

  # Merge like elements
  counts <- do.call(rbind,lapply(all,FUN=function(x){x$counts}))
  lons <- unlist(lapply(all,FUN=function(x){x$lon}))
  lats <- unlist(lapply(all,FUN=function(x){x$lat}))

  # Sanitize the names of the counts header
  colnames(counts) <- gsub("^\\s+|\\s+$","",colnames(counts))
  colnames(counts) <- gsub("/","_",colnames(counts))
  colnames(counts) <- gsub("&","_",colnames(counts))
  colnames(counts) <- gsub("-","_",colnames(counts))
  colnames(counts) <- gsub("\\.","_",colnames(counts))

  rownames(counts) <- gsub("^\\s+|\\s+$","",rownames(counts))

  names <- rownames(counts)
  # Get summer mean temperatures for each location
  coords <- cbind(LONGITUDE=lons,LATITUDE=lats)

  # Create a SpatialPointsDataFrame of the samples
  points <- SpatialPointsDataFrame(coords,data.frame(NAME=names,counts), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

  locations <- points[!duplicated(coordinates(points)),]

  # Get PRISM modern data for 1961:1990, following Whitmore et al 2005
  if(element=="tavg"){
    tmin <- getPRISMPointsData(sp.points=locations, type='tmin', LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
    tmax <- getPRISMPointsData(sp.points=locations, type='tmax', LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
    climate <- (tmin+tmax)/2
  }else{
    climate <- getPRISMPointsData(locations, type=element, LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
  }

  climate.points <- climate[match(data.frame(t(coordinates(points))), data.frame(t(coordinates(locations))))]
  locations <- locations[!is.na(climate),]
  points.exist <- points[!is.na(climate.points),]
  coords <- coords[!is.na(climate.points),]
  climate.points <- climate.points[!is.na(climate.points)]

  points.exist@data <- do.call('cbind',list(coords,MODERN_CLIMATE=climate.points,points.exist@data))
  points.slim <- points.exist
  points.slim@data <- data.frame(NAME=points.exist@data$NAME,MODERN_CLIMATE=points.exist@data$MODERN_CLIMATE)

  writeOGR(points.slim,paste(out.dir,label,".kml",sep=''),layer=label, driver="KML",overwrite_layer=T)
  write.csv(points.exist@data,paste(out.dir,label,".csv",sep=''), row.names=F)

  return(points.exist)
}

getF70 <- function(data.dir, prism.dir, out.dir, label, modern.years=1961:1990, element="tavg", months=c(1:12), fun="mean"){

  files <- list.files(data.dir,full.names=T)
  files <- files[!grepl("_index.txt",files)]

  all <- lapply(files,FUN=function(x){readGPD_F70(x)})
  # Get list of files for our records
  files <- unlist(lapply(all,FUN=function(x){x$file}))

  counts <- do.call(rbind,lapply(all,FUN=function(x){x$counts}))
  lons <- unlist(lapply(all,FUN=function(x){x$lon}))
  lats <- unlist(lapply(all,FUN=function(x){x$lat}))
  ages <- unlist(lapply(all,FUN=function(x){x$age}))
  names <- unlist(lapply(all,FUN=function(x){x$name}))

  # Sanitize the names of the counts header
  colnames(counts) <- gsub("^\\s+|\\s+$","",colnames(counts))
  colnames(counts) <- gsub("/","_",colnames(counts))
  colnames(counts) <- gsub("&","_",colnames(counts))
  colnames(counts) <- gsub("-","_",colnames(counts))
  colnames(counts) <- gsub("\\.","_",colnames(counts))

  names <- gsub("^\\s+|\\s+$","",names)

  # Get summer mean temperatures for each location
  coords <- cbind(LONGITUDE=lons,LATITUDE=lats)

  # Create a SpatialPointsDataFrame of the samples
  points <- suppressWarnings(SpatialPointsDataFrame(coords,data.frame(NAME=names,counts), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))

  locations <- points[!duplicated(coordinates(points)),]

  # Get PRISM modern data for 1961:1990, following Whitmore et al 2005
  if(element=="tavg"){
    tmin <- getPRISMPointsData(sp.points=locations, type='tmin', LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
    tmax <- getPRISMPointsData(sp.points=locations, type='tmax', LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
    climate <- (tmin+tmax)/2
  }else{
    climate <- getPRISMPointsData(locations, type=element, LT81.dir=prism.dir, year.range=modern.years, months=months, fun=fun)
  }

  climate.points <- climate[match(data.frame(t(coordinates(points))), data.frame(t(coordinates(locations))))]
  points.exist <- points[!is.na(climate.points),]
  coords <- coords[!is.na(climate.points),]
  ages <- ages[!is.na(climate.points)]
  climate.points <- climate.points[!is.na(climate.points)]

  points.exist@data <- do.call('cbind',list(YEAR=1950-ages,coords,MODERN_CLIMATE=climate.points,points.exist@data))

  points.slim <- points.exist
  points.slim@data <- data.frame(NAME=points.exist@data$NAME,MODERN_CLIMATE=points.exist@data$MODERN_CLIMATE)
  points.slim <- points.slim[!duplicated(coordinates(points.slim)),]

  writeOGR(points.slim,paste(out.dir,label,".kml",sep=''),layer=label, driver="KML",overwrite_layer=T)
  write.csv(points.exist@data,paste(out.dir,label,".csv",sep=''), row.names=F)

  return(points.exist)
}

readGPD_M70 <- function(file){

  everything <- scan(file, sep = '\n', what = '', quiet=T)
  everything <- iconv(everything, "latin1", "ASCII", "?")

  metadata <- gsub('^[^#]*', '', everything, perl = TRUE, useBytes = TRUE)
  metadata <- metadata[metadata != '']
  metadata <- substr(metadata, 2, nchar(metadata))

  justdata <- gsub('#.*$', '', everything, perl = TRUE, useBytes = TRUE)
  justdata <- justdata[justdata != '']

  taxa <- as.numeric(unlist(strsplit(justdata[1], split = ','))[1])
  levels <- as.numeric(unlist(strsplit(justdata[1], split = ','))[2])

  short.tax.names <- substr(justdata[2:(taxa + 1)], 7, 14)

  tax.cat <- as.factor(substr(justdata[2:(taxa + 1)], 16, 16))

  tax.names <- substr(justdata[2:(taxa + 1)], 18, max(nchar(justdata, type = 'width')))

  justdata <- justdata[(taxa+2):length(justdata)]

  data.labels <- justdata[grep(',', justdata, perl = TRUE)]
  data.vector <- justdata[-grep(',', justdata, perl = TRUE)]

  data.labels <- strsplit(data.labels, split = ',')
  data.labels <- matrix(unlist(unlist(data.labels)), ncol = 2, byrow = TRUE)

  depths <- as.numeric(data.labels[,1])

  sample.names <- data.labels[,2]

  absolute.ages <- rep("MODERN",length(data.labels[,2]))

  count.vector <- paste(data.vector, collapse = ' ')
  count.vector <- strsplit(count.vector, split = '[[:space:]]+', perl = TRUE)[[1]]
  count.matrix <- matrix(as.numeric(as.character(count.vector[-1])), ncol = taxa, byrow = TRUE)

  colnames(count.matrix) <- short.tax.names
  rownames(count.matrix) <- sample.names

  locations <- metadata[grep('Lat: ', metadata, perl = TRUE)]
  locations <- gsub(" Lat:","",locations)
  locations <- gsub("N  Lon:","",locations)
  locations <- gsub("W","",locations)
  locations <- gsub("Elev.(m):","",locations, fixed=T)

  locations <- strtrim(locations,28)

  locations <- strsplit(as.character(locations)," ")

  locations <- lapply(locations,FUN=function(x){x[x!=""]})
  locations <- lapply(locations, FUN=function(x){if(length(x) < 3){x<-c(x,"")}else{x<-x}})

  locations <- do.call(rbind,locations)

  lats <- within(data.frame(lats=locations[,1]), {
    dms <- do.call(rbind, strsplit(as.character(lats), "\\."))
    dms[,3] <- gsub('N', '', dms[,3], perl = TRUE, useBytes = TRUE)
    dec <- as.numeric(dms[,1]) + (as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
    rm(dms)
  })

  lons <- within(data.frame(lats=locations[,2]), {
    dms <- do.call(rbind, strsplit(as.character(lats), "\\."))
    dms[,3] <- gsub('W', '', dms[,3], perl = TRUE, useBytes = TRUE)
    dec <- as.numeric(dms[,1]) + (as.numeric(dms[,2]) + as.numeric(dms[,3])/60)/60
    dec <- -dec
    rm(dms)
  })

  elevs <- as.numeric(locations[,3])

  strat.col.out <- list(file=file,
                        counts = abs(count.matrix),
                        sample.names = sample.names,
                        lon = lons$dec,
                        lat = lats$dec,
                        elev = elevs)

  class(strat.col.out) <- 'strat.column'

  return(strat.col.out)
}

readGPD_F70 <- function(file){

  everything <- scan(file, sep = '\n', what = '', quiet=T)
  everything <- iconv(everything, "latin1", "ASCII", "?")
  metadata <- gsub('^[^#]*', '', everything, perl = TRUE, useBytes = TRUE)
  metadata <- metadata[metadata != '']
  metadata <- substr(metadata, 2, nchar(metadata))

  justdata <- gsub('#.*$', '', everything, perl = TRUE, useBytes = TRUE)
  justdata <- justdata[justdata != '']

  taxa <- as.numeric(unlist(strsplit(justdata[1], split = ' '))[2])
  levels <- as.numeric(unlist(strsplit(justdata[1], split = ' '))[3])

  short.tax.names <- substr(justdata[2:(taxa + 1)], 7, 14)
  tax.cat <- as.factor(substr(justdata[2:(taxa + 1)], 16, 16))
  tax.names <- substr(justdata[2:(taxa + 1)], 18,
                      max(nchar(justdata, type = 'width')))

  justdata <- justdata[(taxa+2):length(justdata)]
  data.labels <- justdata[grepl(',', justdata, perl = TRUE) | nchar(justdata)<8]
  data.vector <- justdata[!grepl(',', justdata, perl = TRUE)]
  data.vector <- data.vector[nchar(data.vector)>=8]

  data.labels <- strsplit(data.labels, split = ',')
  data.labels <- lapply(data.labels, FUN=function(x){if(length(x) < 3){x<-c(x,"","")}else{x<-x}})
  data.labels <- matrix(unlist(unlist(data.labels)), ncol = 3, byrow = TRUE)
  depths <- as.numeric(data.labels[,1])
  sample.names <- data.labels[,2]
  absolute.ages <- as.numeric(data.labels[,3])

  count.vector <- paste(data.vector, collapse = ' ')
  count.vector <- strsplit(count.vector, split = '[[:space:]]+', perl = TRUE)[[1]]

  count.matrix <- matrix(as.numeric(as.character(count.vector[-1])), ncol = taxa, byrow = TRUE)
  colnames(count.matrix) <- short.tax.names
  rownames(count.matrix) <- 1:length(absolute.ages)

  NAME <- metadata[grep('Sigle: ', metadata, perl = TRUE)]
  NAME <- gsub(" Sigle:","",NAME)

  lat <- metadata[grep('Latitude: ', metadata, perl = TRUE)]
  lat <- gsub(" Latitude:","",lat)
  if(grepl("S",lat)){north <- FALSE}else{north <- TRUE}
  lat <- gsub("N  ","",lat)
  lat <- gsub("S  ","",lat)
  lat <- strsplit(lat, split = '[[:space:]]+', perl = TRUE)[[1]][2]
  dms <- strsplit(as.character(lat), "\\.")[[1]]
  lat.dec <- as.numeric(dms[1]) + (as.numeric(dms[2]) + as.numeric(dms[3])/60)/60
  if(!north){
    lat.dec <- -lat.dec
  }
  rm(dms)


  lon <- metadata[grep('Longitude: ', metadata, perl = TRUE)]
  lon <- gsub(" Longitude:","",lon)
  if(grepl("E",lon)){west <- FALSE}else{west <- TRUE}
  lon <- gsub("E  ","",lon)
  lon <- gsub("W  ","",lon)
  lon <- strsplit(lon, split = '[[:space:]]+', perl = TRUE)[[1]][2]
  dms <- strsplit(as.character(lon), "\\.")[[1]]
  lon.dec <- as.numeric(dms[1]) + (as.numeric(dms[2]) + as.numeric(dms[3])/60)/60
  if(west){
    lon.dec <- -lon.dec
  }
  rm(dms)

  strat.col.out <- list(file=rep(file,length(absolute.ages)),
                        name=rep(NAME,length(absolute.ages)),
                        counts = abs(count.matrix),
                        ages = absolute.ages,
                        lon = rep(lon.dec,length(absolute.ages)),
                        lat = rep(lat.dec,length(absolute.ages)))

  class(strat.col.out) <- 'strat.column'

  return(strat.col.out)
}
