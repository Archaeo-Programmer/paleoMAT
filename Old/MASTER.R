# A script for creating a low-frequency temperature reconstruction from
# pollen using the modern analog technique

# Load necessary packages
# Additional packages are loaded in the *_FUNCTIONS.R scripts below
library(FedData)
FedData::pkg_test("sp")
FedData::pkg_test("raster")
FedData::pkg_test("magrittr")
FedData::pkg_test("dplyr")
FedData::pkg_test("analogue")
FedData::pkg_test("stratigraph") # the library for reading GPD-formatted pollen files

options(scipen=999)

# Load the functions for all analyses below
source('./R/GPD_FUNCTIONS.R')
source('./R/MAT_FUNCTIONS.R')

output.dir <- "./OUTPUT/"

######## BEGIN DATA IMPORT ########
# Get CRU 10-minute climatology
# Mean temperature
dir.create(paste0(output.dir,"/DATA/CRU/"), recursive = T, showWarnings = F)
FedData::download_data(url = "https://crudata.uea.ac.uk/cru/data/hrg/tmc/grid_10min_tmp.dat.gz",
                       destdir = paste0(output.dir,"/DATA/CRU/"))
R.utils::gunzip(paste0(output.dir,"/DATA/CRU/grid_10min_tmp.dat.gz"), remove = F)
CRU <- readr::read_fwf(
  paste0(output.dir,"/DATA/CRU/grid_10min_tmp.dat"),
  col_positions = readr::fwf_widths(widths = c(rep(9,2),rep(7,12)),
                                    col_names = c("LAT",
                                                  "LON",
                                                  "January",
                                                  "February",
                                                  "March",
                                                  "April",
                                                  "May",
                                                  "June",
                                                  "July",
                                                  "August",
                                                  "September",
                                                  "October",
                                                  "November",
                                                  "December")
  )
) %>%
  sp::SpatialPointsDataFrame(coords = .[,c("LON","LAT")], data = .[])




## Load the GPD (pollen) M70 dataset + PRISM mean growing season temperature for 1961:1990
## NOTE: This requires a previous extraction from the LT81 dataset, which takes a LONG TIME.
## Better to just load the pre-extracted dataset
# M70 <- getM70(data.dir=paste(MASTER.DATA,"POLLEN/GPD/M70",sep=''), prism.dir=paste(MASTER.DATA,"PRISM/LT81/",sep=''), out.dir="../DATA/", label="M70_CONUS", modern.years=1961:1990, element="tavg", months=c(5,6,7,8,9), fun="mean")
M70 <- readOGR("../DATA/M70_CONUS.kml", layer='M70_CONUS')
M70.data <- read.csv("../DATA/M70_CONUS.csv")
## Get the GPD (pollen) F70 dataset, from years AD 1-2000, inclusive + PRISM mean growing season temperature for 1950:1980
# F70 <- getF70(data.dir=paste(MASTER.DATA,"POLLEN/GPD/F70",sep=''), prism.dir=paste(MASTER.DATA,"PRISM/LT81/",sep=''), out.dir="../DATA/", label="F70_CONUS", modern.years=1961:1990, element="tavg", months=c(5,6,7,8,9), fun="mean")
F70 <- readOGR("../DATA/F70_CONUS.kml", layer='F70_CONUS')
F70.data <- read.csv("../DATA/F70_CONUS.csv")
## Trim the F70 to the four corners states
states <- readOGR(paste(MASTER.DATA,"NATIONAL_ATLAS/statep010",sep=''), layer='statep010')
states <- spTransform(states,CRS(projection(F70)))
states <- states[states$STATE %in% c("Arizona","Colorado","Utah","New Mexico"),]
SWUS.F70 <- F70[as.vector(!is.na((F70 %over% states)[,1])),]
writeOGR(SWUS.F70,"../DATA/F70_SWUS.kml", layer='F70_SWUS', driver="KML", overwrite_layer=T)
SWUS.F70.data <- F70.data[as.character(F70.data$NAME) %in% as.character(SWUS.F70$Name),]
# Add the data from Wright 2006
WRIGHT2006 <- read.csv(paste(MASTER.DATA,"POLLEN/WRIGHT2006.csv",sep=''))
WRIGHT2006[is.na(WRIGHT2006)] <- 0
SWUS.F70.data.append <- rbind(SWUS.F70.data,WRIGHT2006)
SWUS.F70.data.append.meta <- unique(SWUS.F70.data.append[SWUS.F70.data.append$YEAR>0,"NAME"])

## Generate a map of the study area, with pollen, dendros, and GHCN sites represented
source('../FIGURES/R/SW_MAP.R')

######## FINISH DATA IMPORT ########

######## BEGIN ANALYSIS ########

#### Low frequency modulation via continuous wavelet transformation ####
## First, we need a reconstruction of low frequency regional temperature change in the SWUS
## We use the modern analogue technique (MAT).
SWUS.MAT.retro <- getMAT(fossil.data = SWUS.F70.data,
                         modern.data = M70.data,
                         recon.years = 1:2000)
CONUS.MAT.retro <- getMAT(fossil.data = F70.data,
                          modern.data = M70.data,
                          recon.years=1:2000)
SWUS.MAT.retro.append <- getMAT(fossil.data = SWUS.F70.data.append,
                                modern.data = M70.data,
                                recon.years = 1:2000)

## Modulate the temperature curves using the SWUS MAT reconstruction
CWTmodulation <- function(station, retros, MAT, climate, calib.years){
  climate <- climate[,c("YEAR",GHCN.stations[station])]
  # First, get the deviation from the mean
  modern.climate <- mean(climate[climate$YEAR %in% 1961:1990,2])

  high.freq.dev <- high.freq <- retros[[GHCN.stations.abbrev[station]]]
  high.freq.dev[,2:4] <- high.freq.dev[,2:4] - modern.climate

  retro <- CWT_combine_series(high.freq$RETRO, MAT$y)
  lower <- CWT_combine_series(high.freq$LOWER, MAT$y)
  upper <- CWT_combine_series(high.freq$UPPER, MAT$y)

  modulated <- data.frame(YEAR=high.freq$YEAR, RETRO=retro, LOWER=lower, UPPER=upper)

  # Rescale to calibration data
  modulated$RETRO <- meanVarianceTuning(prediction = modulated$RETRO[modulated$YEAR %in% calib.years],
                                        training = high.freq$RETRO[high.freq$YEAR %in% calib.years],
                                        x.validation = modulated$RETRO)
  modulated$LOWER <- meanVarianceTuning(prediction = modulated$LOWER[modulated$YEAR %in% calib.years],
                                        training = high.freq$LOWER[high.freq$YEAR %in% calib.years],
                                        x.validation = modulated$LOWER)
  modulated$UPPER <- meanVarianceTuning(prediction = modulated$UPPER[modulated$YEAR %in% calib.years],
                                        training = high.freq$UPPER[high.freq$YEAR %in% calib.years],
                                        x.validation = modulated$UPPER)


  return(modulated)
}
# tavg.BROAD.retros.CWT.SWUS <- lapply(1:length(GHCN.stations),FUN=function(station,...){CWTmodulation(station=station, retros=tavg.BROAD.retros, MAT=SWUS.MAT.retro, climate=PRISM.data.tavg, calib.years=calibration.years)})
# names(tavg.BROAD.retros.CWT.SWUS) <- GHCN.stations.abbrev
#
# tavg.BROAD.retros.CWT.CONUS <- lapply(1:length(GHCN.stations),FUN=function(station,...){CWTmodulation(station=station, retros=tavg.BROAD.retros, MAT=CONUS.MAT.retro, climate=PRISM.data.tavg, calib.years=calibration.years)})
# names(tavg.BROAD.retros.CWT.CONUS) <- GHCN.stations.abbrev



#################### BEGIN PART II ####################
#### Spatial reconstructions of mean growing-season temperature, ####
#### cool-season precipitation, and growing-season precipitation ####

######## BEGIN DATA IMPORT ########

## First, define the study areas, and extract the PRISM data from the CONUS LT81 PRISM dataset
## This takes awhile, so I'm commenting it out for now. Only do it once.
# Force Raster to load large rasters into memory
rasterOptions(chunksize=2e+07,maxmemory=2e+08)

# Names of the study areas
area.names <- c("VEPIIN","VEPIIS")

## Extract the VEPIIN area.
sim.poly.VEPIIN <- createArea(North = 4170000, South = 4102000, East = 740000, West = 672800, projection.string = "+proj=utm +datum=NAD83 +zone=12")
# extractPRISM(object=sim.poly.VEPIIN, out.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/", LT81.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/LT81/", label="VEPIIN", year.range=1895:2012, force.redo=T)
## Extract the VEPIIS area.
sim.poly.VEPIIS <- createArea(North = 4030400, South = 3939600, East = 435800, West = 359200, projection.string = "+proj=utm +datum=NAD83 +zone=13")
# extractPRISM(object=sim.poly.VEPIIS, out.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/", LT81.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/LT81/", label="VEPIIS", year.range=1895:2012, force.redo=T)

## Load the PRISM (interpolated climate) data for the study areas
## NOTE: This requires a previous extraction from the LT81 dataset (above).
## This function allows for creating annual aggregates of months.
# PRISM.data.h2o_year.prcp.bricks <- subsetPRISM(prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="prcp", months=c(-2,-1,0,1,2,3,4,5,6,7,8,9), fun="sum")
# PRISM.data.cool.prcp.bricks <- subsetPRISM(prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="prcp", months=c(-2,-1,0,1,2,3,4), fun="sum")
# PRISM.data.grow.prcp.bricks <- subsetPRISM(prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="prcp", months=c(5,6,7,8,9), fun="sum")
# PRISM.data.grow.tmax.bricks <- subsetPRISM(prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="tmax", months=c(5,6,7,8,9), fun="mean")
# PRISM.data.grow.tmin.bricks <- subsetPRISM(prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="tmin", months=c(5,6,7,8,9), fun="mean")
#
# ## Calculate an estimate of accumulated growing season GDD (Tbase=10, Tut=30)
# # Cap tmax and tmin at Tut
# PRISM.data.grow.tmax.GDD.bricks <- mclapply(1:length(PRISM.data.grow.tmax.bricks),FUN=function(i,...){calc(PRISM.data.grow.tmax.bricks[[i]],function(x) { x[x>30] <- 30; return(x) })})
# PRISM.data.grow.tmin.GDD.bricks <- mclapply(1:length(PRISM.data.grow.tmin.bricks),FUN=function(i,...){calc(PRISM.data.grow.tmin.bricks[[i]],function(x) { x[x>30] <- 30; return(x) })})
# # Floor tmax and tmin at Tbase
# PRISM.data.grow.tmax.GDD.bricks <- mclapply(1:length(PRISM.data.grow.tmax.GDD.bricks),FUN=function(i,...){calc(PRISM.data.grow.tmax.GDD.bricks[[i]],function(x) { x[x<10] <- 10; return(x) })})
# PRISM.data.grow.tmin.GDD.bricks <- mclapply(1:length(PRISM.data.grow.tmin.GDD.bricks),FUN=function(i,...){calc(PRISM.data.grow.tmin.GDD.bricks[[i]],function(x) { x[x<10] <- 10; return(x) })})
# PRISM.data.grow.GDD.bricks <- mclapply(1:length(PRISM.data.grow.tmax.GDD.bricks),FUN=function(i,...){(((PRISM.data.grow.tmax.GDD.bricks[[i]]+PRISM.data.grow.tmin.GDD.bricks[[i]])/2)-10)*153})
# # Convert to Fahrenheit GDD
# PRISM.data.grow.GDD.bricks <- mclapply(1:length(PRISM.data.grow.GDD.bricks),FUN=function(i,...){PRISM.data.grow.GDD.bricks[[i]]*1.8})
#
# # Find average temperature grids
# # PRISM.data.grow.tavg.bricks <- mclapply(1:length(PRISM.data.grow.tmax.bricks),FUN=function(i,...){(PRISM.data.grow.tmax.bricks[[i]]+PRISM.data.grow.tmin.bricks[[i]])/2})
# # Subset all grids to calibration period
# PRISM.data.h2o_year.prcp.bricks <- lapply(PRISM.data.h2o_year.prcp.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))/10})
# PRISM.data.cool.prcp.bricks <- lapply(PRISM.data.cool.prcp.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))/10})
# PRISM.data.grow.prcp.bricks <- lapply(PRISM.data.grow.prcp.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))/10})
# PRISM.data.grow.tmax.bricks <- lapply(PRISM.data.grow.tmax.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))})
# PRISM.data.grow.tmin.bricks <- lapply(PRISM.data.grow.tmin.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))})
# PRISM.data.grow.GDD.bricks <- lapply(PRISM.data.grow.GDD.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))})
# # PRISM.data.grow.tavg.bricks <- lapply(PRISM.data.grow.tavg.bricks,FUN=function(x){subset(x,paste("X",calibration.years,sep=''))})
# # Save the bricks
# save(PRISM.data.h2o_year.prcp.bricks,file="../OUTPUT/PRISM_data_h2o_year_prcp_bricks.Rdata")
# save(PRISM.data.cool.prcp.bricks,file="../OUTPUT/PRISM_data_cool_prcp_bricks.Rdata")
# save(PRISM.data.grow.prcp.bricks,file="../OUTPUT/PRISM_data_grow_prcp_bricks.Rdata")
# save(PRISM.data.grow.tmax.bricks,file="../OUTPUT/PRISM_data_grow_tmax_bricks.Rdata")
# save(PRISM.data.grow.tmin.bricks,file="../OUTPUT/PRISM_data_grow_tmin_bricks.Rdata")
# save(PRISM.data.grow.GDD.bricks,file="../OUTPUT/PRISM_data_grow_GDD_bricks.Rdata")
# save(PRISM.data.grow.tavg.bricks,file="../OUTPUT/PRISM_data_grow_tavg_bricks.Rdata")
# Load the PRISM data bricks
load("../OUTPUT/PRISM_data_h2o_year_prcp_bricks.Rdata")
load("../OUTPUT/PRISM_data_cool_prcp_bricks.Rdata")
load("../OUTPUT/PRISM_data_grow_prcp_bricks.Rdata")
load("../OUTPUT/PRISM_data_grow_tmax_bricks.Rdata")
load("../OUTPUT/PRISM_data_grow_tmin_bricks.Rdata")
load("../OUTPUT/PRISM_data_grow_GDD_bricks.Rdata")
# load("../OUTPUT/PRISM_data_grow_tavg_bricks.Rdata")

## Get a SPDF of ITRDB metadata
# and get ITRDB database
ITRDB.data <- read.csv("../DATA/ITRDB_DATA.csv")
ITRDB.meta.sp <- getITRDBMetadataSPDF(names(ITRDB.data),data.dir="../DATA/")

## Load the GPD (pollen) M70 dataset + PRISM mean growing season temperature for 1961:1990
## NOTE: This requires a previous extraction from the LT81 dataset, which takes a LONG TIME.
## Better to just load the pre-extracted dataset
# M70 <- getM70(data.dir=paste(MASTER.DATA,"POLLEN/GPD/M70",sep=''), prism.dir=paste(MASTER.DATA,"PRISM/LT81/",sep=''), out.dir="../DATA/", label="M70_CONUS", modern.years=1961:1990, element="tavg", months=c(5,6,7,8,9), fun="mean")
M70 <- readOGR("../DATA/M70_CONUS.kml", layer='M70_CONUS')
M70.data <- read.csv("../DATA/M70_CONUS.csv")
## Get the GPD (pollen) F70 dataset, from years AD 1-2000, inclusive + PRISM mean growing season temperature for 1950:1980
# F70 <- getF70(data.dir=paste(MASTER.DATA,"POLLEN/GPD/F70",sep=''), prism.dir=paste(MASTER.DATA,"PRISM/LT81/",sep=''), out.dir="../DATA/", label="F70_CONUS", modern.years=1961:1990, element="tavg", months=c(5,6,7,8,9), fun="mean")
F70 <- readOGR("../DATA/F70_CONUS.kml", layer='F70_CONUS')
F70.data <- read.csv("../DATA/F70_CONUS.csv")
## Trim the F70 to the four corners states
states <- readOGR(paste(MASTER.DATA,"NATIONAL_ATLAS/statep010",sep=''), layer='statep010')
states <- spTransform(states,CRS(projection(F70)))
states <- states[states$STATE %in% c("Arizona","Colorado","Utah","New Mexico"),]
SWUS.F70 <- F70[as.vector(!is.na((F70 %over% states)[,1])),]
writeOGR(SWUS.F70,"../DATA/F70_SWUS.kml", layer='F70_SWUS', driver="KML", overwrite_layer=T)
SWUS.F70.data <- F70.data[as.character(F70.data$NAME) %in% as.character(SWUS.F70$Name),]

## ... and go ahead and perform the MAT analysis
SWUS.MAT.retro <- getMAT(fossil.data=SWUS.F70.data,modern.data=M70.data,recon.years=1:2000)

######## END DATA IMPORT ########
