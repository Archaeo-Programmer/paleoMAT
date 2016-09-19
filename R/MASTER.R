# A script for learning how to do cross-validation, using the data from Stahle et al. 2013, and then
# then entire ITRDB.
# Set the working directory to the location of R-scripts
setwd("~/IMPORTANT/DISSERTATION/PALEOPROD/R_FINAL/")

# Load necessary packages
# Additional packages are loaded in the *_FUNCTIONS.R scripts below
library(care)
library(dplR)
library(cvTools)
library(parallel)
library(plyr)
library(xtable)
library(spatial.tools)
library(gdata)
library(rgdal)

options(scipen=999)

# Load the functions for all analyses below
source('./UTILITY_FUNCTIONS.R')
source('./GHCN_FUNCTIONS.R')
source('./ITRDB_FUNCTIONS.R')
source('./GPD_FUNCTIONS.R')
source('./PRISM_FUNCTIONS.R')
source('./CROSS_VALIDATE_FUNCTIONS.R')
source('./BROAD_FUNCTIONS.R')
source('./MAT_FUNCTIONS.R')
source('./WAVELET_FUNCTIONS.R')
source('./NED_FUNCTIONS.R')
source('./NRCS_FUNCTIONS.R')
source('./NHD_FUNCTIONS.R')
source('./DEM_DRAIN_FUNCTIONS.R')
source('./NRCS_RES_FILLER_FUNCTIONS.R')

## Set the master data directory
# MASTER.DATA <- "/Volumes/BOCINSKY_DATA/DATA/"
MASTER.DATA <- "/Users/bocinsky/Desktop/SAA2014/BOCINSKY_ANSCHUETZ_CLARK_SAA2014/CCAC/"

## Set the calibration period
# Here, I use a 60 year period ending at 1983 
# to maximize the number of dendro series.
calibration.years <- 1924:1983

## The weather station locations to be analyzed
GHCN.stations <- c("USC00055531","USC00051886","USC00295084","USC00293031")
GHCN.stations.abbrev <- c("MVNP","CRTZ","LANL","ESPN")
GHCN.stations.long <- c("Mesa Verde National Park, Colorado","Cortez, Colorado","Los Alamos National Laboratory, New Mexico","Espanola, New Mexico")


######## BEGIN DATA IMPORT ########

## Load the GHCN station data
# GHCN.data.prcp <- getGHCN(GHCN.stations, data.dir=paste(MASTER.DATA,"GHCN/DAILY/",sep=''), element="prcp", months=c(-2,-1,0,1,2,3,4,5,6,7,8,9), fun="sum")
# GHCN.data.tmax <- getGHCN(GHCN.stations, data.dir=paste(MASTER.DATA,"GHCN/DAILY/",sep=''), element="tmax", months=c(5,6,7,8,9), fun="mean")
# GHCN.data.tmin <- getGHCN(GHCN.stations, data.dir=paste(MASTER.DATA,"GHCN/DAILY/",sep=''), element="tmin", months=c(5,6,7,8,9), fun="mean")
# GHCN.data.prcp[,!(names(GHCN.data.prcp)=="YEAR")] <- GHCN.data.prcp[,!(names(GHCN.data.prcp)=="YEAR")]/100
# GHCN.data.tmax[,!(names(GHCN.data.tmax)=="YEAR")] <- GHCN.data.tmax[,!(names(GHCN.data.tmax)=="YEAR")]/10
# GHCN.data.tmin[,!(names(GHCN.data.tmin)=="YEAR")] <- GHCN.data.tmin[,!(names(GHCN.data.tmin)=="YEAR")]/10
# write.csv(GHCN.data.prcp,"../DATA/GHCN.data.prcp.csv")
# write.csv(GHCN.data.tmax,"../DATA/GHCN.data.tmax.csv")
# write.csv(GHCN.data.tmin,"../DATA/GHCN.data.tmin.csv")
GHCN.data.prcp <- read.csv("../DATA/GHCN.data.prcp.csv")
GHCN.data.tmax <- read.csv("../DATA/GHCN.data.tmax.csv")
GHCN.data.tmin <- read.csv("../DATA/GHCN.data.tmin.csv")
GHCN.data.tavg <- as.data.frame((data.matrix(GHCN.data.tmax)+data.matrix(GHCN.data.tmin))/2)
GHCN.data.tavg$YEAR <- GHCN.data.tmax$YEAR
GHCN.data.prcp$YEAR <- as.numeric(as.character(GHCN.data.prcp$YEAR))
GHCN.data.tmax$YEAR <- as.numeric(as.character(GHCN.data.tmax$YEAR))
GHCN.data.tmin$YEAR <- as.numeric(as.character(GHCN.data.tmin$YEAR))
GHCN.data.tavg$YEAR <- as.numeric(as.character(GHCN.data.tavg$YEAR))


## Get a SPDF of weather station metadata (for extracting PRISM data)
GHCN.meta.sp <- getGHCNStationMetadataSPDF(GHCN.stations, data.dir="../DATA/")

## Load the PRISM (interpolated climate) data for the station locations
## NOTE: This requires a previous extraction from the LT81 dataset, which takes a LONG TIME.
## If you get ahold of the LT81 PRISM dataset, you can run the script ./PRISM_EXTRACTOR.R to perform personalized spatial extractions.
# PRISM.data.prcp <- subsetPRISM(locations=GHCN.meta.sp, prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="prcp", months=c(-2,-1,0,1,2,3,4,5,6,7,8,9), fun="sum")
# PRISM.data.tmax <- subsetPRISM(locations=GHCN.meta.sp, prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="tmax", months=c(5,6,7,8,9), fun="mean")
# PRISM.data.tmin <- subsetPRISM(locations=GHCN.meta.sp, prism.bricks.dir=paste(MASTER.DATA,"PRISM/EXTRACTIONS/",sep=''), element="tmin", months=c(5,6,7,8,9), fun="mean")
# write.csv(PRISM.data.prcp,"../DATA/PRISM.data.prcp.csv")
# write.csv(PRISM.data.tmax,"../DATA/PRISM.data.tmax.csv")
# write.csv(PRISM.data.tmin,"../DATA/PRISM.data.tmin.csv")
PRISM.data.prcp <- read.csv("../DATA/PRISM.data.prcp.csv")
PRISM.data.tmax <- read.csv("../DATA/PRISM.data.tmax.csv")
PRISM.data.tmin <- read.csv("../DATA/PRISM.data.tmin.csv")
PRISM.data.prcp[,!(names(PRISM.data.prcp)=="YEAR")] <- PRISM.data.prcp[,!(names(PRISM.data.prcp)=="YEAR")]/10
PRISM.data.tavg <- as.data.frame((data.matrix(PRISM.data.tmax)+data.matrix(PRISM.data.tmin))/2)
PRISM.data.tavg$YEAR <- PRISM.data.tmax$YEAR

### Commented out for speed. Uncomment and run if changing data or running for first time!
## Load the ITRDB (tree ring) data, and crop to study period
# ITRDB.data <- getITRDB(raw.dir = paste(MASTER.DATA,"DENDRO/ITRDB/",sep=''), output.dir="../DATA/", type='standard', download=F, force.redo=T)
# ITRDB.meta <- read.csv("../DATA/ITRDB_METADATA.csv",colClasses="character")
# 
# ## Load tree ring chronologies not in the ITRDB, replacing the ITRDB series they supercede
# # Load the Stahle et al. 2013 dataset
# # Data derived from http://www.uark.edu/dendro/MVdata
# STAHLE2013 <- read.csv(paste(MASTER.DATA,"DENDRO/STAHLE2013.csv", sep=''))[,c("YEAR","STD")]
# STAHLE2013$STD <- STAHLE2013$STD/mean(STAHLE2013$STD, na.rm=T)
# names(STAHLE2013) <- c("YEAR","CO_MVDF2013")
# ITRDB.data <- merge(ITRDB.data,STAHLE2013,by=c("YEAR"),all=T)
# ITRDB.data <- ITRDB.data[,!(names(ITRDB.data) %in% "CO021")]
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="CO_MVDF2013",NAME="MESA VERDE",SPECIES="PSME",ELEVATION=2100,LAT=37.183667,LON=-108.487879,START=480,END=2008,CONTRIBUTOR="MATTHEW SALZER"))
# ITRDB.meta <- ITRDB.meta[!(ITRDB.meta$SERIES %in% c("CO021")),]
# 
# # Load the Towner and Salzer 2013 dataset
# # Tree ring chronologies provided by Matt Salzer, Sept. 6, 2013
# TOWNER2013 <- read.csv(paste(MASTER.DATA,"DENDRO/TOWNER2013.csv", sep=''))
# names(TOWNER2013) <- c("YEAR","NM_AM2013","NM_JZ2013","NM_AH2013")
# TOWNER2013$NM_AM2013 <- TOWNER2013$NM_AM2013/mean(TOWNER2013$NM_AM2013, na.rm=T)
# TOWNER2013$NM_JZ2013 <- TOWNER2013$NM_JZ2013/mean(TOWNER2013$NM_JZ2013, na.rm=T)
# TOWNER2013$NM_AH2013 <- TOWNER2013$NM_AH2013/mean(TOWNER2013$NM_AH2013, na.rm=T)
# ITRDB.data <- merge(ITRDB.data,TOWNER2013,by=c("YEAR"),all=T)
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="NM_AM2013",NAME="ALTA MESA",SPECIES="PSME",ELEVATION=2724,LAT=36.225599,LON=-106.658080,START=759,END=2002,CONTRIBUTOR="RON TOWNER"))
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="NM_JZ2013",NAME="JEMEZ",SPECIES="PSME",ELEVATION=1707,LAT=35.613880,LON=-106.725826,START=598,END=2002,CONTRIBUTOR="RON TOWNER"))
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="NM_AH2013",NAME="ARROYO HONDO",SPECIES="PSME",ELEVATION=2182,LAT=35.619451,LON=-105.921547,START=985,END=2002,CONTRIBUTOR="RON TOWNER"))
# 
# 
# # Load the Salzer and Kipfmueller 2005 datasets
# # Data provided by Matt Salzer, Dec. 11, 2013
# SALZER2005 <- read.csv(paste(MASTER.DATA,"DENDRO/SALZER2005.csv", sep=''))[,c("YEAR","SALZER2005_SFP","SALZER2005_ALMAGRE")]
# names(SALZER2005) <- c("YEAR","AZ_SFP2005","CO_ALM2005")
# SALZER2005$AZ_SFP2005 <- SALZER2005$AZ_SFP2005/mean(SALZER2005$AZ_SFP2005, na.rm=T)
# SALZER2005$CO_ALM2005 <- SALZER2005$CO_ALM2005/mean(SALZER2005$CO_ALM2005, na.rm=T)
# ITRDB.data <- merge(ITRDB.data,SALZER2005,by=c("YEAR"),all=T)
# ITRDB.data <- ITRDB.data[,!(names(ITRDB.data) %in% c("AZ510","CO524"))]
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="AZ_SFP2005",NAME="SAN FRANCISCO PEAKS",SPECIES="PIAR",ELEVATION=2682,LAT=35.316667,LON=-111.716667,START=-662,END=2002,CONTRIBUTOR="MATTHEW SALZER"))
# ITRDB.meta <- rbind(ITRDB.meta,list(SERIES="CO_ALM2005",NAME="ALMAGRE",SPECIES="PIAR",ELEVATION=3535,LAT=38.766667,LON=-104.966667,START=-568,END=1994,CONTRIBUTOR="MATTHEW SALZER"))
# ITRDB.meta <- ITRDB.meta[!(ITRDB.meta$SERIES %in% c("AZ510","CO524")),]
# 
# # Write the amended ITRDB metadata
# write.csv(ITRDB.meta,"../DATA/ITRDB_METADATA.csv", row.names=F)
# 
# ## Get a SPDF of ITRDB metadata
# ITRDB.meta.sp <- getITRDBMetadataSPDF(names(ITRDB.data),data.dir="../DATA/")
# 
# ## Get a shapefile of the 48 CONUS states
# states <- readOGR("/Volumes/BOCINSKY_DATA/DATA/NATIONAL_ATLAS/statep010", layer='statep010')
# # And select only the four corners
# states <- states[states$STATE %in% c("Arizona","Colorado","Utah","New Mexico"),]
# states <- spTransform(states,CRS(projection(ITRDB.meta.sp)))
# 
# ## Trim the IRTB database to the four corners states
# ITRDB.meta.sp <- ITRDB.meta.sp[as.vector(!is.na((ITRDB.meta.sp %over% states)[,1])),]
# # Select only the four corners series
# ITRDB.data <- ITRDB.data[,c("YEAR",as.character(ITRDB.meta.sp$SERIES))]
# # Remove years with no data
# ITRDB.data <- ITRDB.data[apply(ITRDB.data,1,FUN=function(i){any(!is.na(i[-1]))}),]
# 
# ITRDB.meta.sp$NAME <- sanitizeITRDBnames(ITRDB.meta.sp$NAME)
# 
# # Write the amended ITRDB data and metadata
# write.csv(ITRDB.data,"../DATA/ITRDB_DATA.csv",row.names=F)
# write.csv(ITRDB.meta.sp,"../DATA/ITRDB_METADATA.csv", row.names=F)

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
# Add the data from Wright 2006
WRIGHT2006 <- read.csv(paste(MASTER.DATA,"POLLEN/WRIGHT2006.csv",sep=''))
WRIGHT2006[is.na(WRIGHT2006)] <- 0
SWUS.F70.data.append <- rbind(SWUS.F70.data,WRIGHT2006)
SWUS.F70.data.append.meta <- unique(SWUS.F70.data.append[SWUS.F70.data.append$YEAR>0,"NAME"])

## Generate a map of the study area, with pollen, dendros, and GHCN sites represented
source('../FIGURES/R/SW_MAP.R')

######## FINISH DATA IMPORT ########

######## BEGIN ANALYSIS ########

#### Correlations between PRISM AND GHCN datasets ####
pairwise.corr <- function(name,x,y){
  if(name %in% names(x) & name %in% names(y)){
    x <- x[,c("YEAR",name)]
    y <- y[,c("YEAR",name)]
    xy <- merge(x,y,by="YEAR")
    xy <- xy[,names(xy)!="YEAR"]
    xy.cor <- cor(xy,use="complete.obs")[1,2]
    return(xy.cor)
  }else{
    return()
  }
}

prcp.corr <- lapply(GHCN.stations,FUN=function(name,...){pairwise.corr(name,x=GHCN.data.prcp[GHCN.data.prcp$YEAR %in% calibration.years,],y=PRISM.data.prcp[PRISM.data.prcp$YEAR %in% calibration.years,])})
tmax.corr <- lapply(GHCN.stations,FUN=function(name,...){pairwise.corr(name,x=GHCN.data.tmax[GHCN.data.tmax$YEAR %in% calibration.years,],y=PRISM.data.tmax[PRISM.data.tmax$YEAR %in% calibration.years,])})
tmin.corr <- lapply(GHCN.stations,FUN=function(name,...){pairwise.corr(name,x=GHCN.data.tmin[GHCN.data.tmin$YEAR %in% calibration.years,],y=PRISM.data.tmin[PRISM.data.tmin$YEAR %in% calibration.years,])})
tavg.corr <- lapply(GHCN.stations,FUN=function(name,...){pairwise.corr(name,x=GHCN.data.tavg[GHCN.data.tavg$YEAR %in% calibration.years,],y=PRISM.data.tavg[PRISM.data.tavg$YEAR %in% calibration.years,])})

## Mean absolute differences between PRISM AND GHCN datasets
pairwise.MAPE <- function(name,x,y){
  if(name %in% names(x) & name %in% names(y)){
    x <- x[,c("YEAR",name)]
    y <- y[,c("YEAR",name)]
    xy <- merge(x,y,by="YEAR")
    xy <- xy[,names(xy)!="YEAR"]
    xy <- xy[complete.cases(xy),]
    #     xy.z <- scale(c(xy[,1],xy[,2]))
    #     xy[,1] <- xy.z[1:(length(xy.z)/2)]
    #     xy[,2] <- xy.z[((length(xy.z)/2)+1):length(xy.z)]
    xy.MAPE <- mape(xy[,1],xy[,2])
    return(xy.MAPE)
  }else{
    return()
  }
}

prcp.MAPE <- lapply(GHCN.stations,FUN=function(name,...){pairwise.MAPE(name,x=GHCN.data.prcp[GHCN.data.prcp$YEAR %in% calibration.years,],y=PRISM.data.prcp[PRISM.data.prcp$YEAR %in% calibration.years,])})
tmax.MAPE <- lapply(GHCN.stations,FUN=function(name,...){pairwise.MAPE(name,x=GHCN.data.tmax[GHCN.data.tmax$YEAR %in% calibration.years,],y=PRISM.data.tmax[PRISM.data.tmax$YEAR %in% calibration.years,])})
tmin.MAPE <- lapply(GHCN.stations,FUN=function(name,...){pairwise.MAPE(name,x=GHCN.data.tmin[GHCN.data.tmin$YEAR %in% calibration.years,],y=PRISM.data.tmin[PRISM.data.tmin$YEAR %in% calibration.years,])})
tavg.MAPE <- lapply(GHCN.stations,FUN=function(name,...){pairwise.MAPE(name,x=GHCN.data.tavg[GHCN.data.tavg$YEAR %in% calibration.years,],y=PRISM.data.tavg[PRISM.data.tavg$YEAR %in% calibration.years,])})

source('../TABLES/R/PRISM_GHCN_CORRS.R')
source('../TABLES/R/PRISM_GHCN_MAPE.R')
source('../FIGURES/R/PRISM_GHCN_PRECIP.R')
source('../FIGURES/R/PRISM_GHCN_TEMP.R')

#### Best CAR regression model for prism series each series ####
series <- ITRDB.data[ITRDB.data$YEAR %in% calibration.years,]
series <- series[,t(complete.cases(t(series)))]
series <- series[,-1]

## Regressing temperature on all dendro chronologies 
calcCV <- function(x, Y.vector, type){
  regularize <- TRUE
  cat("CAR Regression ",type," ~",GHCN.stations.abbrev[x],"\n")
  series.slm.cv <- cv.slm.models(Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], Xtrain=series, max.series=50, K=2, R=99, type="random", regularize=regularize)
  save(series.slm.cv,file=paste("../OUTPUT/",GHCN.stations.abbrev[x],"_",type,"_CV.Rdata", sep=''))
  load(paste("../OUTPUT/",GHCN.stations.abbrev[x],"_",type,"_CV.Rdata", sep=''))
  series.slm.cv.best <- getBestFit(series.slm.cv, max.variables=NULL)
  
  choice <- series.slm.cv.best["RMSE"]
  
  stackBoxplot(series.slm.cv,file=paste("../FIGURES/",GHCN.stations.abbrev[x],"_",type,"_CV.pdf", sep=''),labels=list(expression(R[c]^2),expression(R[v]^2), expression(RMSE),expression(RE),expression(CE)), choice=choice,  fig.height=6, between=0.15)
  
  final.slm <- series.slm.cv$MODELS[[choice]]
  
  return(list(MODELS=series.slm.cv$MODELS, CAR=series.slm.cv$CAR, REG=series.slm.cv$REG, LOCATION=GHCN.stations[x], SLM.final=final.slm))
}

# prcp.slms <- lapply(1:length(GHCN.stations),FUN=function(i,...){calcCV(i,Y.vector=PRISM.data.prcp, type="PRCP")})
# names(prcp.slms) <- GHCN.stations.abbrev
# save(prcp.slms,file="../OUTPUT/PRCP_SLMS.Rdata")
# 
# tavg.slms <- lapply(1:length(GHCN.stations),FUN=function(i,...){calcCV(i,Y.vector=PRISM.data.tavg, type="TAVG")})
# names(tavg.slms) <- GHCN.stations.abbrev
# save(tavg.slms,file="../OUTPUT/TAVG_SLMS.Rdata")


load("../OUTPUT/PRCP_SLMS.Rdata")
load("../OUTPUT/TAVG_SLMS.Rdata")

## Generate a map showing the dendros selected from each station
source('../FIGURES/R/CAR_SELECTION_PRCP.R')
## ... and a table showing the same
source('../TABLES/R/CAR_SELECTION_PRCP.R')
## Generate a map showing the dendros selected from each station
source('../FIGURES/R/CAR_SELECTION_TAVG.R')
## ... and a table showing the same
source('../TABLES/R/CAR_SELECTION_TAVG.R')


#### Comparison of CAR results with other reconstruction methods ####
## The other methods to be tested are single-chronology regression (best CAR score),
## single-chronology regression (nearest chronology),
## mean variance matching (best CAR score),
## and mean-variance matching (nearest chronology).
compare.methods <- function(x, Y.vector, slms, K=2, R=99, type="random"){
  # Get nearest chronology to reconstruction site
  station.location <- spTransform(GHCN.meta.sp[GHCN.meta.sp$ID==GHCN.stations[x],],CRS("+proj=utm +datum=NAD83 +zone=12"))
  series.locations <- spTransform(ITRDB.meta.sp,CRS("+proj=utm +datum=NAD83 +zone=12"))
  series.locations <- series.locations[series.locations$SERIES %in% names(series),]
  nearest.series <- as.character(series.locations$SERIES)[which(gDistance(station.location,series.locations, byid=T)==min(gDistance(station.location,series.locations, byid=T)))][1]
  
  folds <- cvFolds(nrow(series), K=K, R=R, type=type)
  folds.out <- unlist(lapply(1:folds$K,FUN=function(x,...){ getFolds(folds=folds,i=x) }), recursive=F)
  if(type!="consecutive" & length(folds.out)<=R & K!=nrow(series)){
    folds.out <- unlist(folds.out, recursive=F)
  }
  
  # Get all CAR scores
  series.car <- slms[[x]]$CAR
  
  # Calculate CV stats for the best CAR regression
  all.car.cv <- cv.slm(Xtrain=series[,colnames(slms[[x]]$SLM.final$CAR)], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", lambda=slms[[x]]$REG[1],lambda.var=slms[[x]]$REG[2], fold.list=folds.out)
  all.car.CV_stats <- colMedians(all.car.cv$ERROR)
  
  # Calculate CV stats for the best CAR regression + mean/variance matching
  all.car.meanVar.cv <- cv.slm(Xtrain=series[,colnames(slms[[x]]$SLM.final$CAR)], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", lambda=slms[[x]]$REG[1],lambda.var=slms[[x]]$REG[2], meanVar=T, fold.list=folds.out)
  all.car.meanVar.CV_stats <- colMedians(all.car.meanVar.cv$ERROR)
  
  # Calculate CV stats for the single chronology regression using the chronology with the highest CAR^2 score
  best.car.lm.cv <- cv.lm(Xtrain=series[,names(series.car)[1]], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", fold.list=folds.out)
  best.car.lm.CV_stats <- colMedians(best.car.lm.cv)
  
  # Calculate CV stats for the single chronology regression using the nearest chronology to the reconstruction site
  nearest.lm.cv <- cv.lm(Xtrain=series[,nearest.series], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", fold.list=folds.out)
  nearest.lm.CV_stats <- colMedians(nearest.lm.cv)
  
  # Calculate CV stats for the mean variance matching technique using the chronology with the highest CAR^2 score
  best.car.meanVar.CV_stats <- colMedians(cv.meanVariance(Xtrain=series[,names(series.car)[1]], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", fold.list=folds.out))
  
  # Calculate CV stats for the mean variance matching technique using the nearest chronology to the reconstruction site
  nearest.meanVar.CV_stats <- colMedians(cv.meanVariance(Xtrain=series[,nearest.series], Ytrain=Y.vector[Y.vector$YEAR %in% calibration.years,GHCN.stations[x]], K=2, R=99, type="random", fold.list=folds.out))
  
  return(do.call('rbind',list(all.car.CV_stats=all.car.CV_stats,all.car.meanVar.CV_stats=all.car.meanVar.CV_stats,best.car.lm.CV_stats=best.car.lm.CV_stats,nearest.lm.CV_stats=nearest.lm.CV_stats,best.car.meanVar.CV_stats=best.car.meanVar.CV_stats,nearest.meanVar.CV_stats=nearest.meanVar.CV_stats)))
}
# prcp.compare <- lapply(1:length(GHCN.stations),FUN=function(i,...){compare.methods(i,Y.vector=PRISM.data.prcp, slms=prcp.slms)})
# tavg.compare <- lapply(1:length(GHCN.stations),FUN=function(i,...){compare.methods(i,Y.vector=PRISM.data.tavg, slms=tavg.slms)})
# save(prcp.compare,file="../OUTPUT/PRCP_COMPARE.Rdata")
# save(tavg.compare,file="../OUTPUT/TAVG_COMPARE.Rdata")

load("../OUTPUT/PRCP_COMPARE.Rdata")
load("../OUTPUT/TAVG_COMPARE.Rdata")

## Generate a table showing the results of the model comparison exercise
source('../TABLES/R/MODEL_COMPARE.R')


#### Best Reconstruction Over Available Data ####
## Beginning with the calibration period, start stepping backwards
getBROADslms <- function(station, Y.vector){
  cat("\n Calculating BROAD SLMs for",GHCN.stations.abbrev[station],"\n")  
  return(BROADrecon(Ytrain=Y.vector[,c("YEAR",GHCN.stations[station])], Xtrain=ITRDB.data, recon.years=1:2000, calib.years=calibration.years, max.series=10, K=2, R=99, type="random", regularize=T))
}
# prcp.BROAD.slms <- lapply(1:length(GHCN.stations),FUN=function(i,...){getBROADslms(i,Y.vector=PRISM.data.prcp)})
# names(prcp.BROAD.slms) <- GHCN.stations.abbrev
# tavg.BROAD.slms <- lapply(1:length(GHCN.stations),FUN=function(i,...){getBROADslms(i,Y.vector=PRISM.data.tavg)})
# names(tavg.BROAD.slms) <- GHCN.stations.abbrev
# save(prcp.BROAD.slms,file="../OUTPUT/PRCP_BROAD_SLMS.Rdata")
# save(tavg.BROAD.slms,file="../OUTPUT/TAVG_BROAD_SLMS.Rdata")

load("../OUTPUT/PRCP_BROAD_SLMS.Rdata")
load("../OUTPUT/TAVG_BROAD_SLMS.Rdata")

## ... and then do retrodictions, matching the mean and variance over the calibration period.
getBROADretro <- function(station,Y.vector, slms){
  cat("\n Retrodicting BROAD SLMs for",GHCN.stations.abbrev[station],"\n")
  return(BROADretro(Ytrain=Y.vector[,c("YEAR",GHCN.stations[station])], Xtrain=ITRDB.data, slms=slms[[station]], calib.years=calibration.years, meanVar=T))
}
# prcp.BROAD.retros <- lapply(1:length(GHCN.stations),FUN=function(station,...){getBROADretro(station=station,Y.vector=PRISM.data.prcp, slms=prcp.BROAD.slms)})
# names(prcp.BROAD.retros) <- GHCN.stations.abbrev
# tavg.BROAD.retros <- lapply(1:length(GHCN.stations),FUN=function(station,...){getBROADretro(station=station,Y.vector=PRISM.data.tavg, slms=tavg.BROAD.slms)})
# names(tavg.BROAD.retros) <- GHCN.stations.abbrev
# save(prcp.BROAD.retros,file="../OUTPUT/PRCP_BROAD_RETROS.Rdata")
# save(tavg.BROAD.retros,file="../OUTPUT/TAVG_BROAD_RETROS.Rdata")

load("../OUTPUT/PRCP_BROAD_RETROS.Rdata")
load("../OUTPUT/TAVG_BROAD_RETROS.Rdata")

#### Low frequency modulation via continuous wavelet transformation ####
## First, we need a reconstruction of low frequency regional temperature change in the SWUS
## We use the modern analogue technique (MAT).
SWUS.MAT.retro <- getMAT(fossil.data=SWUS.F70.data,modern.data=M70.data,recon.years=1:2000)
CONUS.MAT.retro <- getMAT(fossil.data=F70.data,modern.data=M70.data,recon.years=1:2000)
SWUS.MAT.retro.append <- getMAT(fossil.data=SWUS.F70.data.append,modern.data=M70.data,recon.years=1:2000)

## Modulate the temperature curves using the SWUS MAT reconstruction
CWTmodulation <- function(station, retros, MAT, climate, calib.years){
  climate <- climate[,c("YEAR",GHCN.stations[station])]
  # First, get the deviation from the mean
  modern.climate <- mean(climate[climate$YEAR %in% 1961:1990,2])
  
  high.freq.dev <- high.freq <- retros[[GHCN.stations.abbrev[station]]]
  high.freq.dev[,2:4] <- high.freq.dev[,2:4] - modern.climate
  
  retro <- CWT_combine_series(high.freq$RETRO,MAT$y)
  lower <- CWT_combine_series(high.freq$LOWER,MAT$y)
  upper <- CWT_combine_series(high.freq$UPPER,MAT$y)
  
  modulated <- data.frame(YEAR=high.freq$YEAR, RETRO=retro, LOWER=lower, UPPER=upper)
  
  # Rescale to calibration data
  modulated$RETRO <- meanVarianceTuning(prediction=modulated$RETRO[modulated$YEAR %in% calib.years], training=high.freq$RETRO[high.freq$YEAR %in% calib.years], x.validation=modulated$RETRO)
  modulated$LOWER <- meanVarianceTuning(prediction=modulated$LOWER[modulated$YEAR %in% calib.years], training=high.freq$LOWER[high.freq$YEAR %in% calib.years], x.validation=modulated$LOWER)
  modulated$UPPER <- meanVarianceTuning(prediction=modulated$UPPER[modulated$YEAR %in% calib.years], training=high.freq$UPPER[high.freq$YEAR %in% calib.years], x.validation=modulated$UPPER)

  
  return(modulated)
}
# tavg.BROAD.retros.CWT.SWUS <- lapply(1:length(GHCN.stations),FUN=function(station,...){CWTmodulation(station=station, retros=tavg.BROAD.retros, MAT=SWUS.MAT.retro, climate=PRISM.data.tavg, calib.years=calibration.years)})
# names(tavg.BROAD.retros.CWT.SWUS) <- GHCN.stations.abbrev
# save(tavg.BROAD.retros.CWT.SWUS,file="../OUTPUT/TAVG_BROAD_RETROS_CWT_SWUS.Rdata")
# 
# tavg.BROAD.retros.CWT.CONUS <- lapply(1:length(GHCN.stations),FUN=function(station,...){CWTmodulation(station=station, retros=tavg.BROAD.retros, MAT=CONUS.MAT.retro, climate=PRISM.data.tavg, calib.years=calibration.years)})
# names(tavg.BROAD.retros.CWT.CONUS) <- GHCN.stations.abbrev
# save(tavg.BROAD.retros.CWT.CONUS,file="../OUTPUT/TAVG_BROAD_RETROS_CWT_CONUS.Rdata")

load("../OUTPUT/TAVG_BROAD_RETROS_CWT_SWUS.Rdata")
load("../OUTPUT/TAVG_BROAD_RETROS_CWT_CONUS.Rdata")

## Generate figures showing the results of the CWT
source('../FIGURES/R/CWT_SWUS_GRAPH.R')
source('../FIGURES/R/CWT_CONUS_GRAPH.R')

source('../FIGURES/R/FINAL_RECONS_SWUS.R')
source('../FIGURES/R/FINAL_RECONS_CONUS.R')


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

#### Spatial retrodictions of all climate signals ####
## Unlike the analyses in Part I, we cannot use BROAD reconstruction 
## because it will take entirely too long. Thus, we economize by not
## using cross-validation, and only generating a reconstruction using
## the top "n" dendrochronologies, as determined by their squared CAR scores.
## Again, this takes awhile (>10 hours). Run it only once, then load the saved files.
## ... then do the retrodictions.

# PRISM.data.h2o_year.prcp.bricks.slms <- lapply(PRISM.data.h2o_year.prcp.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# save(PRISM.data.h2o_year.prcp.bricks.slms,file="../OUTPUT/PRISM_data_h2o_year_prcp_bricks_slms.Rdata")
# load("../OUTPUT/PRISM_data_h2o_year_prcp_bricks_slms.Rdata")
# PRISM.data.h2o_year.prcp.bricks.retro <- lapply(1:length(PRISM.data.h2o_year.prcp.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.h2o_year.prcp.bricks.slms[[i]], Ytrain.brick=PRISM.data.h2o_year.prcp.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T) })
# save(PRISM.data.h2o_year.prcp.bricks.retro,file="../OUTPUT/PRISM_data_h2o_year_prcp_bricks_retro.Rdata")
# load("../OUTPUT/PRISM_data_h2o_year_prcp_bricks_retro.Rdata")
# rm(PRISM.data.h2o_year.prcp.bricks.slms)
# rm(PRISM.data.h2o_year.prcp.bricks.retro)

# PRISM.data.cool.prcp.bricks.slms <- lapply(PRISM.data.cool.prcp.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# save(PRISM.data.cool.prcp.bricks.slms,file="../OUTPUT/PRISM_data_cool_prcp_bricks_slms.Rdata")
# load("../OUTPUT/PRISM_data_cool_prcp_bricks_slms.Rdata")
# PRISM.data.cool.prcp.bricks.retro <- lapply(1:length(PRISM.data.cool.prcp.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.cool.prcp.bricks.slms[[i]], Ytrain.brick=PRISM.data.cool.prcp.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T) })
# save(PRISM.data.cool.prcp.bricks.retro,file="../OUTPUT/PRISM_data_cool_prcp_bricks_retro.Rdata")
# load("../OUTPUT/PRISM_data_cool_prcp_bricks_retro.Rdata")
# rm(PRISM.data.cool.prcp.bricks.slms)
# rm(PRISM.data.cool.prcp.bricks.retro)

# PRISM.data.grow.prcp.bricks.slms <- lapply(PRISM.data.grow.prcp.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# save(PRISM.data.grow.prcp.bricks.slms,file="../OUTPUT/PRISM_data_grow_prcp_bricks_slms.Rdata")
# load("../OUTPUT/PRISM_data_grow_prcp_bricks_slms.Rdata")
# PRISM.data.grow.prcp.bricks.retro <- lapply(1:length(PRISM.data.grow.prcp.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.grow.prcp.bricks.slms[[i]], Ytrain.brick=PRISM.data.grow.prcp.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T, mc=F) })
# save(PRISM.data.grow.prcp.bricks.retro,file="../OUTPUT/PRISM_data_grow_prcp_bricks_retro.Rdata")
# load("../OUTPUT/PRISM_data_grow_prcp_bricks_retro.Rdata")
# rm(PRISM.data.grow.prcp.bricks.slms)
# rm(PRISM.data.grow.prcp.bricks.retro)

# PRISM.data.grow.tmax.bricks.slms <- lapply(PRISM.data.grow.tmax.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# save(PRISM.data.grow.tmax.bricks.slms,file="../OUTPUT/PRISM_data_grow_tavg_bricks_slms.Rdata")
# gc()
# load("../OUTPUT/PRISM_data_grow_tmax_bricks_slms.Rdata")
# PRISM.data.grow.tmax.bricks.retro <- lapply(1:length(PRISM.data.grow.tmax.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.grow.tmax.bricks.slms[[i]], Ytrain.brick=PRISM.data.grow.tmax.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T, mc=F) })
# save(PRISM.data.grow.tmax.bricks.retro,file="../OUTPUT/PRISM_data_grow_tmax_bricks_retro.Rdata")
# # load("../OUTPUT/PRISM_data_grow_tmax_bricks_retro.Rdata")
# rm(PRISM.data.grow.tmax.bricks.slms)
# rm(PRISM.data.grow.tmax.bricks.retro)
# gc()
# 
# PRISM.data.grow.tmin.bricks.slms <- lapply(PRISM.data.grow.tmin.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# save(PRISM.data.grow.tmin.bricks.slms,file="../OUTPUT/PRISM_data_grow_tmin_bricks_slms.Rdata")
# gc()
# load("../OUTPUT/PRISM_data_grow_tmin_bricks_slms.Rdata")
# PRISM.data.grow.tmin.bricks.retro <- lapply(1:length(PRISM.data.grow.tmin.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.grow.tmin.bricks.slms[[i]], Ytrain.brick=PRISM.data.grow.tmin.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T, mc=F) })
# save(PRISM.data.grow.tmin.bricks.retro,file="../OUTPUT/PRISM_data_grow_tmin_bricks_retro.Rdata")
# # load("../OUTPUT/PRISM_data_grow_tmin_bricks_retro.Rdata")
# rm(PRISM.data.grow.tmin.bricks.slms)
# rm(PRISM.data.grow.tmin.bricks.retro)
# gc()
# 
# # PRISM.data.grow.GDD.bricks.slms <- lapply(PRISM.data.grow.GDD.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# # save(PRISM.data.grow.GDD.bricks.slms,file="../OUTPUT/PRISM_data_grow_GDD_bricks_slms.Rdata")
# # load("../OUTPUT/PRISM_data_grow_GDD_bricks_slms.Rdata")
# # PRISM.data.grow.GDD.bricks.retro <- lapply(1:length(PRISM.data.grow.GDD.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.grow.GDD.bricks.slms[[i]], Ytrain.brick=PRISM.data.grow.GDD.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T, mc=F) })
# # save(PRISM.data.grow.GDD.bricks.retro,file="../OUTPUT/PRISM_data_grow_GDD_bricks_retro.Rdata")
# # load("../OUTPUT/PRISM_data_grow_GDD_bricks_retro.Rdata")
# # rm(PRISM.data.grow.GDD.bricks.slms)
# # rm(PRISM.data.grow.GDD.bricks.retro)
# # gc()
# 
# # PRISM.data.grow.tavg.bricks.slms <- lapply(PRISM.data.grow.tavg.bricks, FUN=function(Ytrain.brick,...){ BROADrecon.brick(Ytrain.brick=Ytrain.brick, Xtrain=ITRDB.data, Ytrain.brick.years=1924:1983, recon.years=1:2000, calib.years=1924:1983, static.count=NULL, breaks=c(1,201,401,801,1001,1201,1401,1601,1801,1984), max.series=NULL, K=1, R=1, type="random", regularize=TRUE, mc=T) })
# # save(PRISM.data.grow.tavg.bricks.slms,file="../OUTPUT/PRISM_data_grow_tavg_bricks_slms.Rdata")
# # load("../OUTPUT/PRISM_data_grow_tavg_bricks_slms.Rdata")
# # PRISM.data.grow.tavg.bricks.retro <- lapply(1:length(PRISM.data.grow.tavg.bricks.slms), FUN=function(i,...){ BROADretro.brick(brick.slms=PRISM.data.grow.tavg.bricks.slms[[i]], Ytrain.brick=PRISM.data.grow.tavg.bricks[[i]], Xtrain=ITRDB.data, calib.years=1924:1983, recon.years=1:2000, meanVar=T, mc=F) })
# # save(PRISM.data.grow.tavg.bricks.retro,file="../OUTPUT/PRISM_data_grow_tavg_bricks_retro.Rdata")
# # load("../OUTPUT/PRISM_data_grow_tavg_bricks_retro.Rdata")
# # rm(PRISM.data.grow.tavg.bricks.slms)
# # rm(PRISM.data.grow.tavg.bricks.retro)
# 
# # load("../OUTPUT/PRISM_data_cool_prcp_bricks_retro.Rdata")
# # lapply(1:length(PRISM.data.cool.prcp.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.cool.prcp.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_prcp_cool_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# # rm(PRISM.data.cool.prcp.bricks.retro)
# # 
# # load("../OUTPUT/PRISM_data_grow_prcp_bricks_retro.Rdata")
# # lapply(1:length(PRISM.data.grow.prcp.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.grow.prcp.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_prcp_grow_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# # rm(PRISM.data.grow.prcp.bricks.retro)
# 
# load("../OUTPUT/PRISM_data_grow_tmax_bricks_retro.Rdata")
# lapply(1:length(PRISM.data.grow.tmax.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.grow.tmax.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_tmax_grow_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# rm(PRISM.data.grow.tmax.bricks.retro)
# 
# load("../OUTPUT/PRISM_data_grow_tmin_bricks_retro.Rdata")
# lapply(1:length(PRISM.data.grow.tmin.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.grow.tmin.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_tmin_grow_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# rm(PRISM.data.grow.tavg.bricks.retro)

# load("../OUTPUT/PRISM_data_grow_GDD_bricks_retro.Rdata")
# lapply(1:length(PRISM.data.grow.GDD.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.grow.GDD.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_grow_GDD_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# rm(PRISM.data.grow.GDD.bricks.retro)

# load("../OUTPUT/PRISM_data_h2o_year_prcp_bricks_retro.Rdata")
# lapply(1:length(PRISM.data.h2o_year.prcp.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.h2o_year.prcp.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_h2o_year_prcp_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# rm(PRISM.data.h2o_year.bricks.retro)

# load("../OUTPUT/PRISM_data_grow_tavg_bricks_retro.Rdata")
# lapply(1:length(PRISM.data.grow.tavg.bricks.retro), FUN=function(i){ writeRaster(PRISM.data.grow.tavg.bricks.retro[[i]],paste("../OUTPUT/",area.names[i],"_tavg_grow_retro.tif", sep=''),options=c("COMPRESS=DEFLATE","INTERLEAVE=BAND","PROFILE=GDALGeoTIFF","ZLEVEL=9"), dataType='FLT4S', overwrite=TRUE)})
# rm(PRISM.data.grow.tavg.bricks.retro)


## Get PRISM daily data, and monthly norms (at 4km resolution), and get local relationship between data and norms
## Extract the VEPIIN area.
# sim.poly.VEPIIN <- createArea(North = 4170000, South = 4102000, East = 740000, West = 672800, projection.string = "+proj=utm +datum=NAD83 +zone=12")
# system.time(extractPRISM_DAILY(object=sim.poly.VEPIIN, out.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/", daily.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/DAILY/", label="VEPIIN", year.range=2008:2013, force.redo=T))
# gc()
# 
# ## Extract the VEPIIS area.
# sim.poly.VEPIIS <- createArea(North = 4030400, South = 3939600, East = 435800, West = 359200, projection.string = "+proj=utm +datum=NAD83 +zone=13")
# extractPRISM_DAILY(sim.poly.VEPIIS, out.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/", daily.dir="/Volumes/BOCINSKY_DATA/DATA/PRISM/DAILY/", label="VEPIIS", year.range=1981:2010, force.redo=T)
# gc()

# source('./WEATHER_FUNCTIONS.R')
# 
# VEPIIN.PRISM.tmax.daily.growing.norms <- getDailyNorms(extraction="VEPIIN",file="tmax_DAILY_FINAL_800",fun="mean")
# VEPIIN.PRISM.tmin.daily.growing.norms <- getDailyNorms(extraction="VEPIIN",file="tmin_DAILY_FINAL_800",fun="mean")
# VEPIIN.PRISM.prcp.daily.growing.norms <- getDailyNorms(extraction="VEPIIN",file="ppt_DAILY_FINAL_800",fun="sum")
# VEPIIS.PRISM.tmax.daily.growing.norms <- getDailyNorms(extraction="VEPIIS",file="tmax_DAILY_FINAL_800",fun="mean")
# VEPIIS.PRISM.tmin.daily.growing.norms <- getDailyNorms(extraction="VEPIIS",file="tmin_DAILY_FINAL_800",fun="mean")
# VEPIIS.PRISM.prcp.daily.growing.norms <- getDailyNorms(extraction="VEPIIS",file="ppt_DAILY_FINAL_800",fun="sum")


# test <- data.frame(index=1:length(as.numeric(VEPIIN.PRISM.prcp.daily.growing.norms[[1]][1])), series=as.numeric(VEPIIN.PRISM.prcp.daily.growing.norms[[1]][1]))
# test.lo <- loess(series~index, test)
# plot(test)
# lines(predict(test.lo, se = TRUE)$fit)


# load("../OUTPUT/PRISM_data_grow_tmax_bricks_retro.Rdata")
# daily.test <- as.numeric(VEPIIN.PRISM.tmax.daily.growing.norms[1])


# # Growing season recon: May--September, day 121--273
# load("../OUTPUT/PRISM_data_grow_tmax_bricks_retro.Rdata")
# load("../OUTPUT/PRISM_data_grow_tmin_bricks_retro.Rdata")
# load("../OUTPUT/PRISM_data_grow_prcp_bricks_retro.Rdata")
# annual.brick.tmin <- PRISM.data.grow.tmin.bricks.retro[[1]]
# annual.brick.tmax <- PRISM.data.grow.tmax.bricks.retro[[1]]
# annual.brick.prcp <- PRISM.data.grow.prcp.bricks.retro[[1]]
# annual.brick.prcp[annual.brick.prcp<0] <- 0
# daily.brick.tmin <- brick(readGDAL(paste("/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/VEPIIN/tmin_DAILY_FINAL_800.tif",sep=''), silent = TRUE))[[1:365]]
# daily.brick.tmax <- brick(readGDAL(paste("/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/VEPIIN/tmax_DAILY_FINAL_800.tif",sep=''), silent = TRUE))[[1:365]]
# daily.brick.prcp <- brick(readGDAL(paste("/Volumes/BOCINSKY_DATA/DATA/PRISM/EXTRACTIONS/VEPIIN/ppt_DAILY_FINAL_800.tif",sep=''), silent = TRUE))[[1:365]]
# 
# annual.brick.tmin.rounded <- round(annual.brick.tmin,digits=0)
# annual.brick.tmax.rounded <- round(annual.brick.tmax,digits=0)
# annual.brick.prcp.rounded <- round(annual.brick.prcp,digits=0)
# 
# all.df <- as.data.frame(t(do.call('rbind',list(as.numeric(getValues(annual.brick.tmin.rounded)),as.numeric(getValues(annual.brick.tmax.rounded)),as.numeric(getValues(annual.brick.prcp.rounded))))))
# all.df <- unique(all.df)










# test.out <- prepDSSATWeather.bricks(out.dir="~/Desktop/WEATHER/", annual.brick.tmin,annual.brick.tmax,annual.brick.prcp, daily.brick.tmin, daily.brick.tmax, daily.brick.prcp, recon.days, recon.years)
# 
# daily.test <- daily[1]
# annual.test <- as.numeric(PRISM.data.grow.tmax.bricks.retro[[1]][1])
# 
# test.out <- retrodictDailySeries(annual.series=annual.test, annual.daily.training=daily.test, recon.days=c(121:273), recon.years=c(1:2000), fun="mean")
# 
# coordinates(daily.test)
# 
# 
# 
# mean(test.out$SERIES[test.out$YEAR==1])
# plot(test.out$SERIES[test.out$YEAR %in% c(1:20)], type="l")
# lines(rep(annual.test[1:20],each=length(c(121:273))), col="red")
# 
# 
# 
# load("../OUTPUT/PRISM_data_grow_prcp_bricks_retro.Rdata")
# daily.test <- as.numeric(VEPIIN.PRISM.prcp.daily.growing.norms[[1]][1])/10
# annual.test <- as.numeric(PRISM.data.grow.prcp.bricks.retro[[1]][1])
# 
# # Growing season recon: May--September, day 121--273
# test.out <- retrodictDailySeries(annual.series=annual.test, annual.daily.training=daily.test, recon.days=c(121:273), recon.years=c(1:2000), fun="sum")
# sum(test.out$SERIES[test.out$YEAR==1])
# annual.test[1]
# plot(test.out$SERIES[test.out$YEAR %in% c(1:20)], type="l")
# lines(rep(annual.test[1:20],each=length(c(121:273))), col="red")



source('./DSSAT_FUNCTIONS.R')
sim.poly.VEPIIN <- createArea(North = 4170000, South = 4102000, East = 740000, West = 672800, projection.string = "+proj=utm +datum=NAD83 +zone=12")
DSSAT.spatial(template=sim.poly.VEPIIN, label="VEPIIN", raw.dir="/Volumes/BOCINSKY_DATA/DATA/", force.redo=F)

source('./DSSAT_FUNCTIONS.R')
sim.poly.CCAC <- createArea(South = 37.3375, North = 37.37472, West = -108.6578, East = -108.6114, projection.string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
production.out.brick <- DSSAT.spatial(template=sim.poly.CCAC, label="CCAC", raw.dir=MASTER.DATA, force.redo=F)
writeGDAL(as(production.out.brick, "SpatialGridDataFrame"), "../DATA/CCAC_PROD_BRICK.tif", drivername="GTiff", type="Float32", options=c("INTERLEAVE=PIXEL", "COMPRESS=DEFLATE", "ZLEVEL=9"))
write.csv(names(production.out.brick),"../DATA/CCAC_PROD_BRICK_BANDS.csv", row.names=F, col.names=F)


plot(as.numeric(cellStats(production.out.brick,mean)), type='l')



CCAC.kml <- spTransform(sim.poly.CCAC,CRS("+proj=longlat +ellps=WGS84 +datum=NAD83 +no_defs"))
suppressWarnings(writeOGR(CCAC.kml,"/Users/bocinsky/Desktop/CCAC.kml","CCAC",driver="KML", overwrite_layer=TRUE))

NRCS.CCAC <- extractNRCS(template=sim.poly.CCAC, label="CCAC", raw.dir=paste(path.expand(MASTER.DATA),"NRCS/",sep=''), force.redo=F)
NRCS.CCAC.kml <- spTransform(NRCS.CCAC,CRS("+proj=longlat +ellps=WGS84 +datum=NAD83 +no_defs"))
suppressWarnings(writeOGR(NRCS.CCAC.kml,"/Users/bocinsky/Desktop/NRCS_CCAC.kml","NRCS_CCAC",driver="KML", overwrite_layer=TRUE))



plots <- read.csv("~/IMPORTANT/DISSERTATION/PALEOPROD/PFP/PFP_PLOTS.csv")
plots <- SpatialPointsDataFrame(coords=plots[,c("UTMEast","UTMNorth")],data=plots,proj4string=CRS("+proj=utm +datum=NAD83 +zone=12"))
plots <- spTransform(plots,CRS(projection(VEPIIN.NED)))


CCAC.texture <- as.numeric(raster::extract(VEPIIN.textures.rast,plots[plots$Garden=="POG",]))
CCAC.depth <- as.numeric(raster::extract(VEPIIN.depths.rast,plots[plots$Garden=="POG",]))











sim.poly.VEPIIS <- polygonFromExtent(PRISM.data.h2o_year.prcp.bricks[[2]])
VEPIIS.NRCS <- extractNRCS(template=sim.poly.VEPIIS, label="VEPIIS", raw.dir="/Volumes/BOCINSKY_DATA/DATA/NRCS", SFNF.dir="/Volumes/BOCINSKY_DATA/DATA/NRCS/SFNF/", force.redo=T)
VEPIIS.NHD <- extractNHD(template=sim.poly.VEPIIS, label="VEPIIS", raw.dir="/Volumes/BOCINSKY_DATA/DATA/NHD", remove.modern=TRUE, force.redo=F)
VEPIIS.NED <- extractNED(template=sim.poly.VEPIIS, label="VEPIIS", raw.dir="/Volumes/BOCINSKY_DATA/DATA/NED", res="1", drain=T, NHD.raw.dir="/Volumes/BOCINSKY_DATA/DATA/NHD", NRCS.raw.dir="/Volumes/BOCINSKY_DATA/DATA/NRCS", force.redo=T)






# TT.plot( class.sys = "USDA.TT", lang = "en" )
# TT.points(centroids.css*100,geo=TT.geo.get(class.sys = "USDA.TT"))










# plot(sim.poly.VEPIIS)
# plot(VEPIIS.NRCS, add=T)
# plot(sim.poly.VEPIIS, add=T, border="red", lwd=2)



