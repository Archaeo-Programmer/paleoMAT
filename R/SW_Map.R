##updated 6/12/17 with new naming system of NHD classes (no longer NHD$NHD, now NHD$_)
##distill function does not work, but still seems to produce pdf with "dev.off"
#devtools::install_github("bocinsky/FedData")
library(FedData)
pkg_test("maptools")
pkg_test("maps")
pkg_test("RColorBrewer")
pkg_test("parallel")
pkg_test("entropy")
pkg_test("gtools")
pkg_test("UScensus2010")
pkg_test("xtable")
pkg_test("png")
pkg_test("TeachingDemos")
pkg_test("gdata")
pkg_test("matrixStats")
pkg_test("PBSmapping")
pkg_test("berryFunctions")
pkg_test("rgdal")

#setwd("/Volumes/VILLAGE/LAURA_FAUNA/Gini_Figures/")
#setwd("~/Desktop")
library(raster)
# Load some mapping functions Bocinsky wrote
junk <- lapply(list.files("./src", full.names=T),source)

fig.height <- 5.5 # inches
fig.width <- 4.5 # inches
buffer <- 0.2 # degrees
fig.asp <- fig.width/fig.height

master.proj <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# Full Page Dimensions
#VEPIIN <- polygon_from_extent(extent(672800,740000,4102000,4170000),"+proj=utm +datum=NAD83 +zone=12")
#VEPIIS <- polygon_from_extent(extent(359200,435800,3939600,4030400),"+proj=utm +datum=NAD83 +zone=13")

# Transform everything to the master projection
#VEPIIN <- spTransform(VEPIIN,master.proj)
#VEPIIS <- spTransform(VEPIIS,master.proj)

#boundary <- bbox(rgeos::gUnion(VEPIIN,VEPIIS))

#boundary[,'min'] <- boundary[,'min']-buffer
#boundary[,'max'] <- boundary[,'max']+buffer
#boundary.asp <- rowDiffs(boundary)[1,1]/rowDiffs(boundary)[2,1]
#center <- c(mean(boundary[1,]),mean(boundary[2,]))
#geo.asp <- 1/cos((center[2] * pi)/180)

#if(boundary.asp<(geo.asp*fig.asp)){
# south <- boundary['y','min']
#north <- boundary['y','max']

#west <- center[1]-((north-south)*(geo.asp*fig.asp))/2
#east <- center[1]+((north-south)*(geo.asp*fig.asp))/2
#}else{
#  west <- boundary['x','min']
# east <- boundary['x','max']

#  south <- center[2]-((east-west)/(geo.asp*fig.asp))/2
#  north <- center[2]+((east-west)/(geo.asp*fig.asp))/2
#}

#studyArea <- polygon_from_extent(extent(west,east,south,north),"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

studyArea <- polygon_from_extent(extent(-109.7,-107.2,35.4,38.0),"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
east <- -107.2
west <- -109.7
north <- 38.0
south <- 35.4

### Read in the gini site data
Gini_Sites <- read.csv("Gini_measurements_with_UTM_postAmerind.csv", header = TRUE, strip.white = TRUE, stringsAsFactors=F)
Gini_Sites_HH <- unique.data.frame(Gini_Sites)
# set up a variable for mapping total sample size
Gini_Sites_HH$sqrt_N <- sqrt(Gini_Sites_HH$Num_HH)
# get rid of individual components for Arroyo Hondo and just use totals -- they're all post-1280
#aDNA_Sites <- subset(aDNA_Sites, sitenum != "LA12.1")
#aDNA_Sites <- subset(aDNA_Sites, sitenum != "LA12.2")
#aDNA_Sites <- subset(aDNA_Sites, sitenum != "LA12.3")
# aDNA_Sites

##sites are now a mixture of UTM zones 12 and 13, which is a variable in dataset
Zone13_sites <- subset(Gini_Sites_HH, UTMZone == 13)
Zone12_sites <- subset(Gini_Sites_HH,UTMZone == 12)
## Promote this dataframe to a SpatialPointsDataFrame
## by stating which columns contain the x and y coordinates
coordinates(Zone13_sites) <- ~UTMEast+UTMNorth
coordinates(Zone12_sites) <- ~UTMEast+UTMNorth

#coordinates(Gini_Sites_HH) <- ~UTMEast+UTMNorth
#projection(Gini_Sites_HH) <- CRS("+proj=utm +datum=NAD83 +zone=12")

projection(Zone12_sites) <- CRS("+proj=utm +datum=NAD83 +zone=12")
projection(Zone13_sites) <- CRS("+proj=utm +datum=NAD83 +zone=13")
Zone13_sites <- spTransform(Zone13_sites, master.proj)
Zone12_sites <- spTransform(Zone12_sites, master.proj)

##separate sites by stratum
#CHACO SITES
Chaco_sites_z12 <- subset(Zone12_sites, Stratum== "Chaco")
Chaco_sites_z13 <- subset(Zone13_sites, Stratum== "Chaco")
Chaco_sites <- rbind(Chaco_sites_z12, Chaco_sites_z13)

#CMV
CMV_sites_z12 <- subset(Zone12_sites, Stratum %in% c('Dolores','Hovenweep' ,'McElmo'
                                                     ,'Ute Piedmont', 'WCMV','MVNP'))
CMV_sites_z13 <- subset(Zone13_sites, Stratum %in% c('Dolores','Hovenweep' ,'McElmo'
                                                     ,'Ute Piedmont', 'WCMV','MVNP'))
CMV_sites <- rbind(CMV_sites_z12, CMV_sites_z13)

#MSJ SITES
MSJ_sites_z12 <- subset(Zone12_sites, Stratum== "MSJ")
MSJ_sites_z13 <- subset(Zone13_sites, Stratum== "MSJ")
MSJ_sites <- rbind(MSJ_sites_z12, MSJ_sites_z13)


#Chuskas
Chuskas_sites_z12 <- subset(Zone12_sites, Stratum %in% c('NChuska', 'SChuska'))
Chuskas_sites_z13 <- subset(Zone13_sites, Stratum %in% c('NChuska', 'SChuska'))
Chuskas_sites <- rbind(Chuskas_sites_z12, Chuskas_sites_z13)
#library(GISTools)
#Chuskas_jittered <- jitter.points(Chuskas_sites, 0.01)


NED <- get_ned(template=studyArea, label="VEP_OVERVIEW",raw.dir="/Volumes/DATA/NED", extraction.dir="/Volumes/DATA/NED/EXTRACTIONS/")

# Rivers, creeks, and washes
NHD <- get_nhd(template=studyArea, label="VEP_OVERVIEW",raw.dir="/Volumes/DATA/NHD/", extraction.dir="/Volumes/DATA/NHD/EXTRACTIONS/")
NHD$'_Flowline' <- NHD$'_Flowline'[NHD$'_Flowline'$GNIS_Nm %in% c("San Juan River",
                                                                  "Mancos River",
                                                                  "McElmo Creek",
                                                                  "Colorado River",
                                                                  "Dolores River",
                                                                  "Montezuma Creek",
                                                                  "La Plata River",
                                                                  "Animas River",
                                                                  "Chinle Wash",
                                                                  "Chinle Creek",
                                                                  "Chaco Wash",
                                                                  "Chaco River",
                                                                  "Rio Grande",
                                                                  "Rio Chama",
                                                                  "Rio Gallina",
                                                                  "Jemez River",
                                                                  "Rio Puerco",
                                                                  "Puerco River"),]
NHD$'_Flowline' <- rgeos::gLineMerge(NHD$'_Flowline')

NHD$'_Area' <- NHD$'_Area'[NHD$'_Area'$AreSqKm>2 & NHD$'_Area'$FType=="403" & NHD$'_Area'$FCode!="40307",]
NHD$'_Waterbody' <- NHD$'_Waterbody'[NHD$'_Waterbody'$GNIS_Nm %in% c("Cochiti Lake",
                                                                     "Abiquiu Lake",
                                                                     "Navajo Reservoir",
                                                                     "Narraguinnep Reservoir"),]


# get 2500-m contour
NED.2500 <- rasterToContour(NED,levels=2500)
NED.2500 <- as(PolySet2SpatialPolygons(SpatialLines2PolySet(NED.2500)),"SpatialPolygons")
NED.2500 <- unlist(lapply( NED.2500@polygons , slot , "Polygons" ))
NED.2500 <- NED.2500[sapply(NED.2500,function(x){x@area >= .001})]

nCoords <- nrow(NED.2500[["3"]]@coords)
NED.2500[["3"]]@coords <- rbind(NED.2500[["3"]]@coords[1:(nCoords-1),],c(west,north),NED.2500[["3"]]@coords[nCoords,])
nCoords <- nrow(NED.2500[["27"]]@coords)
NED.2500[["27"]]@coords <- rbind(NED.2500[["27"]]@coords[1:(nCoords-1),],c(east,north),NED.2500[["27"]]@coords[nCoords,])
nCoords <- nrow(NED.2500[["41"]]@coords)
NED.2500[["41"]]@coords <- rbind(NED.2500[["41"]]@coords[1:(nCoords-1),],c(east,north),NED.2500[["41"]]@coords[nCoords,])

NED.2500[["41"]]@hole <- TRUE
NED.2500[["41"]]@ringDir <- as.integer(-1)
NED.2500[["41"]]@coords <- NED.2500[["41"]]@coords[nrow(NED.2500[["41"]]@coords):1,]

NED.2500 <- SpatialPolygons(list(Polygons(NED.2500,"polys")),proj4string=CRS(projection(NED)))
NED.2500 <- rgeos::gUnaryUnion(NED.2500)


# States
states <- readOGR("/Volumes/DATA/NATIONAL_ATLAS/statep010/", layer='statep010')
states <- states[states$STATE %in% c("Utah","New Mexico"),]
states <- raster::crop(states,rgeos::gUnaryUnion(spTransform(studyArea,CRS(projection(states)))))
west.state.ycoord <- ymin(extent(rgeos::gIntersection(spTransform(studyArea,CRS(projection(states))),states[states$STATE=="Utah",])))
east.state.ycoord <- ymax(extent(rgeos::gIntersection(spTransform(studyArea,CRS(projection(states))),states[states$STATE=="New Mexico",])))

# Towns
towns <- readOGR("/Volumes/DATA/NATIONAL_ATLAS/citiesx020/", layer='citiesx020')
projection(towns) <- CRS(projection(states))
towns <- raster::crop(towns,rgeos::gUnaryUnion(spTransform(studyArea,CRS(projection(towns)))))
towns <- towns[towns$NAME %in% c("Cortez",  "Durango","Farmington", "Pagosa Springs", "Alamosa", "Taos","Aztec","Albuquerque", "Los Alamos", "Santa Fe", "Gallup"),]

# Federal land
fedland <- readOGR("/Volumes/DATA/NATIONAL_ATLAS/fedlanp020/", layer='fedlanp020')
projection(fedland) <- CRS(projection(states))
fedland <- raster::crop(fedland,rgeos::gUnaryUnion(spTransform(studyArea,CRS(projection(fedland)))))
CANM <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(fedland$NAME1 == "Canyons of the Ancients National Monument"),]), data=data.frame(name="Canyons of the Ancients National Monument"))
MVNP <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(grepl("Mesa Verde",fedland$NAME1)),]), data=data.frame(name="Mesa Verde National Park"))
CHACO <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(grepl("Chaco",fedland$NAME1)),]), data=data.frame(name="Chaco Culture National Historical Park"))
CHELLY <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(fedland$NAME1 == "Canyon de Chelly National Monument"),]), data=data.frame(name="Canyon de Chelly National Monument"))
BAND <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(grepl("Bandelier",fedland$NAME1)),]), data=data.frame(name="Bandelier National Monument"))
SAND <- SpatialPolygonsDataFrame(rgeos::gUnaryUnion(fedland[which(fedland$NAME1 == "Great Sand Dunes National Park"),]), data=data.frame(name="Great Sand Dunes National Park"))
CANM <- spChFIDs(CANM,"Canyons of the Ancients National Monument")
SAND <- spChFIDs(SAND,"Great Sand Dunes National Park")
MVNP <- spChFIDs(MVNP,"Mesa Verde National Park")
CHACO <- spChFIDs(CHACO,"Chaco Culture National Historical Park")
CHELLY <- spChFIDs(CHELLY,"Canyon de Chelly National Monument")
BAND <- spChFIDs(BAND,"Bandelier National Monument")
fedland <- rbind(CANM,MVNP,CHACO,CHELLY)

pointsize <- 8

pdf(file='Gini_site_distrib_map_coloredsymbols.pdf', width=fig.width, height=fig.height, bg="white", pointsize=pointsize)
# quartz(width=fig.width, height=fig.height, bg="white", pointsize=pointsize)

# VEPIIN prcp
par(mai=c(0,0,0,0),
    oma=c(0,0,0,0),
    lend=2,
    ljoin=0,
    xpd=F)
plot.extent <- extent(studyArea)
plot(1, type='n', xlab="", ylab="", xlim=c(-109.61,xmax(plot.extent)),ylim=c(ymin(plot.extent),ymax(plot.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')



plot(NED.2500, border="gray75",col="gray90",add=T)

plot(studyArea, border='white', add=T)
plot(fedland, border="black", lty=3, add=T)
plot(NHD$'_Flowline', add=T)
plot(NHD$'_Area', col='black',border='black',add=T)
plot(NHD$'_Waterbody', col='black',border='black',add=T)
# plot(NHD$'_Area'[NHD$'_Area'$AreSqKm>1,][c(-8,-14,-18),], col='black',border='black',add=T)

# plot(roads, col='grey60', lwd=1, add=T)
# plot(sites, pch=19, add=T)
plot(states, lty=2, add=T)
# plot(NHD$NHDWaterbody, col='black',border='black',add=T)


plot(Chaco_sites, pch=1, lwd=1, col = "coral1", cex = Chaco_sites$sqrt_N, add=T)
plot(CMV_sites, pch=1, lwd=1, col = "#619CFF", cex = CMV_sites$sqrt_N, add=T)
plot(Chuskas_sites, pch= 1, lwd=1, col= "palegreen3", cex= Chuskas_sites$sqrt_N, add=T)
plot(MSJ_sites, pch= 1, lwd=1, col= "mediumorchid2", cex= MSJ_sites$sqrt_N, add=T)
#plot(Chuskas_jittered, pch= 1, lwd=1, col= "red", cex= Chuskas_jittered$sqrt_N, add=T)


###plot sites in B/W with different symbols
#plot(CMV_sites, pch=1, lwd=.2, col = "black", cex = CMV_sites$sqrt_N, add=T)
#plot(Chaco_sites, pch=2, lwd=.2, col = "black", cex = Chaco_sites$sqrt_N, add=T)
#plot(MSJ_sites, pch=0, lwd=.2, col = "black", cex = MSJ_sites$sqrt_N, add=T)


inch.x <- (xmax(plot.extent)-xmin(plot.extent))/(fig.width-par('mai')[2]-par('mai')[4])
inch.y <- (ymax(plot.extent)-ymin(plot.extent))/(fig.height-par('mai')[1]-par('mai')[3])


text(-109.4 +(0.05 * inch.x),37.0+(0.05 * inch.y),labels="Utah", adj=c(0,0), col="black", font=1, cex=1.25)
text(-109.4 +(0.05 * inch.x),37.0-(0.03 * inch.y),labels="Arizona", adj=c(0,1), col="black", font=1, cex=1.25)
text(-108.7+(0.05 * inch.x),37.0+(0.05 * inch.y),labels="Colorado", adj=c(1,0), col="black", font=1, cex=1.25)
text(-108.52-(0.05 * inch.x),37.0-(0.03 * inch.y),labels="New Mexico", adj=c(1,1), col="black", font=1, cex=1.25)

text(fedland[fedland$name=='Canyons of the Ancients National Monument',],labels="Canyons of the",font=2, cex=0.8, adj=c(0.5,-0.4))
text(fedland[fedland$name=='Canyons of the Ancients National Monument',],labels="Ancients NM",font=2, cex=0.8, adj=c(0.5,1.3))

text(fedland[fedland$name=='Mesa Verde National Park',],labels="Mesa",font=2, cex=7/8, adj=c(0.5,-1))
text(fedland[fedland$name=='Mesa Verde National Park',],labels=" Verde NP",font=2, cex=7/8, adj=c(0.5,0.7))

text(fedland[fedland$name=='Canyon de Chelly National Monument',],labels="Canyon de",font=2, cex=7/8, adj=c(0,-4.8))
text(fedland[fedland$name=='Canyon de Chelly National Monument',],labels=" Chelly NM",font=2, cex=7/8, adj=c(0,-3))

text(fedland[fedland$name=='Bandelier National Monument',],labels="Bandelier NM",font=2, cex=7/8, adj=c(0.5,-4.8))

text(fedland[fedland$name=='Chaco Culture National Historical Park',],labels="Chaco",font=2, cex=7/8, adj=c(0.5,-4.8))
text(fedland[fedland$name=='Chaco Culture National Historical Park',],labels=" Culture NHP",font=2, cex=7/8, adj=c(0.5,-3))

text(fedland[fedland$name=='Great Sand Dunes National Park',],labels="Great Sand",font=2, cex=7/8, adj=c(1,0))
# next line was adj=c(1,0.7))
text(fedland[fedland$name=='Great Sand Dunes National Park',],labels=" Dunes NP",font=2, cex=7/8, adj=c(1,1.5))

#text(x=-108.65, y=37.085, labels="Mancos R.", font=1, cex=1, srt=10)
text(x=-109.3, y=37.18, labels="San Juan R.", font=1, cex=1, srt=-23)
#text(x=-108.68, y=37.59, labels="Dolores R.", font=1, cex=1, srt=-23)
text(x=-109.1947, y=37.5889, labels="Montezuma Cr.", font=1, cex=1, srt=97.5)
text(x=-108.78, y=37.305, labels="McElmo Cr.", font=1, cex=1, srt=0)
#text(x=-108.65, y=37.7, labels="VEPII N Study Area", font=2, cex=1.25, srt=0)
#text(x=-106.1, y=35.55, labels="VEPII S Study Area", font=2, cex=1.25, srt=0)

#plot(VEPIIN, lwd=1, add=T)
#plot(VEPIIS, lwd=1, add=T)

plot(towns, pch=22, bg='white', add=T)
text(towns,labels=towns$NAME, pos=c(4,2,1,3,4,4,1,4,4,4), font=2, cex=7/8)

#plot(studyArea, border='white', add=T, lwd=6)
legend("bottomright", pch=c(1,1,1,1), lwd=c(2,2,2,2), lty=c(0,0,0,0), col=c("#619CFF", "coral1", "mediumorchid2", "palegreen3"), pt.cex=1, border=NA, legend= c("Central Mesa Verde sites", "Chaco Canyon sites", "Middle San Juan sites", "Chuskas sites"), text.width = 1, bty="o", bg ="white", cex=1)

scalebar(d=20, cex=0.075/(pointsize/100), font=2, xy = c(-107.5, 35.6), label="20 km", lwd=2, lend=1)
north.width <- 0.1
north.height <- 0.25
inch.xy <- c(0.1,0.15)
n.ratio <- 5/4
arrows(x0=xmax(plot.extent)-1.8*inch.xy[1]*inch.x,y0=ymin(plot.extent)+0.9*inch.xy[2]*inch.y,x1=xmax(plot.extent)-1.8*inch.xy[1]*inch.x,y1=ymin(plot.extent)+0.9*(inch.xy[2]+north.height)*inch.y, length=(north.width/2)/sin(pi/4), angle=45, lwd=n.ratio*north.width/(pointsize/100), lend=1)
text(labels="N",x=xmax(plot.extent)-0.18*inch.x,y=ymin(plot.extent)+0.12*inch.y,adj=c(0.5,0),cex=n.ratio*north.width/(pointsize/100), font=2)


dev.off()


pdf(file='Gini_site_contour_AD_500_to_600_test.pdf', width=fig.width, height=fig.height, bg="white", pointsize=pointsize)
# quartz(width=fig.width, height=fig.height, bg="white", pointsize=pointsize)

Gini <- read.csv(file="Gini_measurements_with_UTM_postAmerind.csv",head=TRUE)
cols.dont.want <- c("Comment", "Reference", "Recorder", "Status")
Gini <- Gini[, ! names(Gini) %in% cols.dont.want, drop = F]
Gini$all_living <- Gini$Pitstr_excl_antechamber_and_area_behind_wingwalls + Gini$Area_behind_wingwalls + Gini$front_rooms_living_area
Gini$all_storage <- Gini$Antechamber_and_floor_wall_cists + Gini$storage_surface_structures_external_features

# For some cases (Chaco especially) we only have total suite area, so avoid generating missing values by:
# Total_HH_Area should be non-missing for all cases after this…

Gini <- within(Gini,
               Total_HH_Area <- ifelse(!is.na(Total_Suite_Area_incl_PS),Total_Suite_Area_incl_PS,all_living + all_storage)
)

# for Amerind let's try the Exploitation/Exploration subdivisions ----some sites are now in BMII period, added in subperiod
## need to add in a subperiod for PIV and historic if Oraibi gets added into same dataframe
Gini$subperiod[Gini$Date_AD < 51]<- 'BMII'
Gini$subperiod[Gini$Date_AD > 51 & Gini$Date_AD < 601] <- 'BMIII.explore'
Gini$subperiod[Gini$Date_AD > 600 & Gini$Date_AD < 701] <- 'BMIII.exploit'
Gini$subperiod[Gini$Date_AD > 700 & Gini$Date_AD < 790] <- 'PI.explore'
Gini$subperiod[Gini$Date_AD > 789 & Gini$Date_AD < 891] <- 'PI.exploit'
Gini$subperiod[Gini$Date_AD > 890 & Gini$Date_AD < 1036] <- 'PII.explore'
Gini$subperiod[Gini$Date_AD > 1035 & Gini$Date_AD < 1146] <- 'PII.exploit'
Gini$subperiod[Gini$Date_AD > 1145 & Gini$Date_AD < 1201] <- 'PIII.explore'
Gini$subperiod[Gini$Date_AD > 1200] <- 'PIII.exploit'


#countour grouping for Years_AD
Gini$year_int[Gini$Date_AD > 500 & Gini$Date_AD < 601] <- 'AD 500-600'
Gini$year_int[Gini$Date_AD > 600 & Gini$Date_AD < 701] <- 'AD 601-700'
Gini$year_int[Gini$Date_AD > 700 & Gini$Date_AD < 801] <- 'AD 701-800'
Gini$year_int[Gini$Date_AD > 801 & Gini$Date_AD < 900] <- 'AD 801-900'
Gini$year_int[Gini$Date_AD > 901 & Gini$Date_AD < 1001] <- 'AD 901-1000'
Gini$year_int[Gini$Date_AD > 1001 & Gini$Date_AD < 1101] <- 'AD 1001-1100'
Gini$year_int[Gini$Date_AD > 1101 & Gini$Date_AD < 1201] <- 'AD 1101-1200'
Gini$year_int[Gini$Date_AD > 1201 & Gini$Date_AD < 1301] <- 'AD 1201-1300'


# turn it into a data table for easy by-group processing
Gini.dt <- data.table(Gini)
Gini %>% mutate_if(is.factor, as.character) -> Ginich
Ginich$Region[(Ginich$Stratum) == 'Dolores' || Ginich$Stratum == 'Hovenweep' || Ginich$Stratum == 'McElmo'
              || Ginich$Stratum == 'Ute Piedmont'|| Ginich$Stratum == 'WCMV'|| Ginich$Stratum == 'MVNP'] <- 'CMV'
Ginich$Region[Ginich$Stratum == 'Chaco'] <- 'Chaco'
Ginich$Region[(Ginich$Stratum) == 'MSJ'] <- 'MSJ and Chuskas'
Ginich$Region[Ginich$Stratum == 'WPueblo/Hopi'] <- 'WPueblo'
Ginich$Region[Ginich$Stratum == 'NChuska'] <- 'MSJ and Chuskas'
Ginich$Region[Ginich$Stratum == "SChuska"] <- 'MSJ and Chuskas'

# turn it into a data table for easy by-group processing, then add sample sizes for later plotting
Ginich.dt <- data.table(Ginich)[order(Region, Date_AD)]

GiniAll <- Ginich[,lapply(.SD,Gini, unbiased = TRUE,
                          conf.level = 0.8, R = 1000, type = "bca"),
                  by= "year_int", .SDcols=21:21 ] # give the Ginis for each value of subperiod
GiniAll$Type <- rep(c("Gini", "Lower_B", "Upper_B"), nrow(GiniAll)) # make new column and fill
GiniAll <- spread(GiniAll, Type, Total_HH_Area)



##sites are now a mixture of UTM zones 12 and 13, which is a variable in dataset
Zone13_sites <- subset(GiniAll, UTMZone == 13)
Zone12_sites <- subset(GiniAll,UTMZone == 12)
## Promote this dataframe to a SpatialPointsDataFrame
## by stating which columns contain the x and y coordinates
coordinates(Zone13_sites) <- ~UTMEast+UTMNorth
coordinates(Zone12_sites) <- ~UTMEast+UTMNorth

#coordinates(Gini_Sites_HH) <- ~UTMEast+UTMNorth
#projection(Gini_Sites_HH) <- CRS("+proj=utm +datum=NAD83 +zone=12")

projection(Zone12_sites) <- CRS("+proj=utm +datum=NAD83 +zone=12")
projection(Zone13_sites) <- CRS("+proj=utm +datum=NAD83 +zone=13")
Zone13_sites <- spTransform(Zone13_sites, master.proj)
Zone12_sites <- spTransform(Zone12_sites, master.proj)

all_sites_HH <- rbind(Zone13_sites, Zone12_sites)


par(mai=c(0,0,0,0),
    oma=c(0,0,0,0),
    lend=2,
    ljoin=0,
    xpd=F)
plot.extent <- extent(studyArea)
plot(1, type='n', xlab="", ylab="", xlim=c(xmin(plot.extent),xmax(plot.extent)),ylim=c(ymin(plot.extent),ymax(plot.extent)), xaxs="i", yaxs="i", axes=FALSE, main='')



plot(NED.2500, border="gray75",col="gray90",add=T)

plot(studyArea, border='white', add=T)
plot(fedland, border="black", lty=3, add=T)
plot(NHD$'_Flowline', add=T)
plot(NHD$'_Area', col='black',border='black',add=T)
plot(NHD$'_Waterbody', col='black',border='black',add=T)
# plot(NHD$'_Area'[NHD$'_Area'$AreSqKm>1,][c(-8,-14,-18),], col='black',border='black',add=T)

# plot(roads, col='grey60', lwd=1, add=T)
# plot(sites, pch=19, add=T)
plot(states, lty=2, add=T)
# plot(NHD$NHDWaterbody, col='black',border='black',add=T)

#plot(CMV_sites, pch=1, lwd=.2, col = "#619CFF", cex = CMV_sites$sqrt_N, add=T)
#plot(Chaco_sites, pch=1, lwd=.2, col = "#00BA38", cex = Chaco_sites$sqrt_N, add=T)
#plot(Gini_Sites_HH, pch=1, lwd=2, col = "orangered2", cex = Gini_Sites_HH$sqrt_N, add=T)

#plot contour lines for AD 500-600
contour(x= all_sites_HH$long, y= all_sites_HH$lat, z= all_sites_HH$Gini[year_int == 'AD 500-600'], levels= seq(0.1, 1.0, 0.1), axes= FALSE, add= T)



####end of test contour code









inch.x <- (xmax(plot.extent)-xmin(plot.extent))/(fig.width-par('mai')[2]-par('mai')[4])
inch.y <- (ymax(plot.extent)-ymin(plot.extent))/(fig.height-par('mai')[1]-par('mai')[3])


text(xmin(plot.extent)+(0.05 * inch.x),west.state.ycoord+(0.05 * inch.y),labels="Utah", adj=c(0,0), col="black", font=1, cex=1.25)
text(xmin(plot.extent)+(0.05 * inch.x),west.state.ycoord-(0.03 * inch.y),labels="Arizona", adj=c(0,1), col="black", font=1, cex=1.25)
text(xmax(plot.extent)-(0.05 * inch.x),west.state.ycoord+(0.05 * inch.y),labels="Colorado", adj=c(1,0), col="black", font=1, cex=1.25)
text(xmax(plot.extent)-(0.05 * inch.x),west.state.ycoord-(0.03 * inch.y),labels="New Mexico", adj=c(1,1), col="black", font=1, cex=1.25)

text(fedland[fedland$name=='Canyons of the Ancients National Monument',],labels="Canyons of the",font=2, cex=0.8, adj=c(0.5,-0.4))
text(fedland[fedland$name=='Canyons of the Ancients National Monument',],labels="Ancients NM",font=2, cex=0.8, adj=c(0.5,1.3))

text(fedland[fedland$name=='Mesa Verde National Park',],labels="Mesa",font=2, cex=7/8, adj=c(0.5,-1))
text(fedland[fedland$name=='Mesa Verde National Park',],labels=" Verde NP",font=2, cex=7/8, adj=c(0.5,0.7))

text(fedland[fedland$name=='Canyon de Chelly National Monument',],labels="Canyon de",font=2, cex=7/8, adj=c(0,-4.8))
text(fedland[fedland$name=='Canyon de Chelly National Monument',],labels=" Chelly NM",font=2, cex=7/8, adj=c(0,-3))

text(fedland[fedland$name=='Bandelier National Monument',],labels="Bandelier NM",font=2, cex=7/8, adj=c(0.5,-4.8))

text(fedland[fedland$name=='Chaco Culture National Historical Park',],labels="Chaco",font=2, cex=7/8, adj=c(0.5,-4.8))
text(fedland[fedland$name=='Chaco Culture National Historical Park',],labels=" Culture NHP",font=2, cex=7/8, adj=c(0.5,-3))

text(fedland[fedland$name=='Great Sand Dunes National Park',],labels="Great Sand",font=2, cex=7/8, adj=c(1,0))
# next line was adj=c(1,0.7))
text(fedland[fedland$name=='Great Sand Dunes National Park',],labels=" Dunes NP",font=2, cex=7/8, adj=c(1,1.5))

text(x=-108.65, y=37.085, labels="Mancos R.", font=1, cex=1, srt=10)
text(x=-109.3, y=37.18, labels="San Juan R.", font=1, cex=1, srt=-23)
text(x=-108.68, y=37.59, labels="Dolores R.", font=1, cex=1, srt=-23)
text(x=-109.1947, y=37.5889, labels="Montezuma Cr.", font=1, cex=1, srt=97.5)
text(x=-108.78, y=37.305, labels="McElmo Cr.", font=1, cex=1, srt=0)
#text(x=-108.65, y=37.7, labels="VEPII N Study Area", font=2, cex=1.25, srt=0)
#text(x=-106.1, y=35.55, labels="VEPII S Study Area", font=2, cex=1.25, srt=0)

#plot(VEPIIN, lwd=1, add=T)
#plot(VEPIIS, lwd=1, add=T)

plot(towns, pch=22, bg='white', add=T)
text(towns,labels=towns$NAME, pos=c(4,2,1,3,3,4,1,4,4,4), font=2, cex=7/8)

plot(studyArea, border='white', add=T, lwd=6)
legend("bottomleft", pch=c(1,2), lwd=c(2,2), lty=c(0,0), col=c("black","black"), pt.cex=1, border=NA, legend= c("Central Mesa Verde sites", "Chaco Canyon sites"), text.width = 1, bty="o", bg ="white", cex=1)

scalebar(d=20, cex=0.075/(pointsize/100), font=2, xy = c(-108.78, 35.6), label="20 km", lwd=2, lend=1)
north.width <- 0.1
north.height <- 0.25
inch.xy <- c(0.1,0.15)
n.ratio <- 5/4
arrows(x0=xmin(plot.extent)+24*inch.xy[1]*inch.x,y0=ymin(plot.extent)+0.8*inch.xy[2]*inch.y,x1=xmin(plot.extent)+24*inch.xy[1]*inch.x,y1=ymin(plot.extent)+0.8*(inch.xy[2]+north.height)*inch.y, length=(north.width/2)/sin(pi/4), angle=45, lwd=n.ratio*north.width/(pointsize/100), lend=1)
text(labels="N",x=xmin(plot.extent)+2.4*inch.x,y=ymin(plot.extent)+0.12*inch.y,adj=c(0.5,0),cex=n.ratio*north.width/(pointsize/100), font=2)


dev.off()




