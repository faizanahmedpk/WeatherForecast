library(RMAWGEN)

library(RgoogleMaps)

SelCatch <- c('Brisbane','Bulloo','Burdekin','CooperCreek','Fitzroy','Johnstone',

'LoganAlbert','SouthCoast','Pine','Warrego')

i10 <- 2 # select the number of your catchment

subDir <- SelCatch[i10]

mainDir <- paste("K://2020_Submission//MULTIRAINFALL//RMAWGEN_Examples//") #change this to the directory where you have stored the file



setwd(mainDir) #change this to the directory where you have stored the file

#save(ELEVATION,LOCATION,PRECIPITATION,PRECIPITATION_MEASUREMENT_END_DAY,

# PRECIPITATION_MEASUREMENT_START_DAY,STATION_LATLON,STATION_NAMES,TEMPERATURE_MAX,

# TEMPERATURE_MEASUREMENT_END_DAY,TEMPERATURE_MEASUREMENT_START_DAY,

# TEMPERATURE_MIN,CatStations,CatBoundary,BuffArea,file=paste(subDir,'ModInp.RData',sep=''))

load(file=paste(subDir,'ModInp.RData',sep=''))

CatStations <- CatStations[CatStations$Available==1,]

plot(BuffArea,typ='l',asp=1,xlab='longitute',ylab='latitude')

lines(CatBoundary,col='red')

with(CatStations,points(lon,lat))

# Googlemaps

library(RgoogleMaps)

lonR <- range(CatBoundary$x)

latR <- range(CatBoundary$y)

CatchMap <- GetMap.bbox(lonR=lonR,latR=latR)

lat = CatBoundary$y;

lon = CatBoundary$x;

PlotOnStaticMap(CatchMap, lat = lat,lon = lon,

cex=1.5,pch=20,destfile = "MyTile1.png",

FUN=lines, add=FALSE,lty=1,lwd=3,asp=1,axes=TRUE);

#and add stations:

PlotOnStaticMap(CatchMap, lat = CatStations$lat,

lon = CatStations$lon,

FUN = points, add=TRUE,pch=19,col='red',cex=1.2)