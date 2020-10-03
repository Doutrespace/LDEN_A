#############################################################################################################
############################################# Description ###################################################
#############################################################################################################

#############################################################################################################
########################################## Load Libraries ###################################################
#############################################################################################################
# Create import function
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}

# Load libraries
ipak(c("sp","sf","raster", "cleangeo", "mapview", "mapedit","DBI", "dplyr", "dbplyr", "odbc", "httr",
       "tidyverse", "ows4R", "spatstat", "fields", "GpGp", "rgdal", "ggplot2","rgeos"))
#############################################################################################################
########################################## Set WD datasets ##################################################
#############################################################################################################
Main_Dir <- "C:/Users/Nillus/OneDrive/Desktop/LDEN_A"
setwd(Main_Dir)
Data_dir <- paste0(Main_Dir,"/Data")
Export_dir <- paste0(Main_Dir,"/Export")
#############################################################################################################
########################################## Load datasets ####################################################
#############################################################################################################
# Change to Data dir
setwd(Data_dir)
# Load Big polygon file (all europe)
#Noise_All <- st_read("name of big file")
Noise_Koblenz_pol <- st_read("Koblenz_noise_pol.gpkg")

# Load Urban Atlas (this should be pulled from web based on name of city)
UA <-st_read("Koblenz_UA_2012_pol.gpkg")

area <- mapview(editMap())

area <- area@object[[1]][[1]][[1]]

### polygonize the area and define crs
area <- Polygon(area)
area <- Polygons(c(area), "area")
area <- SpatialPolygons(c(area), c(1:1), proj4string=CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

# if data available do dran ^^

wfs_bwk <- "http://www.opengis.net/cat/csw/2.0.2 http://schemas.opengis.net/csw/2.0.2/CSW-discovery.xsd"
url <- parse_url(wfs_bwk)
url$query <- list(service = "WFS",
                  version = "2.0.0",
                  request = "GetCapabilities")
request <- build_url(url)
request

bwk_client <- WFSClient$new(wfs_bwk, 
                            serviceVersion = "2.0.0")

AOI <- bbo
#############################################################################################################
#################################### Pre - Process datasets #################################################
#############################################################################################################
# 
# Reproject UA
UA <- st_transform(UA, crs = st_crs(25832))

Noise_Koblenz_pol <- st_transform(Noise_Koblenz_pol, crs = st_crs(25832))

# Rasterize

Noise_Koblenz_r <- raster(ncol=1000, nrow=1000)

extent(Noise_Koblenz_r) <- extent(Noise_Koblenz_pol)

rp <- rasterize(Noise_Koblenz_pol$geom, Noise_Koblenz_r, 'AREA')

Noise_Koblenz_r <- raster(x = "C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Export/test.tif")

crs(Noise_Koblenz_r) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# interpolation

Noise_Koblenz_r_bb <- bbox2SP(372533,5565087,407434.8,5592482.8)

Noise_Koblenz_r_bb <- bbox(Noise_Koblenz_r)

pts = data.frame(x=runif(1000,372533,5565087), y=runif(10,407434.8,5592482.8))

pointcount = function(Noise_Koblenz_r_bb, pts){
  # make a raster of zeroes like the input
  r2 = Noise_Koblenz_r_bb@bbox
  r2[] = 0
  # get the cell index for each point and make a table:
  counts = table(cellFromXY(Noise_Koblenz_r_bb@bbox,pts))
  # fill in the raster with the counts from the cell index:
  r2[as.numeric(names(counts))] = counts
  return(r2)
}

r2 = pointcount(Noise_Koblenz_r_bb, pts)
plot(r2)
points(pts)

values(Noise_Koblenz_r) <- 1:ncell(Noise_Koblenz_r)
Noise_Koblenz_r[25] <- NA
f <- focal(Noise_Koblenz_r, w=matrix(1,nrow=3, ncol=3), fun=mean, NAonly=TRUE, na.rm=TRUE) 

##################################################################################################################
# interpolation 2nd try


# load data
r <- Noise_Koblenz_r
r = aggregate(r, fact = 10)

b <- as(extent(r), "SpatialPolygons")

c <- spsample(b, n = 2000, type = "random")

par(mar=c(1,1,1,1))
plot(c)

pts <- c

rdf = as.data.frame(r, xy = T)
ptsdf = as.data.frame(pts)

# extract points
ptsdf$db = raster::extract(Noise_Koblenz_r,pts)

ggplot()+
  geom_point(data = ptsdf, aes(x = x, y = y
                              ),shape = 4)+
  labs(x ="x", y = "y")+
  scale_color_gradientn(colors = terrain.colors(10))+
  theme_bw()

# IDW Interpolation
library(gstat)

# create empty grid
grid = as(r, "SpatialPixels") 
grddf = as.data.frame(grid)

ggplot()+
  geom_point(data = grddf, aes(x = x, y = y), shape = 3, size = 0.5)+
  geom_point(data = ptsdf, aes(x = x, y =y),
             color = "red")+
  theme_bw()

# convert df to spatial points
pts = ptsdf
coordinates(pts) = ~ x + y
proj4string(pts) = proj4string(grid)

# na -> 0

pts$db[is.na(x = pts$db)] <- 0

idw <- gstat::idw(pts$db ~ 1, pts, newdata=grid, idp=2.0)

# IDW
#idw = idw(formula = db~1, 
          #locations = pts, 
         # newdata = grid)

idwdf = as.data.frame(idw)

idw=as.data.frame(idw)

#set outline bbox

x.range <- as.integer(range(LondonWards1@coords[,1]))
y.range <- as.integer(range(LondonWards1@coords[,2]))

koblenzoutline <- fortify(Noise_Koblenz_r@extent, region="Noise_Koblenz_pol")

plot<-ggplot(data=idw,aes(x=x,y=y))#start with the base-plot 
layer1<-c(geom_tile(data=idw,aes(fill=var1.pred)))#then create a tile layer and fill with predicted values
layer2<-c(geom_path(data=b,aes(x, y, group=group),colour = "grey40", size=1))#then create an outline layer
# now add all of the data together
plot+layer1+scale_fill_gradient(low="#FEEBE2", high="#7A0177")+coord_equal()

ggplot()+
  geom_tile(data = idwdf, aes(x = x, y = y, fill = var1.pred))+
  geom_point(data = ptsdf, aes(x = x, y = y),
             shape = 4)+
  scale_fill_gradientn(colors = terrain.colors(10))+
  theme_bw()


#############################################################################################################
########################################## Process datasets #################################################
#############################################################################################################

#############################################################################################################
################################################# Plots #####################################################
#############################################################################################################



Noise <- st_read("Koblenz_city_noise_fin.gpkg")

plot(Noise)

## RASTER
# Function to create raster
Raster_Bot <- reactive({
  pts$db <- filteredData()
  coordinates(pts@coords.nrs) = ~LON+LAT
  x.range <- as.double(range(pts[,1]))
  y.range <- as.double(range(pts[,2]))
  grd <- expand.grid(x=seq(from=x.range[1],
                           to=x.range[2],
                           by=input$res),
                     y=seq(from=y.range[1],
                           to=y.range[2],
                           by=input$res))
  coordinates(grd) <- ~ x+y
  gridded(grd) <- TRUE
  withProgress(message = 'Calculating raster', {
    idw <- idw(formula=filteredData()[,selectVariable()] ~ 1, locations=Data2, newdata=grd)
    idwrst <<- raster(idw)
  })
  idwrst <<- idwrst[[1]]
  names(idwrst) <<- "TEMP" # variable name
  crs(idwrst) <<- CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  return(idwrst)
  
})


############################################################################################################################
############################################################################################################################