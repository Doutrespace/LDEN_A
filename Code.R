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
       "tidyverse", "ows4R", "spatstat", "fields", "GpGp", "rgdal", "ggplot2","rgeos", "here", "stars",
       "ggnewscale", "scico", "ggrepel"))
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

### Interpolation

################################################### CREATE IDW MODEL
# Create dataframe from Noise merged file
pts <- as.data.frame(Noise_Koblenz_r$test, xy=TRUE)
pts <- na.omit(pts)
names(pts)<- c("X","Y","DB")

# Assign X-Y fields as coordinates 
coordinates(pts) = ~X+Y
x.range <- as.double(range(pts@coords[,1]))
y.range <- as.double(range(pts@coords[,2]))

# Create empty grid basted on coordinates 
grd <- expand.grid(x=seq(from=x.range[1],
                         to=x.range[2],
                         by=100),
                   y=seq(from=y.range[1],
                         to=y.range[2],
                         by=100))
coordinates(grd) <- ~ x+y
gridded(grd) <- TRUE

# Interpolate (IDW) Db values 
idw <- gstat::idw(DB ~ 1, pts, newdata=grd, idp=5.0)

# Create IDW raster file
idwrst <- raster(idw)
idwrst <- idwrst[[1]]
names(idwrst) <- "Db" # variable name
crs(idwrst) <- CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
plot(idwrst)

# Compare original values with predicted ones
pts$DB_O <- raster::extract(idwrst,pts[,1:2])
pts$Diff <- pts$DB_O - pts$DB

# Export IDW raster
writeRaster(idwrst,
            paste0(Export_dir,'/IDW.tif'),
            options=c('TFW=YES'),overwrite=TRUE)

#############################################################################################################
########################################## Process datasets #################################################
#############################################################################################################
UA_poly <- st_transform(UA, "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Extract the underlying raster values for each feature in the polygon layer
UA_poly <- UA_poly %>% mutate(
  DbMean = raster::extract(idwrst, UA_poly, fun = mean, na.rm = TRUE),
  DbMax = raster::extract(idwrst, UA_poly, fun = max, na.rm = TRUE),
  DbMin = raster::extract(idwrst, UA_poly, fun = min, na.rm = TRUE)
)

st_write(UA_poly, paste0(Export_dir,'/DDUF.gpkg'))

UA_DDUF <- filter(UA_poly, class_2012 == "Discontinuous dense urban fabric (S.L. : 50% -  80%)")
UA_DDUF <- as.data.frame(UA_DDUF)

UA_GUA <- filter(UA_poly, class_2012 == "Green urban areas" )
UA_GUA  <- as.data.frame(UA_GUA)


library(patchwork)

P1 <- ggplot(UA_DDUF, aes(x=DbMean)) + 
  geom_density()

P2 <- ggplot(UA_GUA, aes(x=DbMean)) + 
  geom_density()

P1 + P2

#############################################################################################################
################################################# Plots #####################################################
#############################################################################################################



############################################################################################################################
############################################################################################################################