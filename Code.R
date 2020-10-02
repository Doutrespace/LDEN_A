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
ipak(c("sp","sf","raster", "cleangeo", "mapview", "mapedit","DBI", "dplyr", "dbplyr", "odbc", "httr", "tidyverse", "ows4R"))
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
area <- SpatialPolygons(c(area), c(1:1), proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

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

#wtf?


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
  Koblenz_city_noise <- filteredData()
  coordinates(Koblenz_city_noise) = ~LON+LAT
  x.range <- as.double(range(Koblenz_city_noise@coords[,1]))
  y.range <- as.double(range(Koblenz_city_noise@coords[,2]))
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
  crs(idwrst) <<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  return(idwrst)
  
})

############################################################################################################################
############################################################################################################################