#############################################################################################################
############################################# Description ###################################################
#############################################################################################################
#
#############################################################################################################
########################################## Load Libraries ###################################################
#############################################################################################################
# Ceate import function
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}

# Load libraries
ipak(c("sp","sf","raster","tidyverse","cleangeo", "mapview", "mapedit","DBI", "dplyr", "dbplyr", "odbc", "httr",
       "ows4R", "spatstat", "fields", "GpGp", "rgdal", "ggplot2","ggdark","rgeos", "here", "stars",
       "ggnewscale", "scico", "ggrepel", "gstat","spdep", "patchwork","rstudioapi" ,"lubridate", "thematic", "GGally","ggthemes",
       "RColorBrewer", "biscale", "magrittr", "lintr", "disco", "ggdark","rChoiceDialogs"))

#############################################################################################################
########################################## DataFrame Creation ###############################################
#############################################################################################################
CreateDf <- function(NdB_Pol,UA_Pol, AOI_Pol, City_Name){
  
  # Change directory to export 
  setwd(Export_Fo)
  
  # Create export directory to save function output
  dir.create(City_Name, showWarnings = FALSE)
  
  # Change wd to just created folder
  setwd(City_Name)
  
  # Add Class field to poligons (later -> apply family (time) )
  NdB_Pol$Class <- ""
  
  # For loop to reassign dB Class
  for(i in 1:length(NdB_Pol$DB_High)){
    if(NdB_Pol$DB_High[i]==0)                             {NdB_Pol$Class[i] = "0"}
    if(NdB_Pol$DB_High[i]> 0 &&  NdB_Pol$DB_High[i]<= 55) {NdB_Pol$Class[i] = "55"}
    if(NdB_Pol$DB_High[i]> 55 && NdB_Pol$DB_High[i]<= 60) {NdB_Pol$Class[i] = "60"}
    if(NdB_Pol$DB_High[i]> 60 && NdB_Pol$DB_High[i]<= 65) {NdB_Pol$Class[i] = "65"}
    if(NdB_Pol$DB_High[i]> 65 && NdB_Pol$DB_High[i]<= 70) {NdB_Pol$Class[i] = "70"}
    if(NdB_Pol$DB_High[i]> 70 && NdB_Pol$DB_High[i]<= 75) {NdB_Pol$Class[i] = "75"}
    if(NdB_Pol$DB_High[i]> 75 && NdB_Pol$DB_High[i]<= 80) {NdB_Pol$Class[i] = "80"}
    if(NdB_Pol$DB_High[i]> 80)                            {NdB_Pol$DB_High[i] = ">80"}
    print(paste0((i*100)/length(NdB_Pol$DB_High),"%"))
  }
  
  print("dB Values reclassified done...")

  # Merge polygons based on Class
  NdB_Pol <- as(NdB_Pol, 'Spatial')
  
  # Simplify polygons with same Class category
  Noise_Pol_F <- aggregate(NdB_Pol, by = "Class")
  print("aggregate done...")
  
  # Intersect GPKG UA_Pol and Noise_Pol_F
  Pol_Intersect <<- st_intersection(UA_Pol, st_as_sf(Noise_Pol_F))
  print("intersection done...")
   
  #### Calculate db % area in each Urban atlas class 
  # Create urban atlas available classes vector
  Names <- sort(unique(Pol_Intersect$ITEM2012))
  # Create noise available classes
  DbVal <- sort(unique(Pol_Intersect$Class))

  # Create dataframe to storage the information
  SumDF <- data.frame(UA_Class = Names, "0" ,">80", "55" , "60",  "65",  "70",  "75",  "80", "Tot", "Tot_Class_Area")
  names(SumDF) <- c("UA_Class","0" ,">80", "55" , "60",  "65",  "70",  "75",  "80", "Tot", "Tot_Class_Area")
  SumDF$Tot_Class_Area <- 0
  
  # for loop to summarize the information of the UA classes intersected by dB classes  
  for(x in 1:length(Names)){
    
    # Clean dataframe 
    SumDF[x,] <- NA
  
    # Calculate the total area of UA class
    Tot_Area <- sum(st_area(Pol_Intersect[Pol_Intersect$ITEM2012 == Names[x],]))
    SumDF$Tot_Class_Area[x] <- as.numeric(Tot_Area)
    
    # Create Tot value that will check if the sim of the areas is 100% / reset it in each iteration
    Tot = 0
    
    # Loop through all dB classes  
    for(y in 1:length(DbVal)){
      
      # Summ all areas that match the two UA class and dB class  
      Ua_Area <- sum(st_area(Pol_Intersect[Pol_Intersect$ITEM2012 == Names[x] & Pol_Intersect$Class == DbVal[y],]))
      
      # Store the normalized area in the dataframe
      SumDF[x, match(DbVal[y], names(SumDF))] <- Ua_Area*100/Tot_Area
      
      # Summ all rows (one by one)   
      Tot = Tot + as.numeric(Ua_Area*100/Tot_Area)
        
     }
    
    # Save the total area in the dataframe  
    SumDF$Tot[x] <- Tot
  }
  
  # Calculate % of each UA CLASS
  SumDF$Tot_Class_Area <- round((SumDF$Tot_Class_Area[]*100/sum(SumDF$Tot_Class_Area)),2)
   
  # Extract pop data from original pol
  #tmpDF <- as.data.frame(UA_Pol$ITEM2012)
  #names(tmpDF) <- c("Class")
  #tmpDF$Pop <- UA_Pol$Pop2012
  #tmpDF <- aggregate(Pop ~ Class, data=tmpDF, FUN=sum)
   
  # Match pop val in UA class 
  #SumDF$Popul <- tmpDF$Pop
  
  # Save dataframe as csv
  write_csv(SumDF,paste0(City_Name,".csv"))
   
}
#############################################################################################################
########################################## Read from directory ##############################################
#############################################################################################################
# Ask for main folder, here all data MUST be structured
Main_Fo <- "C:\\Users\\Nillus\\OneDrive\\Desktop\\LDEN_A\\Main_Fo"#choose.dir()
Data_Fo <- paste0(Main_Fo,"\\Data")
Export_Fo <- paste0(Main_Fo,"\\Export")

# Change directory
setwd(Data_Fo)

# Create list of available cities folders
available_cities <- list.files(getwd())

# Big for loop to go through all available folders to create a csv with the summary of ......keep adding the description here mf
# change 1 for length(available_cities)

for(i in 2:2){
  
  # Starting message
  print(paste0("Init ", available_cities[i], "analysis"))
  
  # Change directory 
  setwd(paste0(Data_Fo,"\\",available_cities[i]))
  
  # Load Noise GPKG
  available_Nf <- list.files(path = paste0(getwd(),"/1.Noise_pol"), full.names = TRUE)
  Noise_Pol <- st_read(available_Nf)
  
  # Load Urban Atlas GPKG
  available_Ua <- list.files(path = paste0(getwd(),"/2.Urban_Atlas"), full.names = TRUE)
  UrbAtl_Pol <- st_read(available_Ua)
  
  # Load AOI polygon
  available_Aoi <- list.files(path = paste0(getwd(),"/3.Area"), full.names = TRUE)
  Aoi_Pol <- st_read(available_Aoi)
  
  # Call main function to create dataframes from the instersection of the imported data -> here! index the sf 
  CreateDf(Noise_Pol,UrbAtl_Pol,Aoi_Pol,available_cities[i])
  
  # Reset directory to Data folder 
  setwd(Data_Fo)
  
  rm(Noise_Pol,UrbAtl_Pol,Aoi_Pol)
  
}

#############################################################################################################
#############################################################################################################
#############################################################################################################
#
#
#
#+++++++++++++++++++++++++++++++++++++++++ MANUAL ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#############################################################################################################
########################################## Set WD datasets ##################################################
#############################################################################################################
Main_Dir <- "C:/Users/Nillus/OneDrive/Desktop/LDEN_A"

setwd(Main_Dir)

Data_dir <- paste0(Main_Dir,"/Data")

Export_dir <- paste0(Main_Dir,"/Export")
#############################################TMP#############################################################
# Import UA GPKG and reproject crs=25832

UrbAtl_Pol <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Data/Koblenz/WGS84/UA.gpkg") 

# Load area GPKG
Area <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Data/Koblenz/WGS84/Area.gpkg") 

# Import NOISE TIFF
Noise_R <- raster("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Data/Koblenz/WGS84/Noise.tif")
crs(Noise_R) <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "


#############################################################################################################
#################################### Pre - Process datasets #################################################
#############################################################################################################
# Check which polygons overlay on ratser
UrbanAtlas <- st_intersection(UrbAtl_Pol, Area)  
UrbanAtlas <- UrbanAtlas[1:10,]  #tryout

# Extraxt values
UrbanAtlas <-
  UrbanAtlas %>% mutate(
    PixVal = raster::extract(Noise_R, UrbanAtlas)
  )

TMPDF <- as.data.frame(UrbanAtlas$PixVal[1])
names(TMPDF) <- c("dB")

ggplot(TMPDF) +
  geom_bar(aes(x=dB))

#############################################################################################################
########################################## Process datasets #################################################
#############################################################################################################
# Reclas Raster into categories c(0-59-64-69-74-LAF)

# plot histogram of data
hist(Noise_R,
     main = "Distribution of raster dB cell values",
     xlab = "Noise (dB)", ylab = "Number of Pixels",
     col = "springgreen")

reclass_df <- c(0, 0, 0,
                1, 59, 59,
                60, 64, 64,
                65, 74, 74,
                75, Inf, 80)

reclass_m <- matrix(reclass_df,
                    ncol = 3,
                    byrow = TRUE)

#NoiseReclass <- 

#Noise_Pol <- 

Noise_Pol <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Export/Noise_Pol.gpkg") 


# Raster to poligon
Noise_Pol <- rasterToPolygons(Noise_R)
class(Noise_Pol)

# Add Class field to poligons (later -> apply family (time) )
Noise_Pol$Class <- ""

for(i in 1:length(Noise_Pol$Noise)){
  if(Noise_Pol$Noise[i]==0){Noise_Pol$Class[i] = "0"}
  if(Noise_Pol$Noise[i]> 0 && Noise_Pol$Noise[i]<= 55){Noise_Pol$Class[i] = "55"}
  if(Noise_Pol$Noise[i]> 55 && Noise_Pol$Noise[i]<= 60){Noise_Pol$Class[i] = "60"}
  if(Noise_Pol$Noise[i]> 60 && Noise_Pol$Noise[i]<= 65){Noise_Pol$Class[i] = "65"}
  if(Noise_Pol$Noise[i]> 65 && Noise_Pol$Noise[i]<= 70){Noise_Pol$Class[i] = "70"}
  if(Noise_Pol$Noise[i]> 70 && Noise_Pol$Noise[i]<= 75){Noise_Pol$Class[i] = "75"}
  if(Noise_Pol$Noise[i]> 75 && Noise_Pol$Noise[i]<= 80){Noise_Pol$Class[i] = "80"}
  if(Noise_Pol$Noise[i]> 80){Noise_Pol$Class[i] = ">80"}
  print(paste0((i*100)/length(Noise_Pol$Noise),"%"))
}

# Save polygons before merge
st_write(Noise_Pol, paste0(Export_dir,'/Noise_Pol.gpkg'))   

writeOGR(obj = Noise_Pol , dsn = "C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Export/Noise_Pol1.gpkg", layer = "Noise_Pol", driver = "GPKG")

# Merge polygons based on Class
list <- unique(NdB_Pol$Class)
Noise_Pol_F <- aggregate(Noise_Pol, by = "Class") # original


# Intersect GPKG
Pol_Intersect <- st_intersection(UrbAtl_Pol,st_as_sf(Noise_Pol_F))

#############################################################################################################
########################################## Dataset statistics ###############################################
#############################################################################################################

#-> Calculate db % area in each Urban atlas class 
Names <- sort(unique(Pol_Intersect$class_2012))
DbVal <- sort(unique(Pol_Intersect$Class))
SumDF <- data.frame(UA_Class = Names, ">80", "55" , "60",  "65",  "70",  "75",  "80", "Tot", "Tot_Class_Area")
names(SumDF) <- c("UA_Class",">80", "55" , "60",  "65",  "70",  "75",  "80", "Tot", "Tot_Class_Area")
SumDF$Tot_Class_Area <- 0

for(i in 1:length(Names)){
  
  Tot_Area <- sum(st_area(Pol_Intersect[Pol_Intersect$class_2012 == Names[i],]))
  
  SumDF$Tot_Class_Area[i] <- as.numeric(Tot_Area)
  
  Tot = 0
  
  for(j in 1:length(DbVal)){
    
    Ua_Area <- sum(st_area(Pol_Intersect[Pol_Intersect$class_2012 == Names[i] & Pol_Intersect$Class == DbVal[j],]))
    
    SumDF[i,j+1] <- Ua_Area*100/Tot_Area
    
    Tot = Tot + as.numeric(Ua_Area*100/Tot_Area)
    
  }
  
  SumDF$Tot[i] <- Tot
}

# Calculate % of each UA CLASS
SumDF$Tot_Class_Area <- round((SumDF$Tot_Class_Area[]*100/sum(SumDF$Tot_Class_Area)),2)

# Extract pop data from original pol
tmpDF <- as.data.frame(UrbAtl_Pol$class_2012)
names(tmpDF) <- c("Class")
tmpDF$Pop <- UrbAtl_Pol$Pop2012
tmpDF <- aggregate(Pop ~ Class, data=tmpDF, FUN=sum)

# Match pop val in UA class 
SumDF$Popul <- tmpDF$Pop
rm(tmpDF)

write_csv(SumDF,"C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Data/kob.csv")



#############################################################################################################
########################################## Dataset statistics ###############################################
#############################################################################################################






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

############################################### CREATE IDW MODEL ##########################################
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
idw <- gstat::idw(DB ~ 1, pts, newdata=grd, idp=2.0)

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
            paste0(Export_dir,'/IDW2.tif'),
            options=c('TFW=YES'),overwrite=TRUE)

######################################## Create Kinging Model################################################
# Create Spatial Dataframe
coordinates(pts) = ~X+Y
# Compute the sample variogram with 1st order polynomial equation as.formula(DB ~ X + Y)
var.smpl <- variogram(object =  as.formula(DB ~ X + Y),
                      data = pts,
                      cloud = FALSE,
                      cutoff=1000000,
                      width=89900)

# Compute the variogram model by passing the nugget, sill and range values
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# The following plot allows us to assess the fit
plot(var.smpl, dat.fit, xlim=c(0,1000000))

grd2 <- expand.grid(x=seq(from=x.range[1],
                          to=x.range[2],
                          by=100),
                    y=seq(from=y.range[1],
                          to=y.range[2],
                          by=100))

f.1 <- as.formula(pts ~ X + Y) 

dat.krg <- krige(object =  as.formula(DB ~ X + Y), data = pts, grd2, dat.fit)

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

# export the structural statistics
st_write(UA_poly, paste0(Export_dir,'/DDUF.gpkg'))

