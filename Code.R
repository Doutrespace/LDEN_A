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
       "tidyverse", "ows4R", "spatstat", "fields", "GpGp", "rgdal", "ggplot2","ggdark","rgeos", "here", "stars",
       "ggnewscale", "scico", "ggrepel", "gstat","spdep", "patchwork", "lubridate", "thematic", "GGally","ggthemes",
       "RColorBrewer"))
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

################################################### CREATE IDW MODEL ########################################
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

# asign structural classes
UA_DDUF <- filter(UA_poly, class_2012 == "Discontinuous dense urban fabric (S.L. : 50% -  80%)")
UA_DDUF <- as.data.frame(UA_DDUF)

UA_DLDUF <- filter(UA_poly, class_2012 == "Discontinuous low density urban fabric (S.L. : 10% - 30%)")
UA_DLDUF <- as.data.frame(UA_DLDUF)

UA_GUA <- filter(UA_poly, class_2012 == "Green urban areas" )
UA_GUA  <- as.data.frame(UA_GUA)

UA_AL <- filter(UA_poly, class_2012 == "Arable land (annual crops)")
UA_AL <- as.data.frame(UA_AL)

UA_CUF <- filter(UA_poly, class_2012 == "Continuous Urban Fabric (S.L. > 80%)")
UA_CUF <- as.data.frame(UA_CUF)

UA_P <- filter(UA_poly, class_2012 == "Pastures")
UA_P <- as.data.frame(UA_P)

UA_F <- filter(UA_poly, class_2012 == "Forests")
UA_F <- as.data.frame(UA_F)

UA_ICPMPU <- filter(UA_poly, class_2012 == "Industrial, commercial, public, military and private units")
UA_ICPMPU <- as.data.frame(UA_ICPMPU)

UA_LWCU <- filter(UA_poly, class_2012 == "Land without current use")
UA_LWCU <- as.data.frame(UA_LWCU)

UA_W <- filter(UA_poly, class_2012 == "Water")
UA_W <- as.data.frame(UA_W)

UA_IS <- filter(UA_poly, class_2012 == "Isolated structures")
UA_IS <- as.data.frame(UA_IS)

UA_RAL <- filter(UA_poly, class_2012 == "Other roads and associated land")
UA_RAL <- as.data.frame(UA_RAL)

# plot structural UA classes

P1 <- ggplot(UA_DDUF, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)
  
P2 <- ggplot(UA_GUA, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P3 <- ggplot(UA_AL, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P4 <- ggplot(UA_DLDUF, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P5 <- ggplot(UA_P, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P6 <- ggplot(UA_GUA, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P7 <- ggplot(UA_F, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P8 <- ggplot(UA_ICPMPU, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P9 <- ggplot(UA_W, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P10 <- ggplot(UA_IS, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

P11 <- ggplot(UA_RAL, aes(x=DbMean, fill = class_2012, colour = class_2012)) + 
  geom_density(alpha = 0.7)+ dark_mode()+
  theme(legend.position="bottom")+ scale_fill_manual(values=colors)

# assign colors
UA$class_2012 = factor(UA$class_2012, levels = c("Discontinuous dense urban fabric (S.L. : 50% -  80%)","Discontinuous low density urban fabric (S.L. : 10% - 30%)", "Green urban areas",
                                                 "Arable land (annual crops)","Continuous Urban Fabric (S.L. > 80%)","Pastures", "Forests","Industrial, commercial, public, military and private units",
                                                 "Land without current use", "Water", "Isolated structures", "Other roads and associated land"))


#Then you need to define your color vector in the same order.

colors <- c("#4a0000","#9b686a","#209015","#ffb839","#341919", "#8fc78a", "#0c3908", "#7d084d", "#622f2f", "#44eded", "#744e4f", "#51565a")


# asign it to the class
names(colors) <- levels(UA$class_2012)
colScale <- scale_colour_manual(name = "class_2012",values = colors)


#plot all together

(P1 | P2)/(P3 | P4)/(P5 | P6)/(P7 | P8)/(P9 | P10)/(P11)+ plot_layout(ncol=2)

#############################################################################################################
################################################# Plots #####################################################
#############################################################################################################



############################################################################################################################
############################################################################################################################