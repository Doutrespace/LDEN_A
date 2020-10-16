# Create import function
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
       "RColorBrewer", "biscale", "magrittr", "lintr"))
#############################################TMP#############################################################
# Import UA GPKG 
UrbAtl_Pol <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/2.Urban_Atlas/Koblenz_Urban_Atlas.gpkg") 
# Load area GPKG
Area <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/3.Area/Koblenz_citycore_aoi.gpkg") 
# Import NOISE TIFF
Noise_R <- raster("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/0.Noise_Rst/koblenz_citycore_r.tif")
# Check which polygons overlay on ratser
UrbanAtlas <- st_intersection(UrbAtl_Pol, Area)  
# Convert to SP to apply aggregate function
UrbanAtlas <- as(UrbanAtlas[,5], 'Spatial')
# Merge polygons per class 
UrbanAtlas <- aggregate(UrbanAtlas, by = "ITEM2012")
# Switch back to SF to apply raster::extract
UrbanAtlas <- st_as_sf(UrbanAtlas)
# Extraxt values Takes 140 seg
ptm <- proc.time()
UrbanAtlas <-
  UrbanAtlas %>% mutate(
    PixVals = raster::extract(Noise_R, UrbanAtlas)
  )
proc.time() - ptm

# Count NA values 
UrbanAtlas$NumNa <- 0 
UrbanAtlas$Mean <- 0
UrbanAtlas$Median <- 0
UrbanAtlas$Mode <- 0
UrbanAtlas$Sd <- 0
UrbanAtlas$Area <- 0

# Function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for(i in 1:length(UrbanAtlas$PixVals)){
  UrbanAtlas$NumNa[i] <- sum(is.na(UrbanAtlas$PixVals[[i]]))
  UrbanAtlas$Mean[i] <-  mean(UrbanAtlas$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas$Median[i] <- median(UrbanAtlas$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas$Sd[i] <- sd(UrbanAtlas$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas$Mode <- getmode(UrbanAtlas$PixVals[[i]])
  UrbanAtlas$Area[i] <- st_area(UrbanAtlas$geometry[i])
}

ggplot(UrbanAtlas) +
  geom_point(aes(x=Area, y=NumNa))+
  theme(axis.text.x = element_text(angle = 90))

boxplot(UrbanAtlas$PixVals)


#############################################TMP############################################################# To apply NOVA 
# Import UA GPKG 
UrbAtl_Pol <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/2.Urban_Atlas/Koblenz_Urban_Atlas.gpkg") 
# Load area GPKG
Area <- st_read("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/3.Area/Koblenz_citycore_aoi.gpkg") 
# Import NOISE TIFF
Noise_R <- raster("C:/Users/Nillus/OneDrive/Desktop/LDEN_A/Main_Fo/Data/Koblenz_Data/0.Noise_Rst/koblenz_citycore_r.tif")
# Check which polygons overlay on ratser
UrbanAtlas <- st_intersection(UrbAtl_Pol, Area)  
# Extract values Takes AGES
ptm <- proc.time()
UrbanAtlas <-
  UrbanAtlas %>% mutate(
    PixVals = raster::extract(Noise_R, UrbanAtlas)
  )
proc.time() - ptm

# Clone UrbanAtlas sussy move 
UrbanAtlas_TST <- UrbanAtlas

UrbanAtlas_TST$NumNa <- 0 
UrbanAtlas_TST$Mean <- 0
UrbanAtlas_TST$Median <- 0
UrbanAtlas_TST$Mode <- 0
UrbanAtlas_TST$Sd <- 0
UrbanAtlas_TST$Area <- 0

# Function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for(i in 1:length(UrbanAtlas_TST$ITEM2012)){
  UrbanAtlas_TST$NumNa[i] <- sum(is.na(UrbanAtlas_TST$PixVals[[i]]))
  UrbanAtlas_TST$Mean[i] <-  mean(UrbanAtlas_TST$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas_TST$Median[i] <- median(UrbanAtlas_TST$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas_TST$Sd[i] <- sd(UrbanAtlas_TST$PixVals[[i]],na.rm = TRUE)
  UrbanAtlas_TST$Mode[i] <- getmode(UrbanAtlas_TST$PixVals[[i]])
  UrbanAtlas_TST$Area[i] <- st_area(UrbanAtlas_TST$geometry[i])
}

# Compute the analysis of variance
res.aov <- aov(Mode ~ ITEM2012, data = UrbanAtlas_TST)

# Summary of the analysis
summary(res.aov)

# Tukey multiple pairwise-comparisons

TukeyHSD(res.aov)

plot(res.aov, 1)


ggplot(UrbanAtlas_TST) +
  geom_point(aes(x=Area, y=Sd))+
  theme(axis.text.x = element_text(angle = 90))
















