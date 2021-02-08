#############################################################################################################
######################################## OBJECTIVE ##########################################################
#############################################################################################################
# Polygon - Polygon Extract:
# First part on a series of codes to build a protocol to extract db values from a Noise Polygon. 
# The data is provided from each municipal in the form of a Multipolygon. This code aims extract the Polygon
# values in each intersecting Polygon of the Urban Atlas and to in the end calculate
# statistics with it. 
#
# Code made by: Nils Karges
# Email: nils.karges@stud-mail.uni-wuerzburg.de
# 
#############################################################################################################
######################################## LIBRARIES ##########################################################
#############################################################################################################
# Ceate import function
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
}

# Load libraries
ipak(c("sp","sf","raster","tidyverse", "dplyr", "dbplyr", "rgdal", "ggplot2","rgeos","crayon","tictoc", "vroom"))

#############################################################################################################
########################################## DataFrame Creation ###############################################
#############################################################################################################

CreateDf <- function(City_Dir){
  
  # Read available data
  available_Files <- list.files(path = City_Dir, full.names = TRUE)
  available_Files_N <- list.files(path = City_Dir, full.names = F)
  
  # Check data aviability
  if( ((length(grep("UA", list.files(City_Dir,  full.names = F))) > 0) &
       (length(grep("Noise", list.files(City_Dir,  full.names = F))) > 0) &
       ((length(grep(".csv", list.files(City_Dir,  full.names = F))) > 0) == FALSE)) == FALSE){
    
    cat(red("-------------------------------------------|\n"))
    cat(red("No neccessary files found for              |\n"))
    cat(red(paste0(available_Files_N[1],"\n")))
    cat(red("-------------------------------------------|\n"))
  } else{
    
    cat(cyan("-------------------------------------------|\n"))
    cat(cyan(paste0(available_Files_N[1],"\n")))
    cat(cyan("-------------------------------------------|\n"))
    # Build DATAFRAME
    ptm <- proc.time()
    
    cat(cyan("Reading available Files!..."))
    
    # Special case of multilayer, create list of layers
    Av_layers <- st_layers(grep("UA", available_Files, value=TRUE))
    
    AOI_Name <- Av_layers[["name"]][grep("UrbanCore", Av_layers[["name"]])]
    AOI_Pol <- st_read(grep("UA", available_Files, value=TRUE), layer = AOI_Name, quiet = T)
    
    UA_Name <- Av_layers[["name"]][Av_layers[["features"]] > 1]
    UA_Pol <- st_read(grep("UA", available_Files, value=TRUE), layer = UA_Name, quiet = T)
    UA_Pol <- st_transform(UA_Pol, crs = st_crs(AOI_Pol))
    #print(lengths(st_intersects(AOI_Pol,UA_Pol)))
    
    # Read poligons -> add a gate in case no pol is found
    Noise_Pol <- st_read(grep("Noise", available_Files, value=TRUE), quiet = T)
    Noise_Pol <- st_transform(Noise_Pol, crs = st_crs(AOI_Pol))
    Noise_Pol <- st_buffer(Noise_Pol, 0.001)
    
    # Create a difference between AOI_Pol and Noise_Pol
    Noise_0 <-  st_difference(AOI_Pol, st_union(Noise_Pol))
    
    # Merge final Noise SF
    Noise_0 <- st_cast(Noise_0, "POLYGON")
    
    Noise_0_Db <- lapply(1:nrow(Noise_0), function(i){ st_cast(Noise_0[i, ], "POLYGON") }) %>%
      do.call(rbind, .)
    
    names(Noise_0_Db) <- c(names(Noise_0_Db)[1:length(names(Noise_0_Db))-1], "geom")
    st_geometry(Noise_0_Db) <- "geom"
    
    # Create new dataframe
    Noise_Pol_F <- data.frame(matrix(ncol = length(names(Noise_Pol)), nrow = length(Noise_0_Db$geom)))
    colnames(Noise_Pol_F) <- c(names(Noise_Pol))
    Noise_Pol_F$geom <- Noise_0_Db$geom
    Noise_Pol_F$DB_Low <- "0"
    
    Noise_Pol_F <- rbind(Noise_Pol_F,Noise_Pol)
    Noise_Pol_F <- st_as_sf(Noise_Pol_F)
    st_geometry(Noise_Pol_F) <- "geom"
    cat(green("            OK"))
    cat(cyan("  |\n"))
    
    # Transfer to SP objects
    cat(cyan("Transforming to SP objects!..."))
    UA_Pol <- as(UA_Pol, 'Spatial')
    AOI_Pol <- as(AOI_Pol, 'Spatial')
    Noise_Pol <- as(Noise_Pol_F, 'Spatial')
    cat(green("         OK"))
    cat(cyan("  |\n"))
    
    # Fix the MF single topos (width as small as possible)
    Noise_Pol <- gBuffer(Noise_Pol, byid=TRUE, width=0.00001)
    
    # Crop to reduce size 
    cat(cyan("Intersecting poligons to AOI!..."))
    tryCatch({
      Noise_Pol_C <- raster::intersect(Noise_Pol, AOI_Pol)}) #x[subsx, ] is invalid
    tryCatch({
      UA_Pol_C <- raster::intersect(UA_Pol, AOI_Pol)})
    cat(green("       OK"))
    cat(cyan("  |\n"))
    
    # Create Available dB vals and UA classes
    dB_Vals <- as.character(paste0("dB_",unique(sort(Noise_Pol_C$DB_Low))))
    Class_Vals <- unique(sort(UA_Pol_C$class_2018))
    
    # Create Main Dataframe
    Data_Raw <- data.frame(matrix(ncol=length(dB_Vals)+1, nrow=length(Class_Vals)))
    names(Data_Raw) <- c("Class",dB_Vals)
    Data_Raw$Class <- Class_Vals
    
    # Simplify polygons with same dB Val category
    cat(cyan("Aggregating noise poligons!..."))
    Noise_Pol_F <- aggregate(Noise_Pol_C, by = "DB_Low")
    rm(Noise_Pol, Noise_Pol_C)
    cat(green("         OK"))
    cat(cyan("  |\n"))
    
    # Simplify polygons with same Class category
    cat(cyan("Aggregating urban atlas poligons!..."))
    UA_Pol_F <- aggregate(UA_Pol_C, by = "class_2018")
    rm(UA_Pol, UA_Pol_C)
    cat(green("   OK"))
    cat(cyan("  |\n"))
    
    # Intersect GPKG UA_Pol and Noise_Pol_F
    cat(cyan("Intersecting UA and Noise poligons!..."))
    Pol_Intersect <- st_intersection(st_as_sf(UA_Pol_F), st_as_sf(Noise_Pol_F))
    cat(green(" OK"))
    cat(cyan("  |\n"))
    
    # Calculate Poligons Area
    cat(cyan("Extracting values!..."))
    Pol_Intersect$Area <- st_area(Pol_Intersect$geometry)
    
    # Convert to dataframe
    Pol_Intersect_F <- Pol_Intersect %>% st_drop_geometry()
    
    # Add "_dB" and change to character
    Pol_Intersect_F$DB <- paste0("dB_",as.character(Pol_Intersect_F$DB_Low))
    
    # Assign Column and row levels
    Pol_Intersect_F$Row <- match(Pol_Intersect_F$class_2018,Data_Raw$Class)
    Pol_Intersect_F$Col <- match(Pol_Intersect_F$DB,names(Data_Raw))
    
    # Assign area values to final dataframe
    for(i in 1:length(Pol_Intersect_F$Area)){
      Data_Raw[Pol_Intersect_F$Row[i],Pol_Intersect_F$Col[i]] <- Pol_Intersect_F$Area[i]
    }
    
    # Clear workspace
    rm(Noise_Pol_F, Pol_Intersect, Pol_Intersect_F, UA_Pol_F, Av_layers, UA_Name)
    
    # Change NA to 0
    Data_Raw[is.na(Data_Raw)] <- 0
    
    # Create the final index number to sum up
    finsum <- length(names(Data_Raw))
    
    # Sum up the areas of the poligons
    Data_Raw$Area <- rowSums(Data_Raw[2:finsum])
    
    # Filter dataframe to dump non existing classes
    Data_Raw <- filter(Data_Raw, Area !=  0)
    
    # Make Total Normalized Dataframe 
    Tot <- sum(Data_Raw$Area)
    Data_NT <- Data_Raw
    Data_NT[,2:length(names(Data_NT))] <- lapply(Data_NT[,2:length(names(Data_Raw))], FUN = function(x) return(x * 100 / Tot))
    
    # Make Total Normalized Dataframe 
    Data_NC <- Data_Raw
    area <- Data_NC$Area
    Data_NC[,3:length(names(Data_Raw))-1] <- Data_NC[,3:length(names(Data_Raw))-1] <- lapply(Data_NC[,3:length(names(Data_Raw))-1], FUN = function(x) return(x * 100 / area)) 
    
    cat(green("                  OK"))
    cat(cyan("  |\n"))
    
    cat(cyan("Exporting values!..."))
    # export as csv
    write.csv(x=Data_Raw, file=paste0(City_Dir,"\\Result_Raw.csv"), sep = ",")
    write.csv(x=Data_NT,  file=paste0(City_Dir,"\\Result_NT.csv"), sep = ",")
    write.csv(x=Data_NC,  file=paste0(City_Dir,"\\Result_NC.csv"), sep = ",")
    cat(green("                   OK"))
    cat(cyan("  |\n"))
    
    # Show time
    time <- proc.time() - ptm
    cat(cyan("Total time: "))
    cat(red(time[3]))
    cat(cyan("\n"))
    rm(time)
    
    cat(cyan("-------------------------------------------|\n"))
    
  }
}

#################################################################################################################################
#################################################################################################################################
#################################################### Run the Script #############################################################
#################################################################################################################################
#################################################################################################################################

# RUN all the cities gathered in one folder
Main_Fo <- choose.dir()
Av_Cities <- list.files(Main_Fo,  full.names = TRUE)
mapply(function(x) CreateDf(x), Av_Cities)

# RUN just one city
CreateDf("D:\\LDEN_A\\Main_Fo\\Data\\Hamburg_Data")

#################################################################################################################################
#################################################################################################################################
######################################################## End ####################################################################
#################################################################################################################################
#################################################################################################################################
