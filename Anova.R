# anova
# Check which polygons overlay on ratser
UrbanAtlas <- st_intersection(UrbAtl_Pol, Area)  

# Extraxt values
UrbanAtlas <-
  UrbanAtlas %>% mutate(
    PixVal = raster::extract(Noise_R, UrbanAtlas)
  )

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


UrbanAtlas$Mode_dB <- -1

for(i in 1:length(UrbanAtlas$PixVal)){
  UrbanAtlas$Mode_dB[i]  <- Mode(UrbanAtlas$PixVal[[i]])
  
}

UrbanAtlas_F <- UrbanAtlas

UrbanAtlas_F <- UrbanAtlas_F[!is.na(UrbanAtlas_F$Mode_dB),]

UrbanAtlas_F <- as.data.frame(UrbanAtlas_F)

UrbanAtlas_F <- UrbanAtlas_F[,c(5,19)]



library(dplyr)
surveys_spread <- UrbanAtlas_F  %>% group_by(ITEM2012) %>% across(Mode_dB)



Residential <- c("Continuous urban fabric (S.L. : > 80%)",
                 "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                 "Discontinuous dense urban fabric (S.L. : 50% -  80%)",
                 "Discontinuous medium density urban fabric (S.L. : 30% - 50%)",
                 "Discontinuous low density urban fabric (S.L. : 10% - 30%)",
                 "Discontinuous very low density urban fabric (S.L. : < 10%)")

Industrial <- c("Industrial, commercial, public, military and private units")

Others <- c("Isolated structures")

Traffic_Infrastructures <- c("Railways and associated land","Fast transit roads and associated land",
                             "Other roads and associated land","Port areas","Airports")


Non_Residential <- c("Mineral extraction and dump sites","Construction sites","Land without current use")


Recreational <- c("Green urban areas")

Agricultural <- c("Arable land (annual crops)", "Pastures")

Green_Spaces <- c("Water","Forests","Herbaceous vegetation associations (natural grassland, moors...)")



for(i in 1:length(UrbanAtlas_F$ITEM2012)){
  if(UrbanAtlas_F$ITEM2012[i] %in% Residential){UrbanAtlas_F$ITEM2012[i] <- "Residential"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Industrial){UrbanAtlas_F$ITEM2012[i] <- "Industrial"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Others){UrbanAtlas_F$ITEM2012[i] <- "Others"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Traffic_Infrastructures){UrbanAtlas_F$ITEM2012[i] <- "Traffic Infrastructures"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Non_Residential){UrbanAtlas_F$ITEM2012[i] <- "Non Residential artificial structures"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Recreational){UrbanAtlas_F$ITEM2012[i] <- "Recreational"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Agricultural){UrbanAtlas_F$ITEM2012 [i] <- "Agricultural areas"}
  if(UrbanAtlas_F$ITEM2012[i] %in% Green_Spaces){UrbanAtlas_F$ITEM2012[i] <- "Green Spaces"}
  
}



levels(UrbanAtlas_F$ITEM2012)

# median statistics

group_by(UrbanAtlas_F, ITEM2012) %>%
  summarise(
    count = n(),
    mean = mean(Mode_dB, na.rm = TRUE),
    sd = sd(Mode_dB, na.rm = TRUE)
  )

# plot statistics

ggboxplot(UrbanAtlas_F, x = "ITEM2012", y = "Mode_dB", 
          color = "ITEM2012", palette = c("#00AFBB", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC"),
          order = c("Residential", "Industrial", "Non Residential artificial structures"," Agricultural areas ", "Green Spaces", "Others",
                    "Recreational", "Sports and leisure facilities", "Traffic Infrastructures"),
          ylab = "Weight", xlab = "Treatment")

# Plot weight by group
ggline(UrbanAtlas_F, x = "ITEM2012", y = "Mode_dB", 
       add = c("mean_se", "jitter"), 
       order = c("Residential", "Industrial", "Non Residential artificial structures"," Agricultural areas ", "Green Spaces", "Others",
                 "Recreational", "Sports and leisure facilities", "Traffic Infrastructures"),
       ylab = "Weight", xlab = "Treatment")

# Compute the analysis of variance
res.aov <- aov(Mode_dB ~ ITEM2012, data = UrbanAtlas_F)

# Summary of the analysis
summary(res.aov)

# Tukey multiple pairwise-comparisons

TukeyHSD(res.aov)

plot(res.aov, 1)


# Tukey Boxplot



# I need to group the treatments that are not different each other together.
generate_label_df <- function(res.aov, variable){
  
  # Extract labels and factor levels from Tukey post-hoc 
  Tukey.levels <- res.aov[[variable]][,4]
  Tukey.labels <- data.frame(multcompLetters(Tukey.levels)['Letters'])
  
  #I need to put the labels in the same order as in the boxplot :
  Tukey.labels$ITEM2012=rownames(Tukey.labels)
  Tukey.labels=Tukey.labels[order(Tukey.labels$ITEM2012) , ]
  return(Tukey.labels)
}

# Apply the function on my dataset
LABELS <- generate_label_df(res.aov , "UrbanAtlas_F$ITEM2012")


# A panel of colors to draw each group with the same color :
my_colors <- c( 
  rgb(143,199,74,maxColorValue = 255),
  rgb(242,104,34,maxColorValue = 255), 
  rgb(111,145,202,maxColorValue = 255)
)

# Draw the basic boxplot
a <- boxplot(UrbanAtlas_F$Mode_dB ~ UrbanAtlas_F$ITEM2012 , ylim=c(min(UrbanAtlas_F$Mode_dB) , 1.1*max(UrbanAtlas_F$Mode_dB)) , col=my_colors[as.numeric(LABELS[,1])] , ylab="Mode_dB" , main="")

# I want to write the letter over each box. Over is how high I want to write it.
over <- 0.1*max( a$stats[nrow(a$stats),] )

#Add the labels
text( c(1:nlevels(UrbanAtlas_F$ITEM2012)) , a$stats[nrow(a$stats),]+over , LABELS[,1]  , col=my_colors[as.numeric(LABELS[,1])] )





