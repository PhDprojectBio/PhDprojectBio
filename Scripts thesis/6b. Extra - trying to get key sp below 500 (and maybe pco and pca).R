setwd("//home.ansatt.ntnu.no/lcgarcia/Documents/R")

library(janitor)
library(tibble)
library(stringr)
library(dplyr)
library(data.table)
library(sf)
library(s2)
library(stars)
library(ggplot2)
library(wdpar)
library(readxl)

#load("~/R/1900_23_dfDeeps.RData")
deeps <- deeps2 #temporary backup and taking back to that bu, change the 2
#Trying to get an indicator species below 500m
unique(deeps$class)

#filling the columns that are not present in the records with NAs
#and the ones with taxonomic details ready to be filled with the Worms database

taxon <- read.delim("~/R/taxon.txt")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
worms <- right_join(taxon[,c(1,6,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")

#filling up empty fields in class and family
for (i in 1:length(deeps$class)){
  
  if (deeps[i,]$class == "" | is.na(deeps[i,]$class)){
    deeps[i,]$class <- worms$class[match(deeps[i,]$scientificName, worms$scientificName)]
  }
}
rm(i)  

for (i in 1:length(deeps$family)){
  
  if (deeps[i,]$family == "" | is.na(deeps[i,]$family)){
    deeps[i,]$family <- worms$family[match(deeps[i,]$scientificName, worms$scientificName)]
  }
}
rm(i)  

#explore classes for functional groups
unique(deeps$class)
unique(deeps$family)
unique(deeps$scientificName)
deeps %>% filter(class == "") %>% unique()
classtmp <- deeps %>% filter(class == "Mammalia")
unique(classtmp$scientificName)
write.csv(unique(classtmp$scientificName), "Mammalia.csv")
write.csv(unique(deeps$class), "Classesdeeps.csv")

#Exporting classes to ArcGIS
##To do the spatial join in ArcGIS, trying to export in one shape

classesDeeps <- unique(deeps$class)
shapesClasses <- c() 
for(i in 1:length(classesDeeps)){
  shapesClasses[i] <- gsub(" ","",paste(classesDeeps[i],"_complete"))
  x <- deeps %>% filter(class == i)
  assign(gsub(" ","",paste(classesDeeps[i],"_complete")),x)
}
rm(x,i)

for (i in 1:length(shapesClasses)){
  df <- get(shapesClasses[i])
  shp <- st_as_sf(x = df,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","",paste(shapesClasses[i],".shp")), layer_options = "ENCODING=UTF-8", append=FALSE)
}
rm(i,df,shp)
