#loading libraries and directory
setwd("//myDirectory/myFolder/myDocuments/R")

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

#I. Formatting the database (and adding the norwegian expedition for the 1876-1899 time slot).

#Formatting in order to bind databases and get the "depthAccuracy" field in both, which is only present in GBIF
#Change the years accordingly to the required downloads.

gbifa_b1900 -> a
obisb_b1900 -> b

b$eventID = NA
colnames(b)[3] = "dataset"
colnames(a)[3] = "dataset"
colnames(b)[9] = "depthAccuracy"
colnames(a)[11] = "id"
colnames(a)[13] = "flags"
a$database = "gbif"
b$database = "obis"

a = as.data.frame(a)
b = as.data.frame(b)

a$id = as.character(a$id)
a$dateIdentified = as.character(a$dateIdentified)
databases = rbind(a,b)

#If it is needed to import to ArcGIS, rename the fields for coordinates as follows. This keeps both the required formatting for ArcGIS and the coordinates for working in R:

databases$YCoord = databases$decimalLatitude
databases$XCoord = databases$decimalLongitude

databases$year = as.numeric(databases$year)
databases$month = as.numeric(databases$month)
databases$day = as.numeric(databases$day)
databases$coordinateUncertaintyInMeters = as.numeric(databases$coordinateUncertaintyInMeters)
databases$individualCount = as.numeric(databases$individualCount)

databases$scientificName = str_extract(string = databases$scientificName, pattern = "[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}")

#II. Masking the landshape out of databases

x <- st_read("all_landshape.shp")

y <- st_as_sf(x = databases,                         
              coords = c("XCoord", "YCoord"),
              crs = 4326)

y_int <- st_intersects(y,x)
y_log <- lengths(y_int) == 0 
databases_mkd <- y[y_log, ]

#Correction for day and coordinate's fields names

colnames(databases_mkd)[5] = "day"
colnames(databases_mkd)[7] = "XCoord"
colnames(databases_mkd)[6] = "YCoord"

databases_mkd <- as.data.frame(databases_mkd)

#III. Importing contours previously designed in ArcGIS. For this, use the Analysis Tools and Overlay Toolboxes, or the Contour tool in the Spatial Analyst Tools.
###Only running once for working with all the datasets throughout the time series.

#Verifying geometries of contours in R (contours produced in ArcGIS)

x <- st_read("contour0_500.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

    #If needed, use this example to remove a polygon which geometry has not been corrected
    w <- x[147,]

    ggplot() +
      geom_sf(data = w) +
      scale_fill_viridis_c()
    
    x <- x[-147,]
    st_is_valid(x, reason = TRUE)

st_write(x, "contour0_500.shp")

##
x <- st_read("contour500_1000.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour500_1000.shp")

##
x <- st_read("contour1000_1500.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour1000_1500.shp")

##
x <- st_read("contour1500_2000.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour1500_2000.shp")

##
x <- st_read("contour2000_2500.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour2000_2500.shp")

##
x <- st_read("contour2500_3000.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour2500_3000.shp")

##
x <- st_read("contour3000_3500.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour3000_3500.shp")

##
x <- st_read("contour3500_4000.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour3500_4000.shp")

##

#Use this if the contour needs an additional correction in ArcGIS (i.e. vertex went out of boundaries, then exporting and importing again). 
  x <- st_read("contour4000_4500.shp")
  st_is_valid(x, reason = TRUE)
  x <- st_repair_geometry(x)
  st_is_valid(x, reason = TRUE)
  
  st_write(x, "contour4000_4500.shp")
  
  ##
  x <- st_read("contour4500_5000.shp")
  st_is_valid(x, reason = TRUE)
  x <- st_repair_geometry(x)
  st_is_valid(x, reason = TRUE)
  
  st_write(x, "contour4500_5000.shp")

##
x <- st_read("contour5000_5500.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour5000_5500.shp")

##
x <- st_read("contour5500_6000.shp")
st_is_valid(x, reason = TRUE)
x <- st_repair_geometry(x)
st_is_valid(x, reason = TRUE)

st_write(x, "contour5500_6000.shp")

### If there is an isolated polygon. All the nodepth points that are not masked by contours, should coincide with the geometry of this polygon.

x <- st_read("polygon.shp")
st_is_valid(x, reason = TRUE)
x <- st_make_valid(x)
st_is_valid(x, reason = TRUE)

########

# The polygon will be added after the following steps (when masking by depth and latitude, see script 3b).

###III. Including the Norwegian expedition

#Run this with the R assistant if there is any trouble
norwegian_expedition_database <- read_excel("~/R/norwegian_expedition_database.xlsx", 
                                              +     col_types = c("numeric", "numeric", "text", 
                                                                  +         "text", "text", "text", "text", "numeric", 
                                                                  +         "numeric", "text", "text", "text", 
                                                                  +         "numeric", "numeric", "numeric", 
                                                                  +         "numeric", "numeric", "numeric", 
                                                                  +         "numeric", "numeric", "text", "text", 
                                                                  +         "text", "text"))
View(norwegian_expedition_database)
names(norwegian_expedition_database)

#filling the columns that are not present in the records with NAs
  #and the ones with taxonomic details ready to be filled with the Worms database

taxon <- read.delim("~/R/taxon.txt")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
worms <- right_join(taxon[,c(1,6,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")

#filling up fields 
norwegian_expedition_database$coordinateUncertaintyInMeters <- NA
norwegian_expedition_database$class <- worms$class[match(norwegian_expedition_database$scientificNameSpecies, worms$scientificName)]

for (i in 1:length(norwegian_expedition_database$class)){
  
  if ((norwegian_expedition_database[i,]$class == "" | is.na(norwegian_expedition_database[i,]$class)) & (norwegian_expedition_database[i,]$group == "Holothuroidea"
                                                    | norwegian_expedition_database[i,]$group == "Asteroidea"
                                                    | norwegian_expedition_database[i,]$group == "Ophiuroidea")
  ){
    norwegian_expedition_database[i,]$class <- norwegian_expedition_database[i,]$group
  }
}

rm(i)
norwegian_expedition_database$dataset <- NA
norwegian_expedition_database$dateIdentified <- NA
#norwegian_expedition_database$day <- norwegian_expedition_database$day
norwegian_expedition_database$decimalLatitude <- norwegian_expedition_database$yCoord
norwegian_expedition_database$decimalLongitude <- norwegian_expedition_database$xCoord

norwegian_expedition_database$depth <- norwegian_expedition_database$minDepthm

for (i in 1:length(norwegian_expedition_database$depth)){
  if (!is.na(norwegian_expedition_database[i,]$maxDepthm)){
    if (norwegian_expedition_database[i,]$minDepthm >0 & norwegian_expedition_database[i,]$maxDepthm > 0){
      
      norwegian_expedition_database[i,]$depth <-round(rowMeans(norwegian_expedition_database[i,][,c('maxDepthm', 'minDepthm')]), digits = 0)
    }
  }
}

rm(i)
norwegian_expedition_database$depthAccuracy <- NA
norwegian_expedition_database$family <- worms$family[match(norwegian_expedition_database$scientificNameSpecies, worms$scientificName)]
norwegian_expedition_database$id <- norwegian_expedition_database$recordId
norwegian_expedition_database$individualCount <- NA
norwegian_expedition_database$flags <- norwegian_expedition_database$observations
norwegian_expedition_database$kingdom <- worms$kingdom[match(norwegian_expedition_database$scientificNameSpecies, worms$scientificName)]

#kingdom classification for Annelida, Mollusca and Polyzoa phylum
for (i in 1:length(norwegian_expedition_database$kingdom)){
  
  if ((norwegian_expedition_database[i,]$kingdom == "" | is.na(norwegian_expedition_database[i,]$kingdom)) & (norwegian_expedition_database[i,]$group == "Fish" 
                                                                                                              | norwegian_expedition_database[i,]$group == "Annelida" 
                                                                                                              | norwegian_expedition_database[i,]$group == "Mollusca"
                                                                                                              | norwegian_expedition_database[i,]$group == "Holothuroidea"
                                                                                                              | norwegian_expedition_database[i,]$group == "Asteroidea"
                                                                                                              | norwegian_expedition_database[i,]$group == "Gephyrea"
                                                                                                              | norwegian_expedition_database[i,]$group == "Ophiuroidea")
  ){
    norwegian_expedition_database[i,]$kingdom <- "Animalia"
  }
  
  if ((norwegian_expedition_database[i,]$kingdom == "" | is.na(norwegian_expedition_database[i,]$kingdom)) & (norwegian_expedition_database[i,]$group == "Cilioflagellata" 
                                                                                                              | norwegian_expedition_database[i,]$group == "Silicoflagellata")
  ){
    norwegian_expedition_database[i,]$kingdom <- "Chromista"
  } 
} 

rm(i)
#norwegian_expedition_database$month <- norwegian_expedition_database$month
norwegian_expedition_database$scientificName <- norwegian_expedition_database$scientificNameSpecies
norwegian_expedition_database$taxonRank <- "SPECIES"
#norwegian_expedition_database$year <- norwegian_expedition_database$year
norwegian_expedition_database$database <- "norexp_1876_78"
norwegian_expedition_database$YCoord <- norwegian_expedition_database$yCoord
norwegian_expedition_database$XCoord <- norwegian_expedition_database$xCoord

#order by the order in databases (GBIF and OBIS)
names_forexpedition <- names(databases_mkd)[-20]
nor_exp <- norwegian_expedition_database[, names_forexpedition]
nor_exp[20] <- nor_exp[c("XCoord")]
nor_exp[21] <- nor_exp[c("YCoord")]
colnames(nor_exp)[20] <- "XCoordGeom"
colnames(nor_exp)[21] <- "YCoordGeom"

#Masking out landmases

x <- st_read("all_landshape.shp")

#splitting in NA and no NA parts to be able to perform the masking
nor_exp_noNA <- nor_exp %>% filter(!is.na(nor_exp$XCoord) | !is.na(nor_exp$YCoord))
nor_exp_NA <- nor_exp %>% filter(is.na(nor_exp$XCoord) | is.na(nor_exp$YCoord))
nor_exp_NA$geometry <- NA 
nor_exp_NA <- nor_exp_NA[-c(20,21)]

y <- st_as_sf(x = nor_exp_noNA,                         
              coords = c("XCoordGeom", "YCoordGeom"),
              crs = 4326)

y_int <- st_intersects(y,x)
y_log <- lengths(y_int) == 0 
databases_mkd_nor <- y[y_log, ]
databases_mkd_nor <- as.data.frame(databases_mkd_nor)
databases_mkd_nor <- rbind(databases_mkd_nor, nor_exp_NA)
colnames(databases_mkd_nor)[7] = "XCoord"
colnames(databases_mkd_nor)[6] = "YCoord"

#add to GBIF+OBIS (databases_mkd)
#load("~/R/1876_1899a.RData")
databases_mkd = rbind(databases_mkd,databases_mkd_nor)

###End of the script
