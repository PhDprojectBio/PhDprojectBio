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

#join to GBIF+OBIS (databases_mkd)
#load("~/R/1876_1899a.RData")
databases_mkd = rbind(databases_mkd,databases_mkd_nor)
#Save this together with 1876_99a.RData, to now include the Norwegian Expedition files.

###End of the script
