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




###End of the script
