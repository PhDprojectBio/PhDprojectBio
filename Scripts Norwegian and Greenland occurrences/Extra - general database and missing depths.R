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


###End of the script
