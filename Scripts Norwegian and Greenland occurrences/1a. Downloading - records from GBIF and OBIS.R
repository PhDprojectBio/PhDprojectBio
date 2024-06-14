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

#Steps to download records from GBIF and OBIS.
# II. For OBIS, analogue procedure as with GBIF

install.packages("dismo")
library(dismo)
install.packages("robis")
library(robis)

#obis_1876to99
obisb_1876to99 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", enddate = as.Date("1899-12-31"), absence = NULL, flags = NULL)
obisb_1876to99 = obisb_1876to99[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1900to49
obisb_1900to49 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1900-01-01"), enddate = as.Date("1949-12-31"), absence = NULL, flags = NULL)
obisb_1900to49 = obisb_1900to49[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1950to59 
obisb_1950to59 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1950-01-01"), enddate = as.Date("1959-12-31"), absence = NULL, flags = NULL)
obisb_1950to59 = obisb_1950to59[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1960to69
obisb_1960to69 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1960-01-01"), enddate = as.Date("1969-12-31"), absence = NULL, flags = NULL)
obisb_1960to69 = obisb_1960to69[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1970to79
obisb_1970to79 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1970-01-01"), enddate = as.Date("1979-12-31"), absence = NULL, flags = NULL)
obisb_1970to79 = obisb_1970to79[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1980to89
obisb_1980to89 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1980-01-01"), enddate = as.Date("1989-12-31"), absence = NULL, flags = NULL)
obisb_1980to89 = obisb_1980to89[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1990to99
obisb_1990to99 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1990-01-01"), enddate = as.Date("1999-12-31"), absence = NULL, flags = NULL)
obisb_1990to99 = obisb_1990to99[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2000to04
obisb_2000to04 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2000-01-01"), enddate = as.Date("2004-12-31"), absence = NULL, flags = NULL)
obisb_2000to04 = obisb_2000to04[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2005to09
obisb_2005to09 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2005-01-01"), enddate = as.Date("2009-12-31"), absence = NULL, flags = NULL)
obisb_2005to09 = obisb_2005to09[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2010to14
obisb_2010to14 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2010-01-01"), enddate = as.Date("2014-12-31"), absence = NULL, flags = NULL)
obisb_2010to14 = obisb_2010to14[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2015to19
obisb_2015to19 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2015-01-01"), enddate = as.Date("2019-12-31"), absence = NULL, flags = NULL)
obisb_2015to19 = obisb_2015to19[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2020to23
obisb_2020to23 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2020-01-01"), absence = NULL, flags = NULL)
obisb_2020to23 = obisb_2020to23[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

##Request from EMODnet

# load dependencies
## geospatial data handling
library(rgdal)
library(raster)
library(sp)
library(mapdata)
library(ncdf4)
library(XML)
library(dplyr)
library(tidyr)
library(reshape2)
library(downloader)
library(directlabels)
library(rasterVis)
library(ggplot2)
library(knitr)
library(IRdisplay)
library(repr)

remotes::install_github("EMODnet/EMODnetWFS")
library("EMODnetWFS")
#View(emodnet_wfs())
wfs_bio <- emodnet_init_wfs_client(service = "biology_occurrence_data")
info <- emodnet_get_wfs_info(wfs_bio)

xmin= -27
ymin= 56
xmax= 38
ymax= 85
layer <- emodnet_get_layers(
  service = "biology_occurrence_data",
  layers = c("eurobis-obisenv"), 
  BBOX = paste(xmin,ymin,xmax,ymax,sep=",")
)

layer <- layer[["eurobis-obisenv"]]

#Formatting EMODnet accordingly to GBIF+OBIS fields

names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)
names(layer)

### Apply the filters

#layerbu <- layer
#layer <- layerbu
layer <- layer[,c(35,50,4,66,16,37,38,79,52,2,68,57,48,13,40,43,10,123,29,32)]
names(layer)

layer <- as.data.frame(layer)

names(layer)[1] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[1] #"coordinateuncertaintyinmeters" to "coordinateUncertaintyInMeters"
names(layer)[3] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[3] #"datasetid" to "dataset" 
names(layer)[4] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[4] #"dayidentified" to "dateIdentified"
names(layer)[5] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[5] #"daycollected" to "day"
#to calculate a (mean) value for the depth
layer$depth <- (layer$minimumdepthinmeters + layer$maximumdepthinmeters)/2
#remove one of the columns for depth
layer <- layer[-7]
layer[6] <- layer[20]
layer <- layer[-20]
names(layer)
names(layer)[6] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[6] #"minimumdepthinmeters"and "maximumdepthinmeters" to "depth" (mean)
names(layer)[7] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[7] #"depthrange" to "depthAccuracy"
names(layer)[10] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[10] #"samplingeffort" to "individualCount"
names(layer)[11] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[11] #"occurrenceremarks" to "flags"
names(layer)[13] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[13] #"monthcollected" to "month"
names(layer)[14] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[14] #"scientificname" to "scientificName"
names(layer)[15] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[15] #"taxonrank" to "taxonRank"
names(layer)[16] <- names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)[16] #"yearcollected" to "year"
names(layer)
names(dbs_mkd_taxa_new1900_49_deep_diffmatch_nodups)
layer$database <- "emodnet"
layer <- layer[-17]
names(layer)
layer$XCoord <- layer$decimallatitude
layer$YCoord <- layer$decimallongitude
layer$decimalLatitude <- layer$YCoord
layer$decimalLongitude <- layer$XCoord
layer <- layer[-17]
layer <- layer[-17]
names(layer)

databases <- layer


#Every time slot can be saved as a separate archive to process with the following scripts.
###End of the script
