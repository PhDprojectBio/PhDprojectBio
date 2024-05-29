#Extra2: Splitting classes with the split of the 0to500 and the 500plus data

#install.packages("qdapTools")

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
#library(sqldf)
#library(plyr)
#library(qdapTools)

dbs_mkd_taxa_emptyclass <- dbs_mkd_taxa_new %>% filter(class=="")

write.table(dbs_mkd_taxa_emptyclass, file = "matlab_emptyclass.txt", sep = "\t", na = "NA", dec = ".", row.names = FALSE, quote = FALSE)
#assign(gsub(" ","", paste("shp_",name)), class_shp)

shp <- st_as_sf(x = dbs_mkd_taxa_emptyclass,                         
                coords = c("XCoord", "YCoord"),
                crs = 4326)

st_write(shp, "shp_emptyclass.shp")


