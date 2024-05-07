#Extra2: Splitting classes with the split of the 0to500 and the 500plus data

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


taxon <- read.delim("~/R/taxon.txt")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
worms <- right_join(taxon[,c(1,6,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")
#worms <- worms[,c(2,5)]

#container <- dbs_mkd_taxa_new %>% filter(class=="")

#to fill empty spaces for class
#dbs_mkd_taxa_emptyclass <- dbs_mkd_taxa_new %>% filter(class=="")

  dbs_mkd_taxa_new$classWorms <- worms$class[match(dbs_mkd_taxa_new$family, worms$family)]
  dbs_mkd_taxa_new$classworms <- worms$class[match(dbs_mkd_taxa_new$scientificName, worms$scientificName)]

for (i in 1:length(dbs_mkd_taxa_new$class)){

    if (dbs_mkd_taxa_new[i,]$class == "" & dbs_mkd_taxa_new[i,]$classWorms == ""){
      
      dbs_mkd_taxa_new[i,]$class <- dbs_mkd_taxa_new[i,]$classworms
    }
  
    if (dbs_mkd_taxa_new[i,]$class == "" & dbs_mkd_taxa_new[i,]$classWorms != ""){
      
      dbs_mkd_taxa_new[i,]$class <- dbs_mkd_taxa_new[i,]$classWorms
    }
}

rm(i)
  
#Filtering above and below 500m  
    
dbs_mkd_taxa_shallow <- dbs_mkd_taxa_new %>% filter(depth<500)
dbs_mkd_taxa_deep <- dbs_mkd_taxa_new %>% filter(depth>=500)


#Exporting per class to shapefiles and tables for matlab

sp_list2 <- dbs_mkd_taxa_deep %>% arrange(scientificName) %>% group_by(scientificName, kingdom, class, family) %>% summarise(abundance = sum(individualCount)) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
sp_list <- sp_list2[c("scientificName","abundance")] %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(abundance))


sp_list_class_dom <- sp_list2 %>% arrange(class) %>% group_by(class) %>% mutate(maximum = max(abundance)) %>% filter(abundance == maximum)
dom_sp_class <- sp_list_class_dom$scientificName
sp_list_class_dom2 <- dbs_mkd_taxa_deep %>% arrange(class) %>% group_by(class) %>% subset(scientificName %in% dom_sp_class)
sp_list_class_shp <- dbs_mkd_taxa_deep %>% arrange(class) %>% group_by(class) %>% group_split()


#Exporting per class

length(sp_list_class_shp)

for (i in 1:length(sp_list_class_shp)){
  class_shp <- sp_list_class_shp[[i]]
  name <- class_shp$class[1]
  write.table(class_shp, file = gsub(" ","", paste("matlab_",name,".txt")), sep = "\t", na = "NA", dec = ".", row.names = FALSE, quote = FALSE)
  #assign(gsub(" ","", paste("shp_",name)), class_shp)
  
  shp <- st_as_sf(x = class_shp,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","", paste("shp_",name,".shp")))
}

rm(i)

shp <- st_as_sf(x = sp_list_class_dom2,                         
                coords = c("XCoord", "YCoord"),
                crs = 4326)

st_write(shp, "dominant_taxa_class.shp")



write.table(sp_list, file = "splist.txt", sep = "\t", na = "NA", dec = ".", row.names = FALSE, quote = FALSE)
write.table(sp_list_class_dom, file = "splist_dom.txt", sep = "\t", na = "NA", dec = ".", row.names = FALSE, quote = FALSE)
