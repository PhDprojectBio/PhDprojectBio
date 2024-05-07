#Extra 1. Confirming species of l84 in worms (just take until colnamesaux3)

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


##For keeping running
#setwd("//home.ansatt.ntnu.no/lcgarcia/Documents/R")
#load("~/R/2020_23a.RData")
#...
#rm(databases,databases_mkd)
#save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2020_23b.RData")

#rm(list = ls())


#names of species for latitude 84
aux <- convtable_0_500_sp_nodups[29,]
aux2 <- which (aux > 0)
aux3 <- aux[aux2]
colnames(aux3)


#mn <- 0
#mx <- 500

#for(i in 1:12){

#shp <- st_as_sf(x = get(gsub(" ","", paste("mkd_",mn,"_",mx))),                         
 #               coords = c("XCoord", "YCoord"),
  #              crs = 4326)

#st_write(shp, gsub(" ","", paste("mkd_",mn,"_",mx,".shp")))

#mn = mn + 500
#mx = mx + 500

#}

#shp <- st_as_sf(x = polygon,                         
 #               coords = c("XCoord", "YCoord"),
  #              crs = 4326)

#st_write(shp, "polygon.shp")

#rm(i)


sp_list_class_dom <- sp_list2 %>% arrange(class) %>% group_by(class) %>% mutate(maximum = max(abundance)) %>% filter(abundance == maximum)
dom_sp_class <- sp_list_class_dom$scientificName
sp_list_class_dom2 <- dbs_mkd_taxa_new %>% arrange(class) %>% group_by(class) %>% subset(scientificName %in% dom_sp_class)
sp_list_class_shp <- dbs_mkd_taxa_new %>% arrange(class) %>% group_by(class) %>% group_split()


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


