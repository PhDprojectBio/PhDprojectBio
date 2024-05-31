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

#IV. Calculate the summaries for the time-series

#Overall counts
for (i in 1:length(names)){
  if (i == 1){
    counts <- nrow(get(names[i]))
  }
  else{
    counts <- counts + nrow(get(names[i]))
  }
}
counts
rm(i)

#Getting the abundances and species richness per time period

abundances_ts <- c()
s_richness_ts <- c()
for (i in 1:length(namesShallowDeep)){
  
  setone <- get(namesShallowDeep[i])
  sp_list_abundances <- setone %>% arrange(scientificName) %>% group_by(scientificName, kingdom, class, family) %>% summarise(abundance = sum(individualCount)) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
  sp_list <- sp_list_abundances[c("scientificName","abundance")] %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(abundance))

  abundances <- sum(sp_list$abundance)
  s_richness <- nrow(sp_list)

  abundances_ts[i] <- abundances
  s_richness_ts[i] <- s_richness
}










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


