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

names namesShallowDeepPlot[b],"_nodups"

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
occurrences_ts <- c()
for (i in 1:length(namesShallowDeep)){
  
  setone <- get(namesShallowDeep[i])
  sp_list_abundances <- setone %>% arrange(scientificName) %>% group_by(scientificName, kingdom, class, family) %>% summarise(abundance = sum(individualCount)) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
  sp_list <- sp_list_abundances[c("scientificName","abundance")] %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(abundance))
  
  abundances <- sum(sp_list$abundance)
  s_richness <- nrow(sp_list)
  occurrences <- nrow(setone)
  
  abundances_ts[i] <- abundances
  s_richness_ts[i] <- s_richness
  occurrences_ts[i] <- occurrences
}

##################### Here divide this zones to have the shallow and deeps over the overall!!! :)


#V. Converting to shapes, an example
  
  shp <- st_as_sf(x = dataset,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","", paste("shp_",name,".shp")))
}
rm(i)

###End of the script
