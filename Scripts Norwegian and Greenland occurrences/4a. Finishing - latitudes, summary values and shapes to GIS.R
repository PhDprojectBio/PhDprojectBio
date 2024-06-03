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
#These are the keeping objects for the following steps

namesShallowNodups <- gsub("shallow_diffmatch", "shallow_diffmatch_nodups", namesShallowPlot)
namesDeepNodups <- gsub("deep_diffmatch", "deep_diffmatch_nodups", namesDeepPlot)
namesShallowDeepNodups <- c(namesShallowNodups,namesDeepNodups)

singlecounts <- c()
#Overall counts
for (i in 1:length(namesShallowDeepNodups)){
  if (i == 1){
    counts <- nrow(get(namesShallowDeepNodups[i]))
    singlecount <- counts
  }
  if (i > 1){
    counts <- counts + nrow(get(namesShallowDeepNodups[i]))
    singlecount <- nrow(get(namesShallowDeepNodups[i]))
  }
  
  singlecounts[i] <- singlecount
}
counts
rm(i)

ratiosoverall <- singlecounts/counts

#Getting the species richness, abundances and occurrences per time period

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

#V. Converting to shapes, an example
  
  shp <- st_as_sf(x = dataset,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","", paste("shp_",name,".shp")))

###End of the script
