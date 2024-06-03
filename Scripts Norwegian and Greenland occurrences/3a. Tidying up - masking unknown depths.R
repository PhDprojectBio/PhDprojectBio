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

#I. Split records that do not have recorded depth from those that have recorded depth 
nodepth_mkd <- dbs_mkd_taxa %>% filter(is.na(depth)) 
depth_mkd <- anti_join(dbs_mkd_taxa, nodepth_mkd) #records with depth (excluding the ones with no depth)

#II. Retrieving depths for records that do not have depth 

mn <- 0
mx <- 500

while (mn < 6000){
  
  x <- st_read(as.character(gsub(" ","",paste("contour",mn,"_",mx,".shp"))))
  
  #For 1876_99 only. Records with no recorded coordinates need to be excluded in this step.
  nodepth_mkd_noNA <- nodepth_mkd %>% filter(!is.na(nodepth_mkd$XCoord) | !is.na(nodepth_mkd$YCoord))
  nodepth_mkd_NA <- nodepth_mkd %>% filter(is.na(nodepth_mkd$XCoord) | is.na(nodepth_mkd$YCoord))
  nodepth_mkd_NA$geometry <- NA 
  
  y <- st_as_sf(x = nodepth_mkd_noNA,      #For all except 1876_99: nodepth_mkd                   
               coords = c("XCoord", "YCoord"),
               crs = 4326)
      
  y_int <- st_intersects(y,x)
  y_log <- lengths(y_int) > 0 
  mkd <- y[y_log, ]
      
  mkd$XCoord <- st_coordinates(mkd$geometry)[,1]
  mkd$YCoord <- st_coordinates(mkd$geometry)[,2]
      
  mkd <- as.data.frame(mkd)
  
  #For 1876_99 only
  mkd <- rbind(mkd, nodepth_mkd_NA)
  names_nodepth_mkd_NA <- names(nodepth_mkd_NA)
  mkd <- mkd[, names_nodepth_mkd_NA]
  
  assign(gsub(" ","",paste("mkd_",mn,"_",mx)), mkd)
  
  if (mn == 0){
    mkd_allcont <- mkd
  }
  
  if (mn > 0){
    mkd_allcont <- rbind(mkd_allcont,mkd)
  }
  
  mn <- mn + 500
  mx <- mx + 500
}

#get the set that probably correspond to the polygon and graphically represent the result to confirm it and join to the corresponding depth (0to500)
polygon <- anti_join(nodepth_mkd,mkd_allcont)

#For 1876_99 only
polygon <- anti_join(nodepth_mkd_noNA,mkd_allcont)

x <- st_read("all_landshape.shp")
y <- st_read("contour0_500.shp")
z <- st_as_sf(x = polygon,                         
              coords = c("XCoord", "YCoord"),
              crs = 4326)
ggplot() +
  geom_sf(data = x) +
  geom_sf(data = y) +
  scale_fill_viridis_c() +
  geom_sf(data = z) 

mkd_0_500 <- rbind(mkd_0_500, polygon)

#Assigning depths accordingly to the contours

rm(x,y,z)

mn <- 0
mx <- 500

while (mn < 6000){
  
  adddepth <- get(gsub(" ","",paste("mkd_",mn,"_",mx)))
  
  if (nrow(adddepth) > 0){
    adddepth$depth <- mn+1
    adddepth$depthAccuracy <- 999999   #To identify records with missing depthAccuracy
    assign(gsub(" ","",paste("mkd_",mn,"_",mx)),adddepth)
  }
  
  #Creating a new nodepth records with new established depths
  
  if (mn == 0){
    nodepth_mkd_new <- adddepth
  }
  
  if (mn > 0){
    nodepth_mkd_new <- rbind(nodepth_mkd_new,adddepth)
  }
  
  mn <- mn + 500
  mx <- mx + 500
}

#III. Joining record with retrieved depths with those that had depths recorded already.

dbs_mkd_taxa_new <- rbind(depth_mkd,nodepth_mkd_new)

###End of the script
