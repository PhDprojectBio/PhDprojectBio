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
library(vegan)

#For retrieving the GBIF keys
load("~/R/1900_49a.RData")
load("~/R/1950_59a.RData")
load("~/R/1960_69a.RData")
load("~/R/1970_79a.RData")
load("~/R/1980_89a.RData")
load("~/R/1990_99a.RData")
load("~/R/2000_04a.RData")
load("~/R/2005_09a.RData")
load("~/R/2010_14a.RData")
load("~/R/2015_19a.RData")
load("~/R/2020_23a.RData")

#I. Unifying the time-series in one session of R

#This script is going to work with the overall of records
#Import all the dbs_mkd_taxa_new records and add a suffix for each one (e.g. dbs_mkd_taxa_new1900_49)

#Assigning the names for the keeping objects per period (e.g. dbs_mkd_taxa_new1900_49)
names <- c()
years <- c()

for(i in 1:11){
  if (i == 1){
    a <- 1890+(10*i)
    b <- 39+(10*i)
    names[i] <- gsub(" ","",paste("dbs_mkd_taxa_new",a,"_",b))
  }
  if (i > 1 & i < 7){
    a <- 1930+(10*i)
    b <- 39+(10*i)
    names[i] <- gsub(" ","",paste("dbs_mkd_taxa_new",a,"_",b))
  }
  if(i > 6 & i < 9){
    a <- 1965+(5*i)
    b <- 69+(5*i)-100
    names[i] <- gsub(" ","",paste("dbs_mkd_taxa_new",a,"_0",b))
  }
  if (i > 8 & i < 11){
    a <- 1965+(5*i)
    b <- 69+(5*i)-100
    names[i] <- gsub(" ","",paste("dbs_mkd_taxa_new",a,"_",b))
  }
  if(i == 11){
    a <- 1965+(5*i)
    b <- 68+(5*i)-100
    names[i] <- gsub(" ","",paste("dbs_mkd_taxa_new",a,"_",b))
  }

  if (i < 7 | i > 8){
    years[i] <- gsub(" ","",paste(a,"_",b))
  }
  
  if (i > 6 & i < 9){
    years[i] <- gsub(" ","",paste(a,"_0",b))
  }
}

rm(i)
names
years

#Build the objects per period calling the names

for(i in 1:11){

  if(i == 1){
    load(gsub(" ","",paste("~/R/",years[i],"c.RData")))
    rm(list=ls()[! ls() %in% c("dbs_mkd_taxa_new")])
    assign(names[i], dbs_mkd_taxa_new)
  }
  
  if(i == 2){ 
    load(gsub(" ","",paste("~/R/",years[i],"c.RData")))
    rm(list=ls()[! ls() %in% c(names[i-1],"dbs_mkd_taxa_new")])
    assign(names[i], dbs_mkd_taxa_new)
  }
  
  if(i > 2){ 
    load(gsub(" ","",paste("~/R/",years[i],"c.RData")))
    rm(list=ls()[! ls() %in% c(names[1:i-1],"dbs_mkd_taxa_new")])
    assign(names[i], dbs_mkd_taxa_new)
    rm(dbs_mkd_taxa_new)
  }
}

rm(i,b)

#Add the period 1876-1899 to the names vector and the R environment:

load("~/R/1876_99a.RData")
dbs_mkd_taxa_new1876_99 <- dbs_mkd_taxa_new
names <- c("dbs_mkd_taxa_new1876_99",names)

#II. Getting the two depth intervals (0-500m and 500 and below)

namesShallow <- c()
namesDeep <- c()

for (i in 1:length(names)){

  df <- get(names[i])
  namesShallow[i] <- gsub(" ","",paste(names[i],"_shallow"))
  namesDeep[i] <- gsub(" ","",paste(names[i],"_deep"))
  shallow <- df %>% filter(depth<500)
  assign(gsub(" ","",paste(names[i],"_shallow")),shallow)
  deep <- df %>% filter(depth>=500)
  assign(gsub(" ","",paste(names[i],"_deep")),deep)
  namesShallowDeep <- c(namesShallow, namesDeep)
} 
rm(i,shallow,deep)

#III. Last masking correction

library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")

#Importing world landshape from the contents of https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#There are records that register depths in a different contour than they are supposed to be. Here we mask by contour first, tehn they can be plotted (applying the masking correction that comes in the block below first.
#This is based on the content of the script 3a.

rm(i,x,a)
for (i in 1:length(namesShallowDeep)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    x <- st_read(as.character(gsub(" ","",paste("contour",mn,"_",mx,".shp"))))
    
    if(i == 1 | i == 13){
      dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesShallowDeep[i]) %>% filter(XCoord != 0 | YCoord != 0)
      y <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                    coords = c("XCoord", "YCoord"),
                    crs = 4326)
    }
    
    if((i > 1 & i < 13) | (i > 13)){
      y <- st_as_sf(x = get(namesShallowDeep[i]),                         
                    coords = c("XCoord", "YCoord"),
                    crs = 4326)
    }
    
    y_int <- st_intersects(y,x)
    y_log <- lengths(y_int) > 0 
    mkd <- y[y_log, ]
    
    mkd$XCoord <- st_coordinates(mkd$geometry)[,1]
    mkd$YCoord <- st_coordinates(mkd$geometry)[,2]
    
    mkd <- as.data.frame(mkd)
    
    if(nrow(mkd) > 0){
      mkd$minContour <- mn
      mkd$maxContour <- mx
    }
    
    assign(gsub(" ","",paste(namesShallowDeep[i],"_mkd_",mn,"_",mx)), mkd)
    
    if (mn == 0){
      mkd_allcont <- mkd
    }
    
    if (mn > 0){
      mkd_allcont <- rbind(mkd_allcont,mkd)
    }
    
    mn <- mn + 500
    mx <- mx + 500
    
    rm(a)
  }
  
  #get the set that probably correspond to the polygon and graphically represent the result to confirm it and join to the corresponding depth (0to500)
  if(i == 1 | i == 13){
    polygon <- anti_join(as.data.frame(dbs_mkd_taxa_new1xxx_1899_coordinated),as.data.frame(mkd_allcont))
  }
  
  if((i > 1 & i < 13) | (i > 13)){
    polygon <- anti_join(as.data.frame(get(namesShallowDeep[i])),as.data.frame(mkd_allcont))
  }
  
  #x <- st_read("all_landshape.shp")
  #y <- st_read("contour0_500.shp")
  #z <- st_as_sf(x = polygon,                         
  #             coords = c("XCoord", "YCoord"),
  #            crs = 4326)
  #ggplot() +
  # geom_sf(data = x) +
  # geom_sf(data = y) +
  # scale_fill_viridis_c() +
  # geom_sf(data = z) 
  
  if(nrow(polygon) > 0){
    polygon$minContour <- 0
    polygon$maxContour <- 500
  }
  
  polcontour <- rbind(get(gsub(" ","",paste(namesShallowDeep[i],"_mkd_0_500"))), polygon)
  assign(gsub(" ","",paste(namesShallowDeep[i],"_mkd_0_500")), polcontour)
  
  
  rm(x,y,a, mkd)
  #rm(z)
}

#For masking the landshape outliers with the "world" dataframe, represented as a landshape.
#Ocean provinces are assigned and correspondence with contours is writen in the field matchContours.

rm(i,x,a, mn, mx,setone,settwo,y)
world2 <- st_crop(world, xmin = -27, ymin = 50, xmax = 38, ymax = 85)
for (i in 1:length(namesShallowDeep)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    setone <- get(gsub(" ","",paste(namesShallowDeep[i],"_mkd_",mn,"_",mx)))

    #last correction: masking shallow with "world"
    x <- world2
    
    y <- st_as_sf(x = setone,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
    
    y_int <- st_intersects(y,x)
    y_log <- lengths(y_int) == 0 
    setone_mkd <- y[y_log, ]
    
    setone_mkd$XCoord <- st_coordinates(setone_mkd$geometry)[,1]
    setone_mkd$YCoord <- st_coordinates(setone_mkd$geometry)[,2]
    
    setone <- as.data.frame(setone_mkd)
    
    #Assigning new fields
    
    if(nrow(setone) > 0){
      setone$matchContour <- "pendmatch" #to see if there is any without classification afterwards
      
      if (i < 13){
        
        #setting the oceanic province
        setone$oProvince <- "epipelagic and mesopelagic"
        
        if (mn == 0){
          setone$matchContour <- "match"
          
          for(a in 1:nrow(setone)){
            if(setone$depth[a] >= 500){
              setone$matchContour[a] <- "diffmatch"
            }
          }
        }
        
        #merging into one datatable with the new field contours
        
        if (mn > 0){
          setone$matchContour <- "probPlanktonShallowset"
        }
      }
      
      if(i > 12){
      
        #setting the oceanic province
        if(mn == 0){setone$oProvince <- "epipelagic and mesopelagic"}
        if(mn == 500){setone$oProvince <- "mesopelagic"}
        if(mn >= 1000 & mn < 4000){setone$oProvince <- "bathypelagic"}
        if(mn >= 4000){setone$oProvince <- "abyssopelagic"}
        
        if (mn == 0){
          setone$matchContour <- "shallowfromDeepset"
        }
        
        #merging into one datatable with the new field contours
        
        if (mn > 0){
          setone$matchContour <- "match"
          
          for(a in 1:nrow(setone)){
            if(setone$depth[a] >= 0 & setone$depth[a] < setone$minContour[a]){
              setone$matchContour[a] <- "probPlanktonDeepset"
            }
            if(setone$depth[a] >= setone$maxContour[a]){
              setone$matchContour[a] <- "diffmatch"
            }
          }
        }
      } 
      #rm(a)
    }
    
    if(mn == 0){
      settwo <- setone
    }
    
    if(mn > 0){
      settwo <- rbind(settwo, setone)
    }
      
    mn <- mn + 500
    mx <- mx + 500
    
  }
  
  assign(gsub(" ","",paste(namesShallowDeep[i], "_diffmatch")),settwo)  
  rm(x,y,a, setone, setwo)
  #rm(z)
}

#To plot, these are the keeping objects and the labels for the years
namesShallowPlot <- gsub("shallow", "shallow_diffmatch", namesShallow)
namesDeepPlot <- gsub("deep", "deep_diffmatch", namesDeep)
namesShallowDeepPlot <- c(namesShallowPlot,namesDeepPlot)
yearsLabels <- c("1876-1899", "1900-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2023")

###End of the script
