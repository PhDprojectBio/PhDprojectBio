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

#Check scripts and change back R2 to R

#pending this one to reduce corrections and consolidate with records from databases.Also for the keys
load("~/R/19xx_1900a.RData")

#This for the keys
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

#assign the names for the keeping names objects
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

#Merge from here, then add the b1900 database when consolidated. ONLY IF DOING IT AGAIN, THERE IS AN ENVIRONMENT WITH THE OUTCOME ALREADY :)

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

#splitting in 0-500 and 500plus
#&
#getting the dataframes and splitting them to export as a shapefile.
#Need to put below 2GB to avoid incompatibilities (iterated to know this number) and the limit of 500000 (?) due to memory limitations and the warning of 2GB for shp exports.

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
  
for (i in 1:length(namesShallowDeep)){
  df <- get(namesShallowDeep[i])
  n <- 500000
  nx <- nrow(df)
  splitdf <- split(df, rep(1:ceiling(nx/n), each=n, length.out=nx))
  
  for (a in 1:length(splitdf)){
      
    x <- splitdf[[a]]
    assign(gsub(" ","",paste(namesShallowDeep[i],"_",a)), x)
    
    shp <- st_as_sf(x = x,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
    
    st_write(shp, gsub(" ","",paste(namesShallowDeep[i],"_",a,".shp")), layer_options = "ENCODING=UTF-8", append=FALSE)
  }
}
rm(i,a)

for (i in 1:length(names)){
  if (i == 1){
    counts <- nrow(get(names[i]))
  }
  else{
    counts <- counts + nrow(get(names[i]))
  }
}
counts

rm(i,n,nx,x,shp,df,splitdf)

deeps <- data.frame()
for (i in namesDeep){
  x <- get(i)
  deeps <- rbind(deeps,x) 
}
rm(i,x)

##To do the spatial join in ArcGIS, trying to export in one shape
namesSimple <- c()
for(i in 1:length(namesShallowDeep)){
  namesSimple[i] <- gsub(" ","",paste(namesShallowDeep[i],"_simple"))
  x <- get(namesShallowDeep[i]) %>% select(XCoord,YCoord,depth,id,day,month,year,scientificName,class,family)
  assign(gsub(" ","",paste(namesShallowDeep[i],"_simple")),x)
}
rm(x,i)

for (i in 1:length(namesSimple)){
  df <- get(namesSimple[i])
  shp <- st_as_sf(x = df,                         
                    coords = c("XCoord", "YCoord"),
                    crs = 4326)
    
  st_write(shp, gsub(" ","",paste(namesSimple[i],".shp")), layer_options = "ENCODING=UTF-8", append=FALSE)
  }
rm(i,df,shp)