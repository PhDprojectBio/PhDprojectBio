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






###End of the script
