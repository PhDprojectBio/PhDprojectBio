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

#This script is going to work with the overall of records
#Import all the dbs_mkd_taxa_new records and add a suffix for each one (e.g. dbs_mkd_taxa_new1900_49)

I. 
#This for the keys
load("~/R/1876_99a.RData")
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














#Getting the records of all the species in the corresponding time window

sp_list_abundances <- dbs_mkd_taxa_new %>% arrange(scientificName) %>% group_by(scientificName, kingdom, class, family) %>% summarise(abundance = sum(individualCount)) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
sp_list <- sp_list_abundances[c("scientificName","abundance")] %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(abundance))

abundances <- sum(sp_list$abundance)
s_richness <- nrow(sp_list)

#establishing auxiliary vectors
e <- 1
test2 <- c()

for(i in 56:84){
  
  test2[e] <- gsub(" ","", paste("l",i))
  e = e +1
}

testb <- test[1:20]
testc <- test2[1:5]

rm(e)

#Joining the segments inside each latitude, then the next steps can be built on latitudinal bands

r <- 1
i <- 1
#mn <- 0
#mx <- 500

for (r in testc){
  
  for (j in 1:4) {
    
    if(j == 1){
      quart <- get(gsub(" ","", paste(testb[i],"_sp_nodups_oall")))
      segment <- quart
      i <- i +1
    }
    
    else {
      quart <- get(gsub(" ","", paste(testb[i],"_sp_nodups_oall")))
      segment <- rbind(segment,quart)
      i <- i +1
    }
  }
  
  assign(gsub(" ","",paste(r,"_sp_nodups_oall")),segment)
}  

rm(i,segment,j)

#dividing by depth

mn <- 0
mx <- 500
allsp <- c()

while (mx < 6500){
  
  for (r in 1:length(test2)){
    
    segment <- get(gsub(" ","",paste(test2[r],"_sp_nodups_oall")))
    
    if (mn < 5500){
      ftd <- segment %>% filter(depth >= mn & depth < mx)
      #ftd <- as.data.frame(ftd)
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx)), ftd)
    }
    
    if (mn == 5500){
      ftd <- segment %>% filter(depth >= mn & depth <= mx)
      #ftd <- as.data.frame(ftd)
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx)), ftd)
    }
    
    # Data below 6000m
    
    if (mn == 6000){
      ftd <- segment %>% filter(depth > mn)
      #ftd <- as.data.frame(ftd)
      ftd2 <- ftd %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(individualCount))
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx)), ftd)
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx,"_splist")), ftd2)
    }
    
    #Joining in one table the depth bands for every latitudinal band
    #Building up the convergence table
    
    ftd <- ftd %>% arrange(by = scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(individualCount))
    ftd <- ftd %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
    
    
    if (r == 1){
      prev_conv <- left_join(sp_list, ftd, by = c("scientificName"), suffix = c("_base", gsub(" ","", paste("_",test2[r]))))
    }
    
    if (r > 1){
      prev_conv <- left_join(prev_conv, ftd, by = c("scientificName"), suffix = c(gsub(" ","", paste("_",test2[r-1])), gsub(" ","", paste("_",test2[r]))))
    }
  }
  
  # Transposing the rows and columns, replacing NA for 0s and rounding values down to the nearest integer (as necessary for the following procedures)
  
  x = prev_conv[-2]
  x = x %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
  namerows <- colnames(x)
  namerows <- namerows[-1]
  x <- transpose(x, make.names = 1)
  x <- replace(x, is.na(x), 0)
  
  row <- nrow(x)
  col <- ncol(x)
  namecols <- colnames(x)
  unlisted <- as.numeric(unlist(x))
  unlisted <- floor(unlisted)
  x <- matrix(data = unlisted, nrow = row, ncol = col)
  row.names(x) <- namerows
  colnames(x) <- namecols
  x <- as.data.frame(x)
  allsp <- paste(allsp,namecols)
  
  if (mn < 6000){
    assign(gsub(" ","", paste("convtable_",mn,"_",mx,"_abund_nodups")), x)
  }
  
  if (mn == 6000){
    assign(gsub(" ","", paste("convtable_",mn,"plus_abund_nodups")), x)
  }
  
  mn <- mn + 500
  mx <- mx + 5000 #to finish the loop
}

rm(r,row,col)


############################################################

### convergence with occurrences (only for the big database)


#Getting a table with the records of all the species in the corresponding time window
#occurrence_list <- dbs_mkd_taxa_new %>% arrange(scientificName) %>% select(id, scientificName) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName)) %>% select(id) %>% arrange(id)
#occurrence_list$numberOccurrence <- c(1:nrow(occurrence_list))

occurrence <- nrow(dbs_mkd_taxa_new)
#s_richness <- nrow(sp_list)

#establishing auxiliary vectors
e <- 1
test2 <- c()

for(i in 56:84){
  
  test2[e] <- gsub(" ","", paste("l",i))
  e = e +1
}

testb <- test[1:20]
testc <- test2[1:5]

rm(e)

#Joining the segments inside each latitude, then the next steps can be built on latitudinal bands

r <- 1
i <- 1
#mn <- 0
#mx <- 500

for (r in testc){
  
  for (j in 1:4) {
    
    if(j == 1){
      quart <- get(gsub(" ","", paste(testb[i],"_sp_nodups_oall")))
      segment <- quart
      i <- i +1
    }
    
    else {
      quart <- get(gsub(" ","", paste(testb[i],"_sp_nodups_oall")))
      segment <- rbind(segment,quart)
      i <- i +1
    }
  }
  
  assign(gsub(" ","",paste(r,"_sp_nodups_oall")),segment)
}  

rm(i,segment,j)

###dividing by depth

mn <- 0
mx <- 500
allsp <- c()

while (mx < 6500){
  
  for (r in 1:length(test2)){
    
    segment <- get(gsub(" ","",paste(test2[r],"_sp_nodups_oall")))
    
    if (mn < 5500){
      ftd <- segment %>% filter(depth >= mn & depth < mx)
      #ftd <- as.data.frame(ftd)
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx)), ftd)
    }
    
    if (mn == 5500){
      ftd <- segment %>% filter(depth >= mn & depth <= mx)
      #ftd <- as.data.frame(ftd)
      assign(gsub(" ","",paste(test2[r],"_sp_nodups_oall_",mn,"_",mx)), ftd)
    }
    
    #Joining in one table the depth bands for every latitudinal band
    #Building up the convergence table
    
    ftd <- nrow(ftd)
    
    if (r == 1 & mn == 0){
      prev_conv <- ftd
    }
    
    if (r > 1 | mn != 0){
      prev_conv <- rbind(prev_conv, ftd)
    }
  }
  
  mn <- mn + 500
  mx <- mx + 5000 #to finish the loop
}

#no need to transpose
prev_conv <- as.data.frame(matrix(prev_conv, ncol = 2))
rownames(prev_conv) <- test2
colnames(prev_conv) <- c("depth0_500","depth500plus")
occurrence_lats_depths <- prev_conv
rm(prev_conv)

rm(r,row,col)





################################################################################
#ONLY In case of

#Joining in one table the depth bands for every latitudinal band
#Building up the convergence table

ftd <- ftd %>% arrange(by = id)
#ftd <- ftd %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))


if (r == 1){
  prev_conv <- left_join(occurrence_list, ftd, by = c("id"), suffix = c("_base", gsub(" ","", paste("_",test2[r]))))
}

if (r > 1){
  prev_conv <- left_join(prev_conv, ftd, by = c("id"), suffix = c(gsub(" ","", paste("_",test2[r-1])), gsub(" ","", paste("_",test2[r]))))
}
}

# Transposing the rows and columns, replacing NA for 0s and rounding values down to the nearest integer (as necessary for the following procedures)

x = prev_conv[-2]
#x = x %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
namerows <- colnames(x)
namerows <- namerows[-1]
x <- transpose(x, make.names = 1)
x <- replace(x, is.na(x), 0)

row <- nrow(x)
col <- ncol(x)
namecols <- colnames(x)
unlisted <- as.numeric(unlist(x))
unlisted <- floor(unlisted)
x <- matrix(data = unlisted, nrow = row, ncol = col)
row.names(x) <- namerows
colnames(x) <- namecols
x <- as.data.frame(x)
allsp <- paste(allsp,namecols)

if (mn < 6000){
  assign(gsub(" ","", paste("convtable_",mn,"_",mx,"_occur_nodups")), x)
}

if (mn == 6000){
  assign(gsub(" ","", paste("convtable_",mn,"_plus_occur_nodups")), x)
}
