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

#I. Finding duplicates in the database without splitting it first in latitudinal bands (if coming from all the periods from the time-series)

dbs_mkd_taxa_new = dbs_mkd_taxa_newdepths %>% filter(taxonRank == "FORM" | taxonRank == "Species" | taxonRank == "SPECIES" | taxonRank == "SUBSPECIES" | taxonRank == "UNRANKED" | taxonRank == "VARIETY" | is.na(taxonRank))

a <- 1
e <- 1
f <- 1
k <- 1
h <- 1
l <- 0
m <- 1
p <- 1
init <- 55.95
test <- c()

for(i in 1:length(namesSimple)){

  g = get(namesSimple[i])
  assign(gsub(" ","", paste(namesSimple[i],"_sp")),g)
  
  df <- g
  df2 <- g
  k <- k + 1
  mylist <- list()
  link <- data.frame()
  
  while(k > 0){
    x <- print(df %>% filter(duplicated(df$YCoord) & duplicated(df$XCoord) & duplicated(df$depth) & duplicated(df$day) & duplicated(df$month) & duplicated(df$year) & duplicated(df$scientificName) & !duplicated (df$database)))
    df = df[!(duplicated(df$YCoord) & duplicated(df$XCoord) & duplicated(df$depth) & duplicated(df$day) & duplicated(df$month) & duplicated(df$year) & duplicated(df$scientificName) & !duplicated (df$database)),]
    k <- nrow(x)
    mylist[[f]] <- x
    link <- do.call("rbind",mylist)
    f <- (nrow(link)) + 100
    assign(gsub(" ","",paste(namesSimple[i],"_sp_dups")), link)
  }
  
  if (nrow(link) > 0){
    
    newtest <- 1
    v <- data.frame()
    lasttest <- 1
        
    for (a in 1:nrow(link)){
      t <- df2 %>% filter(scientificName == link$scientificName[a])
      assign(gsub(" ","",paste(namesSimple[i],"_sp_dups","_sp",a)),t)
      newtest[a] <- gsub(" ","",paste(namesSimple[i],"_sp_dups","_sp",a))
      
      object <- get(newtest[a])
      h <- h + 1
      newlist <- list()
      newlink <- data.frame()
      
      z <- object %>% distinct(YCoord, XCoord, depth, day, month, year, scientificName, .keep_all = TRUE)
     
      newlink <- anti_join(t, z)
      assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
      assign(gsub(" ","",paste(newtest[a],"_nodups")), z)
      
      if (a == 1){
        all <- anti_join(g, newlink) 
      }
      
      if (a > 1){
        all <- anti_join(get(lasttest[a-1]), newlink)
      }
      
      #naming the set that has no duplicates
      assign(gsub(" ","",paste(namesSimple[i],"_sp_nodups")),all)
      
      #to call all the duplicates that progressively accumulate in one vector and remove them progressively from the total of results for the corresponding latitude (anti_join used above)
      lasttest[a] <- gsub(" ","",paste(namesSimple[i],"_sp_nodups"))
    }
    
    df3 <- all
  }
  
  else {
    df3 <- g
  }

  #to assign a name to the definitive set with no duplicates for all of the segments 
  assign(gsub(" ","",paste(namesSimple[i])),df3)

  if(i == 1){
    dbs_newlength <- nrow(df3)
    spp_level <- nrow(g)
  }

  if(i > 1){
    dbs_newlength <- dbs_newlength + nrow(df3)
    spp_level <- spp_level + nrow(g)
  }
}  

rm(all)
rm(i,a,e,f,k,h,l,m,p)

#II. To only remove the duplicates for only one period

dbs_mkd_taxa_new = dbs_mkd_taxa_newdepths %>% filter(taxonRank == "FORM" | taxonRank == "Species" | taxonRank == "SPECIES" | taxonRank == "SUBSPECIES" | taxonRank == "UNRANKED" | taxonRank == "VARIETY" | is.na(taxonRank))

a <- 1
e <- 1
f <- 1
k <- 1
h <- 1
l <- 0
m <- 1
p <- 1
init <- 55.95
test <- c()

df <- dbs_mkd_taxa_new
df2 <- dbs_mkd_taxa_new
k <- k + 1
mylist <- list()
link <- data.frame()

while(k > 0){
  x <- print(df %>% filter(duplicated(df$YCoord) & duplicated(df$XCoord) & duplicated(df$depth) & duplicated(df$day) & duplicated(df$month) & duplicated(df$year) & duplicated(df$scientificName) & !duplicated (df$database)))
  df = df[!(duplicated(df$YCoord) & duplicated(df$XCoord) & duplicated(df$depth) & duplicated(df$day) & duplicated(df$month) & duplicated(df$year) & duplicated(df$scientificName) & !duplicated (df$database)),]
  k <- nrow(x)
  mylist[[f]] <- x
  link <- do.call("rbind",mylist)
  f <- (nrow(link)) + 100
  assign("period_sp_dups", link)
}

if (nrow(link) > 0){
  
  newtest <- 1
  v <- data.frame()
  lasttest <- 1
   
  for (a in 1:nrow(link)){
    t <- df2 %>% filter(scientificName == link$scientificName[a])
    assign(gsub(" ","",paste("period_sp_dups","_sp",a)),t)
    newtest[a] <- gsub(" ","",paste("period_sp_dups","_sp",a))
    
    object <- get(newtest[a])
    h <- h + 1
    newlist <- list()
    newlink <- data.frame()
    
    z <- object %>% distinct(YCoord, XCoord, depth, day, month, year, scientificName, .keep_all = TRUE)
    
    newlink <- anti_join(t, z)
    assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
    assign(gsub(" ","",paste(newtest[a],"_nodups")), z)
    
    if (a == 1){
      all <- anti_join(g, newlink) 
    }
    
    if (a > 1){
      all <- anti_join(get(lasttest[a-1]), newlink)
    }
    
    #naming the set that has no duplicates
    assign("period_sp_nodups",all)
    
    #to call all the duplicates that progressively accumulate in one vector and remove them progressively from the total of results for the corresponding latitude (anti_join used above)
    lasttest[a] <- "period_sp_nodups"
  }
  
  dbs_mkd_taxa_new <- all
}

rm(all)
rm(i,a,e,f,k,h,l,m,p)

#III. Creating the permanent dataset and depth intervals for before the 1900s.

dbs_mkd_taxa_new1876_99 <- dbs_mkd_taxa_new

#dividing in 0-500 and 500+ (can be run for every period, if changing the input)

dbs_mkd_taxa_new1876_99_shallow <- dbs_mkd_taxa_new1876_99 %>% filter(depth<500)
dbs_mkd_taxa_new1876_99_deep <- dbs_mkd_taxa_new1876_99 %>% filter(depth>=500)

namesShallow <- c("dbs_mkd_taxa_new1876_99_shallow", namesShallow)
namesDeep <- c("dbs_mkd_taxa_new1876_99_deep", namesDeep)
namesShallowDeep <- c(namesShallow,namesDeep)

###End of the script
