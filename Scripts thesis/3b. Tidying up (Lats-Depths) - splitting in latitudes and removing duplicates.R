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

#Correct back to dbs_mkd_taxa_new if necessary, 
#to find duplicates in the database without splitting it first in latitudinal bands (if coming from all the periods from the time-series)

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
last <- data.frame()

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
    #last <- data.frame()
    
    for (a in 1:nrow(link)){
      t <- df2 %>% filter(scientificName == link$scientificName[a])
      assign(gsub(" ","",paste(namesSimple[i],"_sp_dups","_sp",a)),t)
      newtest[a] <- gsub(" ","",paste(namesSimple[i],"_sp_dups","_sp",a))
      
      object <- get(newtest[a])
      h <- h + 1
      #newlist <- list()
      #newlink <- data.frame()
      
      z <- object %>% distinct(YCoord, XCoord, depth, day, month, year, scientificName, .keep_all = TRUE)
      
      #while(h > 0){
      #y <- print(object %>% filter(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)))
      #object = object[!(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)),]
      #h <- nrow(y)
      #newlist[[m]] <- y
      #newlink <- do.call("rbind",newlist)
      #m <- nrow(newlink) + 100
      #assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
      #}
      
      newlink <- anti_join(t, z)
      assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
      #z <- anti_join(t, newlink)
      assign(gsub(" ","",paste(newtest[a],"_nodups")), z)
      
      if (a == 1){
        all <- anti_join(g, newlink) 
      }
      
      if (a > 1){
        all <- anti_join(get(lasttest[a-1]), newlink)
      }
      
      #to name the set that has no duplicates
      #last[a] <- 
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

spp_level
dbs_newlength
rm(all)
rm(i,a,e,f,k,h,l,m,p)

#To only remove the duplicates for only one period:

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
last <- data.frame()

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
  #last <- data.frame()
  
  for (a in 1:nrow(link)){
    t <- df2 %>% filter(scientificName == link$scientificName[a])
    assign(gsub(" ","",paste("period_sp_dups","_sp",a)),t)
    newtest[a] <- gsub(" ","",paste("period_sp_dups","_sp",a))
    
    object <- get(newtest[a])
    h <- h + 1
    #newlist <- list()
    #newlink <- data.frame()
    
    z <- object %>% distinct(YCoord, XCoord, depth, day, month, year, scientificName, .keep_all = TRUE)
    
    #while(h > 0){
    #y <- print(object %>% filter(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)))
    #object = object[!(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)),]
    #h <- nrow(y)
    #newlist[[m]] <- y
    #newlink <- do.call("rbind",newlist)
    #m <- nrow(newlink) + 100
    #assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
    #}
    
    newlink <- anti_join(t, z)
    assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
    #z <- anti_join(t, newlink)
    assign(gsub(" ","",paste(newtest[a],"_nodups")), z)
    
    if (a == 1){
      all <- anti_join(g, newlink) 
    }
    
    if (a > 1){
      all <- anti_join(get(lasttest[a-1]), newlink)
    }
    
    #to name the set that has no duplicates
    #last[a] <- 
    assign("period_sp_nodups",all)
    
    #to call all the duplicates that progressively accumulate in one vector and remove them progressively from the total of results for the corresponding latitude (anti_join used above)
    lasttest[a] <- "period_sp_nodups"
  }
  
  dbs_mkd_taxa_new <- all
}

spp_level
dbs_newlength
rm(all)
rm(i,a,e,f,k,h,l,m,p)

###Creating the permanent dataset and depth intervals for before the 1900s

dbs_mkd_taxa_new1xxx_1899 <- dbs_mkd_taxa_new

#dividing in 0-500 and 500+ (can be performed for every period, changing the input)

dbs_mkd_taxa_new1xxx_1899_shallow <- dbs_mkd_taxa_new1xxx_1899 %>% filter(depth<500)
dbs_mkd_taxa_new1xxx_1899_deep <- dbs_mkd_taxa_new1xxx_1899 %>% filter(depth>=500)

namesShallow <- c("dbs_mkd_taxa_new1xxx_1899_shallow", namesShallow)
namesDeep <- c("dbs_mkd_taxa_new1xxx_1899_deep", namesShallow)


#Removing duplicates + splitting in latitudes (if coming from a one period of the time-series and the output is the no-duplicates per latitudinal band)

# All records (with landshape records masked out before)
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
last <- data.frame()

#splitting by latitudinal bands every degree and filtering out duplicates

for(i in 56:84){
  
  if (i < 61){
    for (j in 1:4) {
      l <- letters[j]
      test[e] <- gsub(" ","", paste("l",i,l))
      e = e +1
    }
  }
  
  else {
    l <- letters[j]
    test[e] <- gsub(" ","", paste("l",i))
    e = e +1
  }
}


i <- 1
mn <- 0
mx <- 500

for (i in 1:length(test)){
  
  mn <- 0
  mx <- 500
  
  if (i < 21) {
    x <- dbs_mkd_taxa_new %>% filter(YCoord>= (init + 0.05) & YCoord< (init + (0.05 + 0.25)))
    assign(test[i], x)
    init <- init + 0.25
  }
  
  if (i > 20 & i < 44) {
    x <- dbs_mkd_taxa_new %>% filter(YCoord>= (init + 0.05) & YCoord< (init + (0.05 + 1.00)))
    assign(test[i], x)
    init <- init + 1.00
  }  
  
  if (i == 44) {
    x <- dbs_mkd_taxa_new %>% filter(YCoord>= (init + 0.05) & YCoord<= (init + (0.05 + 1.00)))
    assign(test[i], x)
    init <- init + 1.00
  }
  
  g = x %>% filter(taxonRank == "FORM" | taxonRank == "Species" | taxonRank == "SPECIES" | taxonRank == "SUBSPECIES" | taxonRank == "UNRANKED" | taxonRank == "VARIETY" | is.na(taxonRank))
  assign(gsub(" ","", paste(test[i],"_sp")),g)
  
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
    assign(gsub(" ","",paste(test[i],"_sp_dups")), link)
  }
  
  if (nrow(link) > 0){
    
    newtest <- 1
    v <- data.frame()
    lasttest <- 1
    #last <- data.frame()
    
    for (a in 1:nrow(link)){
      t <- df2 %>% filter(scientificName == link$scientificName[a])
      assign(gsub(" ","",paste(test[i],"_sp_dups","_sp",a)),t)
      newtest[a] <- gsub(" ","",paste(test[i],"_sp_dups","_sp",a))
      
      object <- get(newtest[a])
      h <- h + 1
      #newlist <- list()
      #newlink <- data.frame()
      
      z <- object %>% distinct(YCoord, XCoord, depth, day, month, year, scientificName, .keep_all = TRUE)
      
      #while(h > 0){
        #y <- print(object %>% filter(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)))
        #object = object[!(duplicated(object$YCoord) & duplicated(object$XCoord) & duplicated(object$depth) & duplicated(object$day) & duplicated(object$month) & duplicated(object$year) & duplicated(object$scientificName) & !duplicated (object$database)),]
        #h <- nrow(y)
        #newlist[[m]] <- y
        #newlink <- do.call("rbind",newlist)
        #m <- nrow(newlink) + 100
        #assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
      #}
      
      newlink <- anti_join(t, z)
      assign(gsub(" ","",paste(newtest[a],"_dups")), newlink)
      #z <- anti_join(t, newlink)
      assign(gsub(" ","",paste(newtest[a],"_nodups")), z)
      
      if (a == 1){
        all <- anti_join(g, newlink) 
      }
      
      if (a > 1){
        all <- anti_join(get(lasttest[a-1]), newlink)
      }
      
      #to name the set that has no duplicates
      #last[a] <- 
      assign(gsub(" ","",paste(test[i],"_sp_nodups")),all)
      
      #to call all the duplicates that progressively accumulate in one vector and remove them progressively from the total of results for the corresponding latitude (anti_join used above)
      lasttest[a] <- gsub(" ","",paste(test[i],"_sp_nodups"))
    }
    
    df3 <- all
  }
  
  else {
    df3 <- g
  }
  
  #to assign a name to the definitive set with no duplicates for all of the latitudes  
  assign(gsub(" ","",paste(test[i],"_sp_nodups_oall")),df3)
  
  if(i == 1){
    dbs_newlength <- nrow(df3)
    spp_level <- nrow(g)
  }
  
  if(i > 1){
    dbs_newlength <- dbs_newlength + nrow(df3)
    spp_level <- spp_level + nrow(g)
  }
}  

#nrow(spp_level_all)
spp_level
dbs_newlength
rm(all)
rm(i,a,e,f,k,h,l,m,p)
