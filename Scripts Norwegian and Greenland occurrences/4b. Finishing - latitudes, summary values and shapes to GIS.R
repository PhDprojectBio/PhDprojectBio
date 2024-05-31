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

#V. Removing duplicates + splitting in latitudes (if the input is a period of the time-series and the output is no-duplicates per latitudinal band for this period)

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
        
    for (a in 1:nrow(link)){
      t <- df2 %>% filter(scientificName == link$scientificName[a])
      assign(gsub(" ","",paste(test[i],"_sp_dups","_sp",a)),t)
      newtest[a] <- gsub(" ","",paste(test[i],"_sp_dups","_sp",a))
      
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
      
      #to name the set that has no duplicates
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

rm(all)
rm(i,a,e,f,k,h,l,m,p)

#VI. Converting to shapes, an example
  
  shp <- st_as_sf(x = dataset,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","", paste("shp_",name,".shp")))
}
rm(i)



