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

#IV. Removing duplicates + splitting in latitudes (if the input is a period of the time-series and the output is no-duplicates per latitudinal band for this period)
#Reccommended to pass many times to not to leave duplicates (seems that they remain a bit?...)

e <- 1
l <- 0
test <- c()

#splitting by latitudinal bands every degree and filtering out duplicates
rm(i)
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

rm(b,x)
b <- 1
srichness_ts <- c()
abundances_ts <- c()
occurrences_ts <- c()

for (b in 1:length(namesShallowDeepPlot)){
  f <- 1
  k <- 1
  m <- 1
  p <- 1
  init <- 55.95
  
  i <- 1
  mn <- 0
  mx <- 500
  
  for (i in 1:length(test)){
    
    mn <- 0
    mx <- 500
    
    if (i < 21) {
      x <- get(namesShallowDeepPlot[b]) %>% filter(YCoord>= (init + 0.05) & YCoord< (init + (0.05 + 0.25)))
      assign(test[i], x)
      init <- init + 0.25
    }
    
    if (i > 20 & i < 44) {
      x <- get(namesShallowDeepPlot[b]) %>% filter(YCoord>= (init + 0.05) & YCoord< (init + (0.05 + 1.00)))
      assign(test[i], x)
      init <- init + 1.00
    }  
    
    if (i == 44) {
      x <- get(namesShallowDeepPlot[b]) %>% filter(YCoord>= (init + 0.05) & YCoord<= (init + (0.05 + 1.00)))
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
      #df <- anti_join(df, x, by = NULL)
      df = df[!(duplicated(df$YCoord) & duplicated(df$XCoord) & duplicated(df$depth) & duplicated(df$day) & duplicated(df$month) & duplicated(df$year) & duplicated(df$scientificName) & !duplicated (df$database)),]
      k <- nrow(x)
      mylist[[f]] <- x
      link <- do.call("rbind",mylist)
      f <- (nrow(link)) + 100
      assign(gsub(" ","",paste(test[i],"_sp_dups")), link)
    }

    if(i == 1){
      spp_level <- nrow(g)
      nodups <- as.data.frame(df)
    }
    
    if(i > 1){
      spp_level <- spp_level + nrow(g)
      nodups <- rbind(nodups,as.data.frame(df))
    }
    
    if(i == 44){
      #Generate the datataset with no duplicates
      assign(gsub(" ","",paste(namesShallowDeepPlot[b],"_nodups")), nodups)
      
      sp_list_abundances <- nodups %>% arrange(scientificName) %>% group_by(scientificName, kingdom, class, family) %>% summarise(abundance = sum(individualCount)) %>% filter(grepl("[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}", scientificName))
      sp_list <- sp_list_abundances[c("scientificName","abundance")] %>% arrange(scientificName) %>% group_by(scientificName) %>% summarise(abundance = sum(abundance))
      
      #quantification of species richness
      srichness <- nrow(sp_list)
      
      #quantification of abundances
      abundances <- sum(sp_list$abundance)
      
      #quantification of occurrences
      occurrences <- nrow(nodups)
    }
  }  
  
  #Here the generation for srichness, abundances and occurrences per time slot, per depth
  
  srichness_ts[b] <- srichness
  abundances_ts[b] <- abundances
  occurrences_ts[b] <- occurrences
}

rm(all)
rm(a,e,f,k,h,l,m,p,z)
rm(i,b)





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

#V. Converting to shapes, an example
  
  shp <- st_as_sf(x = dataset,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
  
  st_write(shp, gsub(" ","", paste("shp_",name,".shp")))

###End of the script
