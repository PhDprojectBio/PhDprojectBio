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

#taxon filtering with Worms database

#Masking the groups that would not correspond to the marine environment (depending on the scope of the study)

unique(databases_mkd$kingdom)
unique(databases_mkd$class)
unique(databases_mkd$family)
dbs_mkd_taxa <- databases_mkd %>% filter(kingdom != "Plantae")
exc_class = c("Tricholomataceae", "Fringillidae", "Insecta", "Arachnida", "Hexapoda", "Diplopoda", "Pauropoda", "Chilopoda", "Symphyla", "Diplura", "Protura", "Collembola")

rm(i)

for (i in exc_class){
  
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(class != i)
}

#to keep the number
dbs_mkd_taxa_detgroups <- dbs_mkd_taxa

######################

#Read worms information in order to determine which families are non-marine
##### !!! Another filter
taxon <- read.delim("~/R/taxon.txt") %>% filter(taxonomicStatus == "accepted")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
#unique(taxon$taxonRank)

#worms2 <- right_join(taxon[,c(1,6,11:20,30)] %>% filter(taxonRank == "Species" | taxonRank == "Variety" | taxonRank == "Subspecies" | taxonRank == "Subsection" | taxonRank == "Forma" | taxonRank == "Subvariety" | taxonRank == "Subforma"), speciesprofile %>% filter(isMarine == 1 & isExtinct != 1), by = "taxonID")
#speciesprofile %>% filter(isExtinct == 1)
#[1] taxonID       isMarine      isFreshwater  isTerrestrial isExtinct     isBrackish   
#<0 rows> (or 0-length row.names)
worms <- right_join(taxon[,c(1,6,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")
unique(dbs_mkd_taxa$kingdom)
unique(dbs_mkd_taxa$class)
#write.table(unique(dbs_mkd_taxa$class),file = "dbs_mkd_taxa_class.csv", sep = ,, row.names = FALSE, quote = FALSE)
families_db <- unique(dbs_mkd_taxa$family)
families_worms <- unique(worms$family)

#which <- which(log_fam2 == 2)
#log_fam2[log_fam2 == 2]

#n <- 1
#rm(i)

#for(i in which){
  #print(unique(families_worms == families_db[i]))
#}

n <- 1
rm(i)
max_log <- c()

# To see the match that was true (FALSE,NA,TRUE), we choose the maximum logical output
for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}

max(max_log)

#Remove NAs

unique(max_log)
length(families_db)

if(length(which(max_log == 1))>0) {
  families_db <- families_db[-(which(max_log == 1))]
}
length(families_db)

#Repeat without NA

n <- 1
rm(i)
max_log <- c()

for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}

unique(max_log)

vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

#Verification for Fringilla
#which(names(vector2false) =="Fringillidae")
#which(names(vector3true) =="Fringillidae")
#which(families_db =="Fringillidae")
#which(dbs_mkd_taxa$scientificName == "Fringilla montifringilla")
#write.csv(vector3true, file = "marine_worms_toverify.txt")
#vector3true[5]

log_fam <- c()
n <- 1
rm(i)

for(i in families_db){
  
  if (length(unique(families_worms == i)) == max(max_log)){
    log_fam[n] <- TRUE 
    n <- n+1
  }
    
  else {
    log_fam[n] <- FALSE 
    n <- n+1
  }
}

length(log_fam[log_fam == TRUE]) # Number of marine
length(log_fam[log_fam == FALSE]) # Number of non-marine


log_fam_nomar <- log_fam == FALSE
mar_fam <- families_db[log_fam] #compare with all list 
nomar_fam <- families_db[log_fam_nomar] #difference

#write.table(mar_fam_nomar,file = "dbs_mkd_taxa_fam_nomarine.csv", sep = ",", row.names = FALSE, quote = FALSE)

#NAs removal control
nomar_fam <- nomar_fam[-(which(is.na(nomar_fam)))]

##############
##### Run this part and add from new lists as it is time-convenient. These one-two lists is assessed with taxa from 1970to79

#Adding and removing families that are marine and present in non-marine list and
  #families that are non-marine and are present in marine list

#Confirmed marine in worms
one <- read_excel("~/R/one.xlsx", col_names = FALSE)
colnames(one) <- c("species")
one <- one$species
#Question marks, confirmed in artsdatabanken
two <- c("Sistotremataceae", "Naetrocymbaceae", "Mycocaliciaceae", "Dacampiaceae", "Lirellidae")
#Marine that turned out to be no marine after confirmation (or non-valid taxa). For further revision, more time is required :)
plus_nomarine <- c("Esocidae", "Clausiliidae", "Physetocarididae")

#merging the new marine and removing from the nomarine list
plus_marine <- append(one,two)
nomar_fam <- nomar_fam[!nomar_fam %in% plus_marine]

#adding the new non-marine
nomar_fam <- append(nomar_fam,plus_nomarine)

###########
#Run comparing to the current database

rm(i)

for (i in nomar_fam){
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(family != i)
}

#to keep the number
dbs_mkd_taxa_wormsnmanual <- dbs_mkd_taxa

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed before
species_db <- unique(dbs_mkd_taxa$scientificName)
species_worms <- unique(worms$scientificName)

n <- 1
rm(i)
max_log <- c()

# To see the match that was true (FALSE,NA,TRUE), we choose the maximum logical output
for(i in species_db){
  max_log[i] <- length(unique(species_worms == i))
}

max(max_log)

#Remove NAs

unique(max_log)
length(species_db)

if(length(which(max_log == 1))>0) {
  species_db <- species_db[-(which(max_log == 1))]
}
length(species_db)

#Repeat without NA

rm(i)
max_log <- c()

for(i in species_db){
  max_log[i] <- length(unique(species_worms == i))
}

unique(max_log)

vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

#Verification for Fringilla
#which(names(vector2false) =="Fringillidae")
#which(names(vector3true) =="Fringillidae")
#which(families_db =="Fringillidae")
#which(dbs_mkd_taxa$scientificName == "Fringilla montifringilla")
#write.csv(vector3true, file = "marine_worms_toverify.txt")
#vector3true[5]

log_spp <- c()
n <- 1
rm(i)

for(i in species_db){
  
  if (length(unique(species_worms == i)) == max(max_log)){
    log_spp[n] <- TRUE 
    n <- n+1
  }
  
  else {
    log_spp[n] <- FALSE 
    n <- n+1
  }
}

length(log_spp[log_spp == TRUE]) # Number of marine
length(log_spp[log_spp == FALSE]) # Number of non-marine


log_spp_nomar <- log_spp == FALSE
mar_spp <- species_db[log_spp] #compare with all list 
nomar_spp <- species_db[log_spp_nomar] #difference

###
#Run comparing to the current database

rm(i)

for (i in nomar_spp){
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa$class))

###End adding

rm(i,n)

##################

#To obtain the species abundance later, the species count that is NA is taken as 1 and added together afterwards for the same species.

dbs_mkd_taxa$individualCount[is.na(dbs_mkd_taxa$individualCount)] <- 1
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0] <- (dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0])*-1 
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount == 0] <- 1

rm(i,n)
