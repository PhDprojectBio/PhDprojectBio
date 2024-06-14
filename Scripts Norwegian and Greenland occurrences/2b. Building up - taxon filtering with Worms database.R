#I. Loading libraries and directory
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

#2b. Taxon filtering with WoRMS database    
rm(b)
length(set24_mkd)
#1:5
#b <- 4
for (b in 11:11){
#I. Masking the groups that would not correspond to the marine environment (depending on the scope of the study)

#Filter to the species level (forGBIF)
dbs_mkd_taxa = get(allgbif24_mkd[b]) %>% filter(taxonRank == "FORM" | taxonRank == "Species" | taxonRank == "SPECIES" | taxonRank == "SUBSPECIES" | taxonRank == "UNRANKED" | taxonRank == "VARIETY" | is.na(taxonRank))
dbs_mkd_taxa$scientificName = str_extract(string = dbs_mkd_taxa$scientificName, pattern = "[a-zA-Z]{1,25}\\s{1}[a-z]{2,25}")

#Then excluding some taxa
#unique(databases_mkd$kingdom)
#unique(databases_mkd$class)
#unique(databases_mkd$family)
dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(kingdom != "Plantae")
exc_class = c("Tricholomataceae", "Fringillidae", "Insecta", "Arachnida", "Hexapoda", "Diplopoda", "Pauropoda", "Chilopoda", "Symphyla", "Diplura", "Protura", "Collembola")

rm(i)

for (i in exc_class){
  
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(class != i)
}

#II. Reading information from WoRMS in order to determine which families are non-marine

taxon <- read.delim("~/R/taxon.txt") %>% filter(taxonomicStatus == "accepted")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
unique(taxon$taxonRank) #verify that the taxonomic level corresponds to species.
worms <- right_join(taxon[,c(1,6:8,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")
unique(dbs_mkd_taxa$kingdom)
unique(dbs_mkd_taxa$class)
families_db <- unique(dbs_mkd_taxa$family)
families_worms <- unique(worms$family)

n <- 1
rm(i)
max_log <- c()

# To see TRUE matches, we choose the maximum logical output, as this contains TRUE results.

for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}
max(max_log)

#Removing NAs

unique(max_log)
length(families_db)

if(length(which(max_log == 1))>0) {
  families_db <- families_db[-(which(max_log == 1))]
}
length(families_db)

#Repeat the previous operation after removing NA records (there was not a match with records that can be contrasted with the information in WoRMS)

n <- 1
rm(i)
max_log <- c()

for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}

unique(max_log)
vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

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
mar_fam <- families_db[log_fam] #subsample the marine families
nomar_fam <- families_db[log_fam_nomar] #subsample the non-marine families

#To check if NAs are removed
nomar_fam <- nomar_fam[-(which(is.na(nomar_fam)))]

#III. Run this part and add from new lists as it is time-convenient. These one-two lists are assessed with taxa from 1970to79

#Adding and removing families that are marine and present in non-marine list and
#families that are non-marine and are present in marine list

#Confirmed marine in worms
one <- read_excel("~/R/one.xlsx", col_names = FALSE)
colnames(one) <- c("species")
one <- one$species
#Question marks, confirmed in artsdatabanken
two <- c("Sistotremataceae", "Naetrocymbaceae", "Mycocaliciaceae", "Dacampiaceae", "Lirellidae")
#Marine that turned out to be no marine after confirmation (or non-valid taxa). It could have further revision.
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

#IV. Removing the species within the families that are non-marine
species_db <- unique(dbs_mkd_taxa$scientificName)
species_worms <- c(unique(worms$scientificName), unique(worms$acceptedNameUsage), unique(worms$parentNameUsage))

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

#Repeat without NAs present 

rm(i)
max_log <- c()

for(i in species_db){
  max_log[i] <- length(unique(species_worms == i))
}

unique(max_log)

vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

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
mar_spp <- species_db[log_spp] #subsample the marine species
nomar_spp <- species_db[log_spp_nomar] #subsample the non-marine species

#V. Run comparing to the current database         

rm(i)

for (i in nomar_spp){
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa$class))
rm(i,n)

################# 

#This part is getting the dataset ready to obtain the species abundance later, the species count that is NA is taken as 1 (one record) and added together afterwards within the same species.
#####PENDING!!!!!!!!!!!!!! FOR THE OTHER DATASETS. This for gbif 

dbs_mkd_taxa$individualCount[is.na(dbs_mkd_taxa$individualCount)] <- 1
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0] <- (dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0])*-1 
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount == 0] <- 1

assign(gsub(" ","",paste(allgbif24[b], "_mkd_worms")),dbs_mkd_taxa)

###End of the script
}
rm(i,n,b)
save.image("~/R/allgbif24_mkd_worms.RData")    





#2b. Taxon filtering with WoRMS database

#I. Masking the groups that would not correspond to the marine environment (depending on the scope of the study)

#Filter to the species level
dbs_mkd_taxa = dbs_mkd_taxa %>% filter(taxonRank == "FORM" | taxonRank == "Species" | taxonRank == "SPECIES" | taxonRank == "SUBSPECIES" | taxonRank == "UNRANKED" | taxonRank == "VARIETY" | is.na(taxonRank))

#Then excluding some taxa
unique(databases_mkd$kingdom)
unique(databases_mkd$class)
unique(databases_mkd$family)
dbs_mkd_taxa <- databases_mkd %>% filter(kingdom != "Plantae")
exc_class = c("Tricholomataceae", "Fringillidae", "Insecta", "Arachnida", "Hexapoda", "Diplopoda", "Pauropoda", "Chilopoda", "Symphyla", "Diplura", "Protura", "Collembola")

rm(i)

for (i in exc_class){
  
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(class != i)
}

#II. Reading information from WoRMS in order to determine which families are non-marine

taxon <- read.delim("~/R/taxon.txt") %>% filter(taxonomicStatus == "accepted")
speciesprofile <- read.delim("~/R/speciesprofile.txt")
unique(taxon$taxonRank) #verify that the taxonomic level corresponds to species.
worms <- right_join(taxon[,c(1,6:8,11:20,30)], speciesprofile %>% filter(isMarine == 1), by = "taxonID")
unique(dbs_mkd_taxa$kingdom)
unique(dbs_mkd_taxa$class)
families_db <- unique(dbs_mkd_taxa$family)
families_worms <- unique(worms$family)

n <- 1
rm(i)
max_log <- c()

# To see TRUE matches, we choose the maximum logical output, as this contains TRUE results.

for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}
max(max_log)

#Removing NAs

unique(max_log)
length(families_db)

if(length(which(max_log == 1))>0) {
  families_db <- families_db[-(which(max_log == 1))]
}
length(families_db)

#Repeat the previous operation after removing NA records (there was not a match with records that can be contrasted with the information in WoRMS)

n <- 1
rm(i)
max_log <- c()

for(i in families_db){
  max_log[i] <- length(unique(families_worms == i))
}

unique(max_log)
vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

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
mar_fam <- families_db[log_fam] #subsample the marine families
nomar_fam <- families_db[log_fam_nomar] #subsample the non-marine families

#To check if NAs are removed
nomar_fam <- nomar_fam[-(which(is.na(nomar_fam)))]

#III. Run this part and add from new lists as it is time-convenient. These one-two lists are assessed with taxa from 1970to79

#Adding and removing families that are marine and present in non-marine list and
  #families that are non-marine and are present in marine list

#Confirmed marine in worms
one <- read_excel("~/R/one.xlsx", col_names = FALSE)
colnames(one) <- c("species")
one <- one$species
#Question marks, confirmed in artsdatabanken
two <- c("Sistotremataceae", "Naetrocymbaceae", "Mycocaliciaceae", "Dacampiaceae", "Lirellidae")
#Marine that turned out to be no marine after confirmation (or non-valid taxa). It could have further revision.
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

#IV. Removing the species within the families that are non-marine
species_db <- unique(dbs_mkd_taxa$scientificName)
species_worms <- c(unique(worms$scientificName), unique(worms$acceptedNameUsage), unique(worms$parentNameUsage))

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

#Repeat without NAs present

rm(i)
max_log <- c()

for(i in species_db){
  max_log[i] <- length(unique(species_worms == i))
}

unique(max_log)

vector2false <- which(max_log == 2) #non-marine
vector3true <- which(max_log == 3) #marine

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
mar_spp <- species_db[log_spp] #subsample the marine families
nomar_spp <- species_db[log_spp_nomar] #subsample the non-marine families

#V. Run comparing to the current database

rm(i)

for (i in nomar_spp){
  dbs_mkd_taxa <- dbs_mkd_taxa %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa$class))
rm(i,n)

##################

#This part is getting the dataset ready to obtain the species abundance later, the species count that is NA is taken as 1 (one record) and added together afterwards within the same species.

dbs_mkd_taxa$individualCount[is.na(dbs_mkd_taxa$individualCount)] <- 1
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0] <- (dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount < 0])*-1 
dbs_mkd_taxa$individualCount[dbs_mkd_taxa$individualCount == 0] <- 1

rm(i,n)
###End of the script
