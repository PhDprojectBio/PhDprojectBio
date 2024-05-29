setwd("//home.ansatt.ntnu.no/lcgarcia/Documents/R2")

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

#2015_19

#load("~/R/2010_14a.RData")
#load("~/R/2010_14b.RData")
load("~/R/2015_19c.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2015_19c.RData")
rm(list = ls())

#other ready (pass one more time only at the end)
###############################################################

#2005_09

#load("~/R/2005_09a.RData")
load("~/R/2005_09b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################
#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2005_09c.RData")
rm(list = ls())

################################################
#2000_04

#load("~/R/2000_04a.RData")
load("~/R/2000_04b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2000_04c.RData")
rm(list = ls())


################################################
#1990_99

#load("~/R/1990_99a.RData")
load("~/R/1990_99b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1990_99c.RData")
rm(list = ls())


################################################
#1980_89

#load("~/R/1980_89a.RData")
load("~/R/1980_89b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1980_89c.RData")
rm(list = ls())


################################################
#1970_79

#load("~/R/1970_79a.RData")
load("~/R/1970_79b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1970_79c.RData")
rm(list = ls())


################################################
#1960_69

#load("~/R/1960_69a.RData")
load("~/R/1960_69b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1960_69c.RData")
rm(list = ls())


################################################
#1950_59

#load("~/R/1950_59a.RData")
load("~/R/1950_59b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1950_59c.RData")
rm(list = ls())


################################################
#1900_49

#load("~/R/1940_49a.RData")
load("~/R/1900_49b.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1900_49c.RData")
rm(list = ls())


#####before 1900 individually and after corrections on the expedition :)


#######Re-checking

#########

#2010_14

#load("~/R/2010_14a.RData")
load("~/R/2010_14b.RData")
#load("~/R/2010_14c.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2010_14c.RData")
rm(list = ls())

##################

#2015_19

#load("~/R/2015_19.RData")
load("~/R/2015_19b.RData")
#load("~/R/2015_19c.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))
length(unique(dbs_mkd_taxa_new$scientificName))
length(unique(dbs_mkd_taxa_new$family))

#write.table(unique(dbs_mkd_taxa_new$classes), file = "classes.csv")
#write.table(unique(dbs_mkd_taxa_new$family), file = "families.csv")
#write.table(dbs_mkd_taxa_new, file = "table.csv")

###End adding

rm(i,n)

##################

#rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2015_19c.RData")
rm(list = ls())

##################

#2020_23

#load("~/R/2020_23a.RData")
load("~/R2/2020_23b.RData")
#load("~/R/2020_23c.RData")

################### #Adding...

#######################
###Removing the species within the families that are non-marine
#!!! dbs_mkd_taxa if following the order of the script, dbs_mkd_taxa_new if the next script has been executed
species_db <- unique(dbs_mkd_taxa_new$scientificName)
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
  dbs_mkd_taxa_new <- dbs_mkd_taxa_new %>% filter(scientificName != i)
}

length(unique(dbs_mkd_taxa_new$class))

###End adding

rm(i,n)

##################

rm(databases,databases_mkd)
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R2/2020_23c.RData")
rm(list = ls())
