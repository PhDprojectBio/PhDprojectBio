#repeat download (change per time slot!)

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

### Specific libraries
library(rgbif)
library(dismo)
library(robis)

#1900_49

load("~/R/a files R/1900_49a.RData")
load("~/R/1900_49c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1900to49)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1900-01-01"), enddate = as.Date("1949-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1900_49d.RData")
rm(list = ls())

#####################
#1950_59

load("~/R/a files R/1950_59a.RData")
load("~/R/1950_59c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1950to59)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1950-01-01"), enddate = as.Date("1959-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1950_59d.RData")
rm(list = ls())

#####################
#1960_69

load("~/R/a files R/1960_69a.RData")
load("~/R/1960_69c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1960to69)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1960-01-01"), enddate = as.Date("1969-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1960_69d.RData")
rm(list = ls())

#####################
#1970_79

load("~/R/a files R/1970_79a.RData")
load("~/R/1970_79c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1970to79)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1970-01-01"), enddate = as.Date("1979-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1970_79d.RData")
rm(list = ls())

#####################
#1980_89

load("~/R/a files R/1980_89a.RData")
load("~/R/1980_89c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1980to89)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1980-01-01"), enddate = as.Date("1989-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1980_89d.RData")
rm(list = ls())

#####################
#1990_99

load("~/R/a files R/1990_99a.RData")
load("~/R/1990_99c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_1990to99)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1990-01-01"), enddate = as.Date("1999-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1990_99d.RData")
rm(list = ls())

#####################
#2000_04

load("~/R/a files R/2000_04a.RData")
load("~/R/2000_04c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_2000to04)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2000-01-01"), enddate = as.Date("2004-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2000_04d.RData")
rm(list = ls())

#####################
#2005_09

load("~/R/a files R/2005_09a.RData")
load("~/R/2005_09c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_2005to09)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2005-01-01"), enddate = as.Date("2009-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2005_09d.RData")
rm(list = ls())

#####################
#2010_14

load("~/R/a files R/2010_14a.RData")
load("~/R/2010_14c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_2010to14)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2010-01-01"), enddate = as.Date("2014-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2010_14d.RData")
rm(list = ls())

#####################
#2015_19

load("~/R/a files R/2015_19a.RData")
load("~/R/2015_19c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_2015to19)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2015-01-01"), enddate = as.Date("2019-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2015_19d.RData")
rm(list = ls())

#####################
#2020_23

load("~/R/a files R/2020_23a.RData")
load("~/R/2020_23c.RData")

#change this one!
a_basis <- occ_download_get(head(gbif_2020to23)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")
a_basis = a_basis[c("basisOfRecord", "gbifID")]

#change this one!!!
b_basis <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2020-01-01"), enddate = as.Date("2023-12-31"), absence = NULL, flags = NULL)
b_basis = b_basis[c("basisOfRecord", "id")]

colnames(a_basis)[2] = "id"
a_basis$database = "gbif"
b_basis$database = "obis"

#now binding both dataframes (OBIS and GBIF)

a_basis = as.data.frame(a_basis)
b_basis = as.data.frame(b_basis)
a_basis$id = as.character(a_basis$id)

### databases_basis containing basis of record for each id
databases_basis = rbind(a_basis,b_basis)
unique(databases_basis$basisOfRecord)

dbs_mkd_taxa_new_nomat <- left_join(dbs_mkd_taxa_new, databases_basis, by = "id")
dbs_mkd_taxa_new_nomat <- filter(dbs_mkd_taxa_new_nomat, basisOfRecord != "MATERIAL_CITATION")
unique(dbs_mkd_taxa_new_nomat$basisOfRecord)

#End of adding...

rm(list=ls()[! ls() %in% c("databases_basis", "dbs_mkd_taxa_new_nomat")])
#rm(databases,databases_mkd)
#Change this one!!!
save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/2020_23d.RData")
rm(list = ls())
