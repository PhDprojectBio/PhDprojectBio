#charging libraries and directory
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

#Steps to download records from GBIF and OBIS.
#I. For GBIF
#Set Up Your GBIF Username and Password

install.packages("usethis")
usethis::edit_r_environ()

#GBIF_USER="user"
#GBIF_PWD="here..."
#GBIF_EMAIL="here..."

install.packages("rgbif") 
library(rgbif)

#The search is going to be organised by year,
#GBIF allows up to three simultaneous downloads. To check the status of the download and to get the records from it, see step 2.

# Step 1: Setting the request to the server.

#between 1876 and 1899
gbif_1876to99 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_lt("year", 1900),
  format = "SIMPLE_CSV"
)

#between 1900 and 1949
gbif_1900to49 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1900), pred_lte("year", 1949)),
  format = "SIMPLE_CSV"
)

#between 1950 and 1959
gbif_1950to59 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1950), pred_lte("year", 1959)),
  format = "SIMPLE_CSV"
)

#between 1960 and 1969
gbif_1960to69 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1960), pred_lte("year", 1969)),
  format = "SIMPLE_CSV"
)

#between 1970 and 1979
gbif_1970to79 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1970), pred_lte("year", 1979)),
  format = "SIMPLE_CSV"
)

#between 1980 and 1989
gbif_1980to89 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1980), pred_lte("year", 1989)),
  format = "SIMPLE_CSV"
)

#between 1990 and 1999
gbif_1990to99 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 1990), pred_lte("year", 1999)),
  format = "SIMPLE_CSV"
)

####between 2000 and 2004
gbif_2000to04 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 2000), pred_lte("year", 2004)),
  format = "SIMPLE_CSV"
)

#between 2005 and 2009
gbif_2005to09 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 2005), pred_lte("year", 2009)),
  format = "SIMPLE_CSV"
)

#between 2010 and 2014
gbif_2010to14 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 2010), pred_lte("year", 2014)),
  format = "SIMPLE_CSV"
)

#between 2015 and 2019
gbif_2015to19 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 2015), pred_lte("year", 2019)),
  format = "SIMPLE_CSV"
)

#between 2020 and 2023
gbif_2020to23 <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus","PRESENT"), 
  pred_not(pred_in("basisOfRecord",c("FOSSIL_SPECIMEN","LIVING_SPECIMEN"))),
  pred_within("POLYGON((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))"),
  pred_and(pred_gte("year", 2020), pred_lte("year", 2023)),
  format = "SIMPLE_CSV"
)

#IMPORTANT NOTE:
# always cite the doi
# citation example: GBIF Occurrence Download https://doi.org/10.15468/dl.w9cyk3 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 08 August 2023. GBIF.org
# You can get it calling the object that you just downloaded (i.e. gbif_2020to23)
# The dataset produced in R will have a slightly different name to differentiate it from the object that holds the citation text.

#Step 2: Checking the status of the download in the server and retrieving the datasets in R. 

#i.e. > gbif_1876to99 (here are the citation guidelines). To check the status and import the download:
occ_download_wait(head(gbif_1876to99))
gbifa_1876to99 <- occ_download_get(head(gbif_1876to99)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1900to49
occ_download_wait(head(gbif_1900to49))
gbifa_1900to49 <- occ_download_get(head(gbif_1900to49)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1950to59
occ_download_wait(head(gbif_1950to59))
gbifa_1950to59 <- occ_download_get(head(gbif_1950to59)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1960to69 
occ_download_wait(head(gbif_1960to69))
gbifa_1960to69 <- occ_download_get(head(gbif_1960to69)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1970to79 
occ_download_wait(head(gbif_1970to79))
gbifa_1970to79 <- occ_download_get(head(gbif_1970to79)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1980to89 
occ_download_wait(head(gbif_1980to89))
gbifa_1980to89 <- occ_download_get(head(gbif_1980to89)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_1990to99
occ_download_wait(head(gbif_1990to99))
gbifa_1990to99 <- occ_download_get(head(gbif_1990to99)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_2000to04
occ_download_wait(head(gbif_2000to04))
gbifa_2000to04 <- occ_download_get(head(gbif_2000to04)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_2005to09
occ_download_wait(head(gbif_2005to09))
gbifa_2005to09 <- occ_download_get(head(gbif_2005to09)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_2010to14
occ_download_wait(head(gbif_2010to14))
gbifa_2010to14 <- occ_download_get(head(gbif_2010to14)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_2015to19
occ_download_wait(head(gbif_2015to19))
gbifa_2015to19 <- occ_download_get(head(gbif_2015to19)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

#i.e. > gbif_2020to23
occ_download_wait(head(gbif_2020to23))
gbifa_2020to23 <- occ_download_get(head(gbif_2020to23)) %>% occ_download_import(header = TRUE, sep = "\t", na.strings = "NA", fill = TRUE) #,quote = "")

# Step 3: Select the fields that you need to work with.

gbifa_1876to99 = gbifa_1876to99[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_1900to49 = gbifa_1900to49[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_1950to59 = gbifa_1950to59[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]

gbifa_1960to69 = gbifa_1960to69[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_1970to79 = gbifa_1970to79[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_1980to89 = gbifa_1980to89[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]

gbifa_1990to99 = gbifa_1990to99[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_2000to04 = gbifa_2000to04[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_2005to09 = gbifa_2005to09[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]

gbifa_2010to14 = gbifa_2010to14[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_2015to19 = gbifa_2015to19[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]
gbifa_2020to23 = gbifa_2020to23[c("coordinateUncertaintyInMeters", "class", "datasetKey", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "depthAccuracy", "family", "gbifID", "individualCount", "issue", "kingdom", "month", "scientificName", "taxonRank", "year")]


# II. For OBIS, analogue procedure as with GBIF

install.packages("dismo")
library(dismo)
install.packages("robis")
library(robis)

#obis_1876to99
obisb_1876to99 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", enddate = as.Date("1899-12-31"), absence = NULL, flags = NULL)
obisb_1876to99 = obisb_1876to99[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1900to49
obisb_1900to49 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1900-01-01"), enddate = as.Date("1949-12-31"), absence = NULL, flags = NULL)
obisb_1900to49 = obisb_1900to49[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1950to59 
obisb_1950to59 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1950-01-01"), enddate = as.Date("1959-12-31"), absence = NULL, flags = NULL)
obisb_1950to59 = obisb_1950to59[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1960to69
obisb_1960to69 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1960-01-01"), enddate = as.Date("1969-12-31"), absence = NULL, flags = NULL)
obisb_1960to69 = obisb_1960to69[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1970to79
obisb_1970to79 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1970-01-01"), enddate = as.Date("1979-12-31"), absence = NULL, flags = NULL)
obisb_1970to79 = obisb_1970to79[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1980to89
obisb_1980to89 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1980-01-01"), enddate = as.Date("1989-12-31"), absence = NULL, flags = NULL)
obisb_1980to89 = obisb_1980to89[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_1990to99
obisb_1990to99 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("1990-01-01"), enddate = as.Date("1999-12-31"), absence = NULL, flags = NULL)
obisb_1990to99 = obisb_1990to99[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2000to04
obisb_2000to04 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2000-01-01"), enddate = as.Date("2004-12-31"), absence = NULL, flags = NULL)
obisb_2000to04 = obisb_2000to04[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2005to09
obisb_2005to09 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2005-01-01"), enddate = as.Date("2009-12-31"), absence = NULL, flags = NULL)
obisb_2005to09 = obisb_2005to09[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2010to14
obisb_2010to14 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2010-01-01"), enddate = as.Date("2014-12-31"), absence = NULL, flags = NULL)
obisb_2010to14 = obisb_2010to14[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2015to19
obisb_2015to19 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2015-01-01"), enddate = as.Date("2019-12-31"), absence = NULL, flags = NULL)
obisb_2015to19 = obisb_2015to19[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#obis_2020to23
obisb_2020to23 <- occurrence(geometry = "POLYGON ((38.000 85.000, -27.000 85.000, -27.000 56.000, 38.000 56.000, 38.000 85.000))", startdate = as.Date("2020-01-01"), enddate = as.Date("2023-12-31"), absence = NULL, flags = NULL)
obisb_2020to23 = obisb_2020to23[c("coordinateUncertaintyInMeters", "class", "datasetName", "dateIdentified", "day", "decimalLatitude", "decimalLongitude", "depth", "eventID", "family", "id", "individualCount", "flags", "kingdom", "month", "scientificName", "taxonRank", "year")]

#Every time slot can be saves as a separate archive to process with the following scripts.
###End of the script
