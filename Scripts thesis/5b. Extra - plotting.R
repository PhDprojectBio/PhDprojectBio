#plotting

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

#plotting for <500m

#xlab = "latitude", xaxt = 'n')
#axis(side=1,at=sites, labels = rev(sites))

#getting ready. Adapted from: https://r-spatial.org/r/2018/10/25/ggplot2-sf.html
#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", 
#"ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## [1] "sf"  
## [1] "data.frame"
#load("~/R/1900_23_df.RData")

#install.packages("ggspatial")
library("ggspatial")

#For shallow

plots <- c()
for (i in 1:length(namesShallow)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- dbs_mkd_taxa_new1xxx_1899 %>% filter(XCoord != 0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesShallow[i]),                         
                 coords = c("XCoord", "YCoord"),
                 crs = 4326)
  }

  plots[i] <- gsub(" ","", paste(namesShallow[i],"_plot"))
  plot_x <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sf, size = 0.1) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
     #                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
      #                     style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85))
  assign(gsub(" ","", paste(namesShallow[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
plots

plot_grid(dbs_mkd_taxa_new1xxx_1899_shallow_plot, dbs_mkd_taxa_new1900_49_shallow_plot, dbs_mkd_taxa_new1950_59_shallow_plot, dbs_mkd_taxa_new1960_69_shallow_plot, dbs_mkd_taxa_new1970_79_shallow_plot,
          dbs_mkd_taxa_new1980_89_shallow_plot, dbs_mkd_taxa_new1990_99_shallow_plot, dbs_mkd_taxa_new2000_04_shallow_plot, dbs_mkd_taxa_new2005_09_shallow_plot,          
          dbs_mkd_taxa_new2010_14_shallow_plot, dbs_mkd_taxa_new2015_19_shallow_plot, dbs_mkd_taxa_new2020_23_shallow_plot, labels = "AUTO")

# for deep

plots <- c()
for (i in 1:length(namesDeep)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- dbs_mkd_taxa_new1xxx_1899 %>% filter(XCoord != 0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesShallow[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plots[i] <- gsub(" ","", paste(namesDeep[i],"_plot"))
  sf <- st_as_sf(x = get(namesDeep[i]),                         
                 coords = c("XCoord", "YCoord"),
                 crs = 4326)
  
  plot_x <- ggplot(data = world) +
    geom_sf() +
    geom_sf(data = sf, size = 0.1) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
    #                      pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #                     style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85))
  assign(gsub(" ","", paste(namesDeep[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
plots

plot_grid(dbs_mkd_taxa_new1xxx_1899_deep_plot, dbs_mkd_taxa_new1900_49_deep_plot, dbs_mkd_taxa_new1950_59_deep_plot, dbs_mkd_taxa_new1960_69_deep_plot, dbs_mkd_taxa_new1970_79_deep_plot,
          dbs_mkd_taxa_new1980_89_deep_plot, dbs_mkd_taxa_new1990_99_deep_plot, dbs_mkd_taxa_new2000_04_deep_plot, dbs_mkd_taxa_new2005_09_deep_plot,          
          dbs_mkd_taxa_new2010_14_deep_plot, dbs_mkd_taxa_new2015_19_deep_plot, dbs_mkd_taxa_new2020_23_deep_plot, labels = "AUTO")



help(geom_sf)
