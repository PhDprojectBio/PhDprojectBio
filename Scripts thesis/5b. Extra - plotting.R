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


#To add to the list of datasets (use once)
namesShallow <- c("dbs_mkd_taxa_new1xxx_1899_shallow",namesShallow)
namesDeep <- c("dbs_mkd_taxa_new1xxx_1899_deep",namesDeep)
namesShallowDeep <- c(namesShallow, namesDeep)

#there are records that register depths in a different contour than they are supposed to be
#contouring to place them in red

rm(i,x,a)
for (i in 1:length(namesShallowDeep)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    x <- st_read(as.character(gsub(" ","",paste("contour",mn,"_",mx,".shp"))))
    
    if(i == 1 | i == 13){
      dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesShallowDeep[i]) %>% filter(XCoord != 0 | YCoord != 0)
      y <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                    coords = c("XCoord", "YCoord"),
                    crs = 4326)
    }
    
    if((i > 1 & i < 13) | (i > 13)){
      y <- st_as_sf(x = get(namesShallowDeep[i]),                         
                    coords = c("XCoord", "YCoord"),
                    crs = 4326)
    }
    
    y_int <- st_intersects(y,x)
    y_log <- lengths(y_int) > 0 
    mkd <- y[y_log, ]
    
    mkd$XCoord <- st_coordinates(mkd$geometry)[,1]
    mkd$YCoord <- st_coordinates(mkd$geometry)[,2]
    
    mkd <- as.data.frame(mkd)
    
    if(nrow(mkd) > 0){
      mkd$minContour <- mn
      mkd$maxContour <- mx
    }
    
    assign(gsub(" ","",paste(namesShallowDeep[i],"_mkd_",mn,"_",mx)), mkd)
    
    if (mn == 0){
      mkd_allcont <- mkd
    }
    
    if (mn > 0){
      mkd_allcont <- rbind(mkd_allcont,mkd)
    }
    
    mn <- mn + 500
    mx <- mx + 500
    
    rm(a)
  }
  
  #get the set that probably correspond to the polygon and graphically represent the result to confirm it and join to the corresponding depth (0to500)
  if(i == 1 | i == 13){
    polygon <- anti_join(as.data.frame(dbs_mkd_taxa_new1xxx_1899_coordinated),as.data.frame(mkd_allcont))
  }
  
  if((i > 1 & i < 13) | (i > 13)){
    polygon <- anti_join(as.data.frame(get(namesShallowDeep[i])),as.data.frame(mkd_allcont))
  }
  
  #x <- st_read("all_landshape.shp")
  #y <- st_read("contour0_500.shp")
  #z <- st_as_sf(x = polygon,                         
  #             coords = c("XCoord", "YCoord"),
  #            crs = 4326)
  #ggplot() +
  # geom_sf(data = x) +
  # geom_sf(data = y) +
  # scale_fill_viridis_c() +
  # geom_sf(data = z) 
  
  if(nrow(polygon) > 0){
    polygon$minContour <- 0
    polygon$maxContour <- 500
  }
  
  polcontour <- rbind(get(gsub(" ","",paste(namesShallowDeep[i],"_mkd_0_500"))), polygon)
  assign(gsub(" ","",paste(namesShallowDeep[i],"_mkd_0_500")), polcontour)
  
  
  rm(x,y,a, mkd)
  #rm(z)
}

#to subset the timeslots in red and black. Red for the points that do not match their depths from de database and the assigned one by the matching bathymetric contour
#masking the land from the polygon that was pending before

rm(i,x,a, mn, mx,setone,settwo,y)
for (i in 1:length(namesShallowDeep)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    setone <- get(gsub(" ","",paste(namesShallowDeep[i],"_mkd_",mn,"_",mx)))

    x <- st_read("all_landshape.shp")
    
    y <- st_as_sf(x = setone,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
    
    y_int <- st_intersects(y,x)
    y_log <- lengths(y_int) == 0 
    setone_mkd <- y[y_log, ]
    
    setone_mkd$XCoord <- st_coordinates(setone_mkd$geometry)[,1]
    setone_mkd$YCoord <- st_coordinates(setone_mkd$geometry)[,2]
    
    setone <- as.data.frame(setone_mkd)
    
    #Assigning new fields
    
    if(nrow(setone) > 0){
      setone$matchContour <- "pendmatch" #to see if there is any without classification afterwards
      
      if (i < 13){
        
        #setting the oceanic province
        setone$oProvince <- "epipelagic and mesopelagic"
        
        if (mn == 0){
          setone$matchContour <- "match"
          
          for(a in 1:nrow(setone)){
            if(setone$depth[a] >= 500){
              setone$matchContour[a] <- "diffmatch"
            }
          }
        }
        
        #merging into one datatable with the new field contours
        
        if (mn > 0){
          setone$matchContour <- "probPlanktonShallowset"
        }
      }
      
      if(i > 12){
      
        #setting the oceanic province
        if(mn == 0){setone$oProvince <- "epipelagic"}
        if(mn == 500){setone$oProvince <- "mesopelagic"}
        if(mn >= 1000 & mn < 4000){setone$oProvince <- "bathypelagic"}
        if(mn >= 4000){setone$oProvince <- "abyssopelagic"}
        
        if (mn == 0){
          setone$matchContour <- "shallowfromDeepset"
        }
        
        #merging into one datatable with the new field contours
        
        if (mn > 0){
          setone$matchContour <- "match"
          
          for(a in 1:nrow(setone)){
            if(setone$depth[a] >= 0 & setone$depth[a] < setone$minContour[a]){
              setone$matchContour[a] <- "probPlanktonDeepset"
            }
            if(setone$depth[a] >= setone$maxContour[a]){
              setone$matchContour[a] <- "diffmatch"
            }
          }
        }
      } 
      #rm(a)
    }
    
    if(mn == 0){
      settwo <- setone
    }
    
    if(mn > 0){
      settwo <- rbind(settwo, setone)
    }
      
    mn <- mn + 500
    mx <- mx + 500
    
  }
  
  assign(gsub(" ","",paste(namesShallowDeep[i], "_diffmatch")),settwo)  
  rm(x,y,a, setone, setwo)
  #rm(z)
}


namesShallowPlot <- gsub("shallow", "shallow_diffmatch", namesShallow)
namesDeepPlot <- gsub("deep", "deep_diffmatch", namesDeep)
namesShallowDeepPlot <- c(namesShallowPlot,namesDeepPlot)

yearsLabels <- c("1876-1899", "1900-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2004", "2005-2009", "2010-2014", "2015-2019", "2020-2023")

#See example of diff matches
#rm(i)
#for (i in 1:length(namesShallowDeep)){
 # print((get(gsub(" ","",paste(namesShallowDeep[23], "_diffmatch"))))[829,])
#}

#last correction: masking shallow with "world"
rm(i,x,a, mn, mx,setone,settwo,y)
world2 <- st_crop(world, xmin = -27, ymin = 50, xmax = 38, ymax = 85)

for (i in 1:length(namesShallowPlot)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    setone <- get(namesShallowPlot[i])
    
    x <- world2
    
    y <- st_as_sf(x = setone,                         
                  coords = c("XCoord", "YCoord"),
                  crs = 4326)
    
    y_int <- st_intersects(y,x)
    y_log <- lengths(y_int) == 0 
    setone_mkd <- y[y_log, ]
    
    setone_mkd$XCoord <- st_coordinates(setone_mkd$geometry)[,1]
    setone_mkd$YCoord <- st_coordinates(setone_mkd$geometry)[,2]
    
    setone <- as.data.frame(setone_mkd)
    
    mn <- mn + 500
    mx <- mx + 500
  }
  
  assign(gsub(" ","",paste(namesShallowPlot[i], "2")),setone)  
  rm(x,y,a, setone, setwo)
  #rm(z)
}
  
namesShallowPlot2 <- gsub("_diffmatch", "_diffmatch2", namesShallowPlot)
################################################## Here finishes the data quality

####PLOTSSS! - Correction

#For shallow
rm(i)
plotsShallow <- c()
for (i in 1:length(namesShallowPlot2)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesShallowPlot2[i]) %>% filter(XCoord != 0 | YCoord !=0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesShallowPlot2[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plotsShallow[i] <- gsub(" ","", paste(namesShallow[i],"_plot2"))
  
  sf <- sf %>% arrange(matchContour)
  
  plot_x <- 
    ggplot(data = world) +
    #ggplot(data = sf) +
    geom_sf() +
    geom_sf(data = sf, aes(color = matchContour), size = 0.001) +
    scale_color_manual(values=c("blue", "turquoise3")) +
    #geom_point(aes(group = matchContour, size = 0.1)) +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
    #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #style = north_arrow_fancy_orienteering) +
    theme(legend.position="none") +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85)) +
    ggtitle(yearsLabels[i])
  assign(gsub(" ","", paste(namesShallow[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
#plots shallow

plot_grid1_2 <- plot_grid(dbs_mkd_taxa_new1xxx_1899_shallow_plot2, dbs_mkd_taxa_new1900_49_shallow_plot2, dbs_mkd_taxa_new1950_59_shallow_plot2, dbs_mkd_taxa_new1960_69_shallow_plot2, dbs_mkd_taxa_new1970_79_shallow_plot2,
                        dbs_mkd_taxa_new1980_89_shallow_plot2, ncol = 2)

#ggsave(plot_grid1, file="plot_grid1.jpeg", units = c("px"), width = 1000, height = 1500)

plot_grid2_2 <- plot_grid(dbs_mkd_taxa_new1990_99_shallow_plot2, dbs_mkd_taxa_new2000_04_shallow_plot2, dbs_mkd_taxa_new2005_09_shallow_plot2, dbs_mkd_taxa_new2010_14_shallow_plot2, dbs_mkd_taxa_new2015_19_shallow_plot2, 
                        dbs_mkd_taxa_new2020_23_shallow_plot2, ncol = 2)

#ggsave(plot_grid2, file="plot_grid2.jpeg", units = c("px"), width = 1000, height = 1500)


##################################

#After: PLOTS

#Plots for intevals
#############
#For shallow
rm(i)
plotsShallow <- c()
for (i in 1:length(namesShallowPlot)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesShallowPlot[i]) %>% filter(XCoord != 0 | YCoord !=0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesShallowPlot[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plotsShallow[i] <- gsub(" ","", paste(namesShallow[i],"_plot"))
  
  sf <- sf %>% arrange(matchContour)
  
  plot_x <- 
    ggplot(data = world) +
    #ggplot(data = sf) +
    geom_sf() +
    geom_sf(data = sf, aes(color = matchContour), size = 0.001) +
    scale_color_manual(values=c("blue", "turquoise3")) +
    #geom_point(aes(group = matchContour, size = 0.1)) +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
              #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
              #style = north_arrow_fancy_orienteering) +
    theme(legend.position="none") +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85)) +
    ggtitle(yearsLabels[i])
  assign(gsub(" ","", paste(namesShallow[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
#plots shallow

plot_grid1 <- plot_grid(dbs_mkd_taxa_new1xxx_1899_shallow_plot, dbs_mkd_taxa_new1900_49_shallow_plot, dbs_mkd_taxa_new1950_59_shallow_plot, dbs_mkd_taxa_new1960_69_shallow_plot, dbs_mkd_taxa_new1970_79_shallow_plot,
          dbs_mkd_taxa_new1980_89_shallow_plot, ncol = 2)

#ggsave(plot_grid1, file="plot_grid1.jpeg", units = c("px"), width = 1000, height = 1500)

plot_grid2 <- plot_grid(dbs_mkd_taxa_new1990_99_shallow_plot, dbs_mkd_taxa_new2000_04_shallow_plot, dbs_mkd_taxa_new2005_09_shallow_plot, dbs_mkd_taxa_new2010_14_shallow_plot, dbs_mkd_taxa_new2015_19_shallow_plot, 
          dbs_mkd_taxa_new2020_23_shallow_plot, ncol = 2)

#ggsave(plot_grid2, file="plot_grid2.jpeg", units = c("px"), width = 1000, height = 1500)

# for deep
rm(i)
plotsDeep <- c()
for (i in 1:length(namesDeepPlot)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesDeepPlot[i]) %>% filter(XCoord != 0 | YCoord !=0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesDeepPlot[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plotsDeep[i] <- gsub(" ","", paste(namesDeep[i],"_plot"))
  
  sf <- sf %>% arrange(matchContour)
    
  plot_x <- 
    ggplot(data = world) +
    #ggplot(data = sf) +
    geom_sf() +
    geom_sf(data = sf, aes(color = matchContour), size = 0.001) +
    scale_color_manual(values=c("gold2", "red", "turquoise3", "black")) +
    #geom_point(aes(group = matchContour, size = 0.1)) +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
                          #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         #style = north_arrow_fancy_orienteering) +
    theme(legend.position="none") +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85)) +
    ggtitle(yearsLabels[i])
  assign(gsub(" ","", paste(namesDeep[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
#plots deep

plot_grid3 <- plot_grid(dbs_mkd_taxa_new1xxx_1899_deep_plot, dbs_mkd_taxa_new1900_49_deep_plot, dbs_mkd_taxa_new1950_59_deep_plot, dbs_mkd_taxa_new1960_69_deep_plot, dbs_mkd_taxa_new1970_79_deep_plot, 
          dbs_mkd_taxa_new1980_89_deep_plot, ncol = 2)

#ggsave(plot_grid3, file="plot_grid3.jpeg", width = 1000, height = 1500)

plot_grid4 <-plot_grid(dbs_mkd_taxa_new1990_99_deep_plot, dbs_mkd_taxa_new2000_04_deep_plot, dbs_mkd_taxa_new2005_09_deep_plot, dbs_mkd_taxa_new2010_14_deep_plot, dbs_mkd_taxa_new2015_19_deep_plot, 
          dbs_mkd_taxa_new2020_23_deep_plot, ncol = 2)

#ggsave(plot_grid4, file="plot_grid4.jpeg", width = 1000, height = 1500)


save.image("//home.ansatt.ntnu.no/lcgarcia/Documents/R/1900_23_dfforPlot.RData")


#Plots for oceanic provinces
#############
#For shallow
rm(i)
plotsShallow <- c()
for (i in 1:length(namesShallowPlot)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesShallowPlot[i]) %>% filter(XCoord != 0 | YCoord !=0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesShallowPlot[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plotsShallow[i] <- gsub(" ","", paste(namesShallow[i],"_plot"))
  
  sf <- sf %>% arrange(matchContour)
  
  plot_x <- 
    ggplot(data = world) +
    #ggplot(data = sf) +
    geom_sf() +
    geom_sf(data = sf, aes(color = oProvince), size = 0.001) +
    #scale_color_manual(values=c("blue", "turquoise3")) +
    #geom_point(aes(group = matchContour, size = 0.1)) +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
    #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #style = north_arrow_fancy_orienteering) +
    #theme(legend.position="none") +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85)) +
    ggtitle(yearsLabels[i])
  assign(gsub(" ","", paste(namesShallow[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
#plots shallow

plot_grid1op <- plot_grid(dbs_mkd_taxa_new1xxx_1899_shallow_plot, dbs_mkd_taxa_new1900_49_shallow_plot, dbs_mkd_taxa_new1950_59_shallow_plot, dbs_mkd_taxa_new1960_69_shallow_plot, dbs_mkd_taxa_new1970_79_shallow_plot,
                        dbs_mkd_taxa_new1980_89_shallow_plot, ncol = 2)

#ggsave(plot_grid1, file="plot_grid1.jpeg", units = c("px"), width = 1000, height = 1500)

plot_grid2op <- plot_grid(dbs_mkd_taxa_new1990_99_shallow_plot, dbs_mkd_taxa_new2000_04_shallow_plot, dbs_mkd_taxa_new2005_09_shallow_plot, dbs_mkd_taxa_new2010_14_shallow_plot, dbs_mkd_taxa_new2015_19_shallow_plot, 
                        dbs_mkd_taxa_new2020_23_shallow_plot, ncol = 2)

#ggsave(plot_grid2, file="plot_grid2.jpeg", units = c("px"), width = 1000, height = 1500)

# for deep
rm(i)
plotsDeep <- c()
for (i in 1:length(namesDeepPlot)){
  
  #to prepare the 1xxx_1899 with only coordinates to plot it
  if(i == 1){
    dbs_mkd_taxa_new1xxx_1899_coordinated <- get(namesDeepPlot[i]) %>% filter(XCoord != 0 | YCoord !=0)
    sf <- st_as_sf(x = dbs_mkd_taxa_new1xxx_1899_coordinated,                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  if(i > 1){
    sf <- st_as_sf(x = get(namesDeepPlot[i]),                         
                   coords = c("XCoord", "YCoord"),
                   crs = 4326)
  }
  
  plotsDeep[i] <- gsub(" ","", paste(namesDeep[i],"_plot"))
  
  sf <- sf %>% arrange(matchContour)
  
  plot_x <- 
    ggplot(data = world) +
    #ggplot(data = sf) +
    geom_sf() +
    geom_sf(data = sf, aes(color = oProvince), size = 0.001) +
    #scale_color_manual(values=c("gold2", "red", "turquoise3", "black")) +
    #geom_point(aes(group = matchContour, size = 0.1)) +
    #annotation_scale(location = "bl", width_hint = 0.5) +
    #annotation_north_arrow(location = "bl", which_north = "true", 
    #pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    #style = north_arrow_fancy_orienteering) +
    #theme(legend.position="none") +
    coord_sf(xlim = c(-27, 38), ylim = c(56, 85)) +
    ggtitle(yearsLabels[i])
  assign(gsub(" ","", paste(namesDeep[i],"_plot")),plot_x)
}
rm(i)

library(cowplot)
#plots deep

plot_grid3op <- plot_grid(dbs_mkd_taxa_new1xxx_1899_deep_plot, dbs_mkd_taxa_new1900_49_deep_plot, dbs_mkd_taxa_new1950_59_deep_plot, dbs_mkd_taxa_new1960_69_deep_plot, dbs_mkd_taxa_new1970_79_deep_plot, 
                        dbs_mkd_taxa_new1980_89_deep_plot, ncol = 2)

#ggsave(plot_grid3, file="plot_grid3.jpeg", width = 1000, height = 1500)

plot_grid4op <-plot_grid(dbs_mkd_taxa_new1990_99_deep_plot, dbs_mkd_taxa_new2000_04_deep_plot, dbs_mkd_taxa_new2005_09_deep_plot, dbs_mkd_taxa_new2010_14_deep_plot, dbs_mkd_taxa_new2015_19_deep_plot, 
                       dbs_mkd_taxa_new2020_23_deep_plot, ncol = 2)

#ggsave(plot_grid4, file="plot_grid4.jpeg", width = 1000, height = 1500)