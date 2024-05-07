setwd("//home.ansatt.ntnu.no/lcgarcia/Documents/R")

library(vegan)

#Rarefaction: of species richness (Gardener)

mn <- 0
mx <- 500
test3 <- c()


setx <- c("x","rar_x")

for (r in setx){
  
  mn <- 0
  mx <- 500

  for (i in 1:13){
  
    if(i < 13){
      x <- get(gsub(" ","",paste("convtable_",mn,"_",mx,"_sp_nodups")))
    }
    
    if(i == 13){
      x = convtable_6000_plus_sp_nodups
    }
    
    names(x)
    row.names(x)
    sn <- specnumber(x)
    print(sn)
    #assign(gsub(" ","",paste("specnumber_",mn,"_",mx)), sn)
    print(range(rowSums(x)))
    rowSums(x)
    
    #The lowest sample size is 0, then might be better to take the number of the bottom of the bathypelagic zone (3000-4000 m)
    #!!!#This would change the next analyses, if we remove the >4000 depth layer (see the curve, to see how unequal can be the layers)
    #???#Remove epipelagic? Sampling effort? Is rarefaction enough?
    #???#What if doing rarefaction per depth instead?
    #Or rarefy along depths instead?, taking the latitudes as a sample??? Too widely spaced?, per basin (latitude covering less or more space of this?). The idea is compare latitudes, this would be comparing depths?
    
    #rarefy(x, sample = 0)
    
    #check were range abundances are 0 (no species recorded)
    if(min(range(rowSums(x)))== 0 & max(range(rowSums(x)))==0){
        rarefied_S <- sn
        rar_x <- x
    }
    
    #Here are the tables with species recorded
    if(!(min(range(rowSums(x)))== 0 & max(range(rowSums(x)))==0)){
      rarefied_S <- rarefy(x, sample = 100)
      rar_x <- rrarefy(x, sample = 100)
      rar_x <- as.data.frame(rar_x)
    }
    
    if (i < 13){
      assign(gsub(" ","", paste("convtable_rarefied_",mn,"_",mx)), rar_x)
    }
    
    if(i == 13){
      assign(gsub(" ","", paste("convtable_rarefied_",mn,"_plus")), rar_x)
    }
    
    #rarefy(x, sample = 60675714)
    
    #Rarefaction: standarise a community dataset using rarefaction (to the 'minimum' size of sample)
    #??? Would this work? 
    
    shannon <- diversity(get(r), "shannon")
    simpson <- 1 - diversity(get(r), "simpson")
    evenness <- round(diversity(get(r), index = "shannon") / log(specnumber(get(r))), digits = 2)
      
    if (i == 1){
      container1 <- rarefied_S
      container2 <- shannon
      container3 <- simpson
      container4 <- evenness
    }
      
    if (i > 1){
      container1 <- append(container1, rarefied_S)
      container2 <- append(container2, shannon)
      container3 <- append(container3, simpson)
      container4 <- append(container3, evenness)
    }
    
    if (i < 13){  
      test3[i] <- gsub(" ","",paste("depth_",mn,"_",mx))
    }
    
    if (i == 13){
      test3[i] <- gsub(" ","",paste("depth_",mn,"_plus"))
    }
    
    mn <- mn + 500
    mx <- mx + 500  
  }
  
  #Species richness, Shannon, Simpson and J-evenness tables
  
    a <- 1
    for (a in 1:4){
        
      set <- get(gsub(" ","",paste("container",a)))
        
      matrix <- matrix(set, nrow = 29, ncol = 13)
      row.names(matrix) <- test2
      colnames(matrix) <- test3
      matrix <- as.data.frame(matrix)
      assign(gsub(" ","",paste("index_",a,"_",r)), matrix)
    }
      
}  

rm(i,a,x)