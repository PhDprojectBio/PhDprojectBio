#Reusable code

############################################################

#######
##Re-usable code into left-joins

rm(i,x,a)
for (i in 1:length(namesShallowDeep)){
  
  mn <- 0
  mx <- 500
  
  while (mn < 6000){
    
    setone <- get(gsub(" ","",paste(namesShallowDeep[i],"_mkd_",mn,"_",mx)))
    
    #merging into one datatable with the new field contours
    
    if(nrow(setone) > 0){
      if(mn == 0){
        settwo <- left_join(x = get(namesShallowDeep[i]), y = setone[ ,c("id", "minContour", "maxContour")], by = "id", all.x=TRUE)
      }
      if(mn > 0){
        settwo <- left_join(x = settwo, y = setone[ ,c("id", "minContour", "maxContour")], by = "id", all.x=TRUE)
      }
    }
    
    mn <- mn + 500
    mx <- mx + 500
    
    rm(a)
  }
  
  assign(gsub(" ","",paste(namesShallowDeep[i], "_cont")),settwo)  
  rm(x,y,a, setone, setwo)
  #rm(z)
}


#


