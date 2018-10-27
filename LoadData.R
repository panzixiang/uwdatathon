library(data.table)
library(dplyr)
library(readxl)
library(choroplethr)
library(choroplethrMaps)
library(choroplethrZip)
library(ggplot2)

listing = fread('/Users/sunyi/Desktop/Project/listings.csv')
mapdata = listing[, c(9,18,29)]

# clean zip code
newdata = NULL
for(i in 1:length(mapdata$zipcode)){
  if(nchar(mapdata$zipcode[i])!=5){
    if(substr(mapdata$zipcode[i],6,6)=='.' || substr(mapdata$zipcode[i],6,6)=='-'){
      newdata = rbind(newdata,list(city = mapdata$city[i], price = mapdata$price[i],zipcode = substr(mapdata$zipcode[i],1,5)))
    }else{
      mapdata$zipcode[i] = NA
    }
  }else{
    newdata = rbind(newdata,mapdata[i,])
  }
}

# Remove the dollar sign
a = newdata
colnames(a)[3]='region'
a$region = as.character(a$region)
a$price = gsub('\\$','',a$price)
a$price = as.numeric(a$price)