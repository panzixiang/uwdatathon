airbnb = fread('/Users/sunyi/Desktop/Datathon/airbnb_dat.csv')
rental = fread('/Users/sunyi/Desktop/Datathon/rental.csv')
rental$RegionName = as.character(rental$RegionName)


data =  airbnb[airbnb$year==2016,]
data1 = data[data$month==11,]
data1 = data[data$city=='seattle']
data1 = data1[,c(12,22)]
colnames(rental)

data1$zipcode = as.character(as.integer(data1$zipcode))
rev_a = data1 %>%
  group_by(zipcode) %>%
  summarise(Na = mean(month_revenue))
rev_r = rental[,c(1,83)]
colnames(rev_r)[2]='Nr'
rev = left_join(rev_a,rev_r,by = c('zipcode'='RegionName'))
plotdata = NULL
for(i in 1:length(rev$zipcode)){
  if(!is.na(rev$Na[i]) && !is.na(rev$Nr[i])){
    plotdata = rbind(plotdata, rev[i,])
  }
}

plotdata$region = plotdata$zipcode
plotdata$value = plotdata$Na#as.integer(100*(plotdata$Na/plotdata$Nr))
zoom = unique(rev_a$zipcode)
zip_zoom=zoom1

rev_a$region = rev$zipcode
rev_a$value = round(100*as.numeric(rev_a$Na)/2637)

county_map <- zip_choropleth(rev_a,zip_zoom = zoom)
county_map + scale_fill_brewer(name = 'Revenue Ratio (&) in Seattle', palette = 'YlOrRd')

