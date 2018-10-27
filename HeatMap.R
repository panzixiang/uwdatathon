#Analysis Overall

mappingdata = a %>%
  group_by(region) %>%
  summarise(value = mean(price,na.rm=T))

county_map <- zip_choropleth(mappingdata,county_zoom = 6037)
county_map + scale_fill_brewer(name = "One-night Price in Los Angeles", palette = "Red")


# Analysis regarding City
a = a[a$city=='seattle',]
zoom1 = unique(a$region)
data("zip.regions")

zoom = NULL
for(i in 1:length(zoom1)){
  if(zoom1[i] %in% zip.regions$region){
    zoom = c(zoom,zoom1[i])
  }
}
county_map <- zip_choropleth(mappingdata,zip_zoom = zoom)
county_map + scale_fill_brewer(name = "One-night Price in Seattle", palette = "Red")


