ambu <- read.csv("C:/Users/Open User/Downloads/nigeria_ambulances.csv")
view(ambu)
foma <- ggplot(data = ambu )
oja <- read.csv("C:/Users/Open User/Downloads/nigeria_markets.csv")
view(oja)
oja_sf <- oja%>%
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326
  )
oja_sf %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = oja_sf$market_settlement_name,
             clusterOptions = markerClusterOptions(),
             popup = ifelse(oja_sf$OBJECTID !=NA,
                            oja_sf$OBJECTID, #Value if True
                            "Not sure of the market's location")) #Value is False
mapview(oja_sf)@map

oja_sf%>%
  leaflet()%>%
  addProviderTiles(providers$MtbMap)
########
oja_sf %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stadia.StamenTonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(
    label = ~market_settlement_name,
    popup = ~ifelse(!is.na(OBJECTID), as.character(OBJECTID), "Not sure of the market's location")
  )

tweet <- read.csv("C:/Users/Open User/Downloads/tweet_sample.csv")
lon_lat <- read.csv("C:/Users/Open User/Downloads/average-latitude-longitude-countries.csv")
view(lon_lat)
view(tweet)
#find common/unique column(s) to merge the two
names(tweet)
names(lon_lat)
#names(lat_lon)
intersect(names(tweet), names(lon_lat))
tweet <- left_join(tweet, lon_lat) %>%
  drop_na(Longitude, Latitude) %>%
  select(-c(ISO.3166.Country.Code))
tweet_sf <- tweet %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )
mapview(tweet_sf)@map
tweet_sf %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stadia.StamenTonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = tweet_sf$country,
             clusterOptions = markerClusterOptions(),
             popup = ifelse(tweet_sf$search_word == "Yes",
                            tweet_sf$search_word, #Value if True
                            "No search word applied"))
view(oja_sf)
