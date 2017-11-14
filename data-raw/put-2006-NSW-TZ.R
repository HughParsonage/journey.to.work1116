

GET("https://opendata.transport.nsw.gov.au/node/271/download", write_disk("~/../Downloads/d2006_spatial_TZ_GMA_ESRI/"))
setwd("~/../Downloads/d2006_spatial_TZ_GMA_ESRI/")
TZ_GMA_2006 <- readOGR(".", layer = "Travel_Zones_GMA_2006")
TZ_GMA_2006_latlon <- spTransform(TZ_GMA_2006, CRS("proj=longlat"))

TZ_2006_Centroids_GMA <-
  gCentroid(TZ_GMA_2006_latlon, byid = TRUE)

TZ_2006_centroids_NSW_GMA <-
  cbind(as.data.frame(TZ_2006_Centroids_GMA),
        as.data.frame(TZ_GMA_2006_latlon@data)) %>%
  as.data.table

d2006_JTW <-
  fread("~/../Downloads/d2006JTW_T01_GMA/d2006JTW_T01_GMA.csv") %>%
  .[, .(workers = sum(Freq)), keyby = D_Tz06]

TZ_2006_centroids_NSW_GMA[d2006_JTW, on = "tz06==D_Tz06", nomatch=0L] %>%
  .[order(-workers)] %>%
  ggplot(aes(x = x, y = y, size = workers)) +
  annotate_coastline("Sydney") +
  geom_point(shape = 21, fill = Orange, color = theGrey) +
  theme_void() +
  coord_map()

TZ_2006_centroids_NSW_GMA[d2006_JTW, on = "tz06==D_Tz06", nomatch=0L] %>%
  .[order(-workers)] %>%
  ggplot(aes(x = x, y = y, size = workers)) +
  annotate_coastline("Sydney") +
  geom_density_2d(n = 500, h = c(0.01, 0.01)) +
  theme_void() +
  coord_map()




