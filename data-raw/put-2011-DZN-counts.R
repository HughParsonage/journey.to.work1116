library(data.table)
library(hutils)
library(magrittr)
library(ASGS)
library(sp)
library(rgeos)
library(ggplot2)

SA2_DZN_2011 <- fread("~/ABS-Data/2011-Home_SA2-Work_DZN-Count.csv")

DZN_2011_persons <-
  SA2_DZN_2011[, .(persons = sum(Count)), keyby = .(DZN_CODE11 = Work_DZN)]

DZN_2011_data <-
  as.data.table(DZN_2011@data) %>%
  .[, id := .I]

DZN_2011_centroids <- gCentroid(DZN_2011, byid = TRUE)
DZN_2011_centroids_tidy <-
  as.data.frame(DZN_2011_centroids) %>%
  as.data.table %>%
  .[, id := .I] %>%
  .[DZN_2011_data, on = "id"] %>%
  setnames(c("x", "y"), c("lon", "lat")) %>%
  .[]

DZN_2016_persons <-
  fread("data-raw/2016-DZN-persons.csv", skip = 10) %>%
  setnames(1:2, c("DZN_CODE16", "persons")) %>%
  drop_empty_cols %>%
  .[DZN_CODE16 %enotin% c("Total",
                          "POW not applicable",
                          "POW No Fixed Address (OT)",
                          "Migratory - Offshore - Shipping (OT)")]

DZN_2016_data <-
  as.data.table(DZN_2016@data) %>%
  .[, id := .I]

DZN_2016_centroids <- gCentroid(DZN_2016, byid = TRUE)
DZN_2016_centroids_tidy <-
  as.data.frame(DZN_2016_centroids) %>%
  as.data.table %>%
  .[, id := .I] %>%
  .[DZN_2016_data, on = "id"] %>%
  setnames(c("x", "y"), c("lon", "lat")) %>%
  .[]

library(PSMA)
SYD <- geocode(number_first = 1, street_name = "Martin", street_type = "Place", postcode = 2000)
MEL <- geocode(number_first = 350, street_name = "Bourke", street_type = "Street", postcode = 3000)
BNE <- geocode(number_first = 261, street_name = "Queen", street_type = "Street", postcode = 4000)
ADL <- geocode(number_first = 141, street_name = "King William", street_type = "Street", postcode = 5000)
PER <- geocode(number_first = 378, street_name = "Wellington", street_type = "Street", postcode = 6000)

lat_lon_by_City <-
  data.table(City = c("SYD", "MEL", "BNE", "ADL", "PER"),
             GPO_lat = c(SYD$LATITUDE, MEL$LATITUDE, BNE$LATITUDE, ADL$LATITUDE, PER$LATITUDE),
             GPO_lon = c(SYD$LONGITUDE, MEL$LONGITUDE, BNE$LONGITUDE, ADL$LONGITUDE, PER$LONGITUDE))

id_by_city <-
  data.table::CJ(id = DZN_2011_centroids_tidy$id,
                 City = c("SYD", "MEL", "BNE", "ADL", "PER")) %>%
  .[lat_lon_by_City, on = "City"]

distance2GPO_by_DZN_CODE11 <-
  DZN_2011_centroids_tidy %>%
  .[id_by_city, on = "id"] %>%
  .[, distance2GPO := haversine_distance(lat, lon, GPO_lat, GPO_lon)] %>%
  .[, .(distance2GPO = min(distance2GPO),
        City = City[which.min(distance2GPO)]), keyby = c("DZN_CODE11")]

distance2GPO_by_DZN_CODE16 <-
  DZN_2016_centroids_tidy %>%
  .[id_by_city, on = "id"] %>%
  .[, distance2GPO := haversine_distance(lat, lon, GPO_lat, GPO_lon)] %>%
  .[, .(distance2GPO = min(distance2GPO),
        City = City[which.min(distance2GPO)]), keyby = c("DZN_CODE16")]

persons_by_GPO_distance_2011 <-
  distance2GPO_by_DZN_CODE11[DZN_2011_persons, on = "DZN_CODE11", nomatch=0L] %>%
  .[, Year := "2011"]

persons_by_GPO_distance_2016 <-
  distance2GPO_by_DZN_CODE16[DZN_2016_persons, on = "DZN_CODE16", nomatch=0L] %>%
  .[, Year := "2016"]

workers_by_GPO <-
  rbind(persons_by_GPO_distance_2011,
        persons_by_GPO_distance_2016,
        use.names = TRUE, fill = TRUE)

correspondence <-
  readxl::read_excel("data-raw/cg_dzn_2011_dzn_2016.xls",
                     sheet = "Table 3",
                     range = "A6:D9562") %>%
  as.data.table %>%
  .[complete.cases(.)]

workers_by_GPO %>%
  .[distance2GPO < 100] %>%
  .[City != "ADL"] %>%
  .[, weight := persons / sum(persons), keyby =.(City, Year)] %>%
  ggplot(aes(x = distance2GPO, weight = weight, fill = Year)) +
  geom_density(alpha = 0.5, bw = 3) +
  facet_wrap(~City)



# workers_2011_by_rounded_km <-
loessed_2011 <-
  persons_by_GPO_distance_2011 %>%
  .[order(distance2GPO)] %>%
  .[distance2GPO < 40] %>%
  .[, x := floor(distance2GPO)] %>%
  .[, .(y = sum(persons)), keyby = c("City", "x")]

loessed_2016 <-
  persons_by_GPO_distance_2016 %>%
  .[order(distance2GPO)] %>%
  .[distance2GPO < 40] %>%
  .[, x := floor(distance2GPO)] %>%
  .[, .(y = sum(persons)), keyby = c("City", "x")]

merge(loessed_2011, loessed_2016, by = c("City", "x"), suffixes = c("_2011", "_2016")) %>%
  .[, change := y_2016 - y_2011] %>%
  .[City != "ADL"] %>%
  ggplot(aes(x = x, y = change, fill = City)) +
  geom_bar(stat = "identity") +
  facet_wrap(~City)

workers_by_GPO %>%
  .[distance2GPO < 100] %>%
  .[City != "ADL"]


