#placenames
library(data.table)
library(sf)

place_names <- fread("/home/ucfnacl/DATA/wm_social_atlas/social_atlas_analysis/place_name_data/IPN_GB_2019.csv")

#os_place_names <- fread("https://osdatahub.os.uk/downloads/open/OpenNames?_ga=2.229483992.590280359.1635867096-428097511.1629126741")

#get subset of cities
#head(place_names)

wm_place_names <- place_names[lad18cd %in% auth_codes,]

#head(wm_place_names)

#towns onlly
library(maps)

#get the placename labels
uk_cities <- world.cities[world.cities$country.etc=="UK",]

#head(uk_cities)

#sf that
uk_cities <- st_as_sf(uk_cities, crs=4326, coords = c("long", "lat"))

uk_cities <- st_transform(uk_cities, 27700)

wm_towns <- st_intersection(uk_cities, wm_outline)

#get only ones where the pop is above a certain threshold
#hist(wm_towns$pop)

#head(wm_towns)

wm_towns <- wm_towns[wm_towns$pop>=50000,]

#now get anotther one
#ggplot(wm_towns) +
#  geom_sf()

wm_towns_vec <- wm_towns$name

#print(wm_towns_vec)

wm_towns_vec <- append(wm_towns_vec, values = c("Oldbury", "Smethwick", "Dudley"))

wm_town_names <- wm_place_names[tolower(place18nm) %in% tolower(wm_towns_vec) & splitind==0 & (descnm=="LOC" | descnm=="BUA"),]

#print(wm_town_names$place18nm)

#head(wm_town_names)

keep_cols <- c("place18cd",  "place18nm", "splitind",  "descnm", "gridgb1e", "gridgb1n")

wm_town_names <- wm_town_names[, ..keep_cols]

#wm_town_names_sf <- st_as_sf(wm_town_names, coords = c("gridgb1e", "gridgb1n"), crs=27700)

#ggplot(wm_towns) +
#  geom_sf(col="Red", aes(label="name")) +
#  geom_sf(data = wm_town_names_sf, col="Green")

#tmap_mode("view")

#tm_shape(wm_towns) +
#  tm_dots(col="red") +
#  tm_shape(wm_town_names_sf) +
#  tm_dots(col="green")

#wm_town_names$place18nm

#wm_town_names

#save it as a csv
#fwrite(wm_town_names,"data/wm_town_placenames.csv", row.names = F)


