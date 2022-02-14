#libraries
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(hrbrthemes)
library(ggsn)

#also load in the WM outline
auth_codes <- c("E08000031", "E08000025", "E08000028", "E08000026", "E08000027", "E08000029", "E08000030")

#let's use this to download the shapefiles of these areas
wm_outline <- st_read("/home/ucfnacl/DATA/wm_social_atlas/social_atlas_analysis/shape_data/Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp", crs=27700)

wm_outline <- wm_outline[wm_outline$lad17cd %in% auth_codes,]

wm_la_outline <- wm_outline

#merge the las together
wm_outline <- st_union(wm_outline)

#this bit is just for getting the bounding box for  a buffered version
#plot(wm_outline)

#st_crs(wm_outline)

#make a buffer around it of 10k
wm_outline_10kbuffer <- st_buffer(wm_outline, 10000)

plot(wm_outline_10kbuffer)


  tm_shape(wm_outline_10kbuffer) +
  tm_polygons() +
    tm_shape(wm_outline) +
    tm_polygons()

#convert to wgs
wm_outline_10kbuffer_wgs <- st_transform(wm_outline_10kbuffer, crs=4326)

#plot(wm_outline_10kbuffer_wgs)

st_bbox(wm_outline_10kbuffer_wgs)

#cropped shapefiles
cropped_files_folder <- "/home/ucfnacl/DATA/wm_social_atlas/social_atlas_analysis/os_data/cropped_wm_os_data"
all_cropped_names <- list.files(cropped_files_folder)


#what's in there?
print(all_cropped_names)

#open green space
greenspace_shp <- st_read(paste0(cropped_files_folder, "/wm_crop_open_green_space.shp"), crs=27700)

#waterways
watercourse_shp <- st_read(paste0(cropped_files_folder, "/wm_crop_open_water_courses.shp"), crs=27700)

#roads
roads_shp <- st_read(paste0(cropped_files_folder, "/wm_cropped_combined_open_roads.shp"), crs=27700)

#good water colour
#water_colour <- "#D6F1FF"
#canal_colour <- "#1f78b4"

#rail lines
rail_lines_shp <- st_read(paste0(cropped_files_folder, "/wm_rail_lines.shp"), crs=27700)

#rail stations
rail_stations_shp <-  st_read(paste0(cropped_files_folder, "/wm_rail_stations.shp"), crs=27700)

#bus stops
bus_stops_shp <- st_read(paste0(cropped_files_folder, "/wm_naptan_stops_all.shp"), crs=27700)


#start plotting
#ggplot() +
#  geom_sf(data=greenspace_shp, col="green", fill="green") +
#  geom_sf(data=watercourse_shp, col=water_colour, fill=water_colour) +
#  geom_sf(data=watercourse_shp[watercourse_shp$form=="canal",], col=canal_colour) +
#  geom_sf(data=roads_shp[roads_shp$class=="Motorway",], col="black", alpha=0.5) +
#  geom_sf(data=roads_shp[roads_shp$class=="A Road",], col="black", alpha=0.1)

#newer version
#good water colour
water_colour <- "#D6F1FF"
canal_colour <- "#1f78b4"

green_colour <- "#228C22"

wm_basemap <- ggplot() +
  geom_sf(data=greenspace_shp, lwd=0, fill=green_colour, alpha=.1) +
  geom_sf(data=watercourse_shp, col=water_colour, fill=water_colour, alpha=.5) +
  geom_sf(data=watercourse_shp[watercourse_shp$form=="canal",], col=canal_colour) +
  geom_sf(data=roads_shp[roads_shp$class %in% c("Unclassified", "Unknown", "Not Classified", "Classified Unnumbered"),], col="light grey", alpha=0.05, lwd=.2) +
  geom_sf(data=roads_shp[roads_shp$class=="B Road",], col="light grey", lwd=.2) +
  geom_sf(data=roads_shp[roads_shp$class=="A Road",], col="dark grey", lwd=.5, alpha=.7) +
  geom_sf(data=roads_shp[roads_shp$class=="Motorway",], col="black", lwd=.5) +
  theme_ipsum_rc() +
  theme(axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        panel.grid=element_blank())

wm_basemap_extra <- ggplot() +
  geom_sf(data=greenspace_shp, lwd=0, fill=green_colour, alpha=.1) +
  geom_sf(data=watercourse_shp, col=water_colour, fill=water_colour, alpha=.5) +
  geom_sf(data=watercourse_shp[watercourse_shp$form=="canal",], col=canal_colour) +
  geom_sf(data=roads_shp[roads_shp$class %in% c("Unclassified", "Unknown", "Not Classified", "Classified Unnumbered"),], col="light grey", alpha=0.05, lwd=.2) +
  geom_sf(data=roads_shp[roads_shp$class=="B Road",], col="light grey", lwd=.2) +
  geom_sf(data=roads_shp[roads_shp$class=="A Road",], col="dark grey", lwd=.5) +
  geom_sf(data=roads_shp[roads_shp$class=="Motorway",], col="black", lwd=.5, alpha=.7) +
  geom_sf(data=bus_stops_shp[bus_stops_shp$StopTyp!="RLY",], shape=18, col="black", size=.5, alpha=.1 ) +
  geom_sf(data=rail_lines_shp, linetype="dotted", col="black", alpha=.8 ) +
  geom_sf(data=rail_stations_shp, shape=15, col="black", fill=NA, alpha=.6) +
  theme_ipsum_rc() +
  theme(axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        panel.grid=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

#coordinates of scale bar
x_min_val <- st_bbox(wm_outline)$xmax*.7

x_max_val <- st_bbox(wm_outline)$xmax*.9

y_min_val <- st_bbox(wm_outline)$ymax*.8

y_max_val <- st_bbox(wm_outline)$ymax*.9

#another version with the full size of the data done
x_min_val <- st_bbox(wm_outline)$xmin

x_max_val <- st_bbox(wm_outline)$xmax

y_min_val <- st_bbox(wm_outline)$ymin

y_max_val <- st_bbox(wm_outline)$ymax

#make a plot of just the area with the scale bars
base_and_scale <- wm_basemap +
  scalebar(#data=wm_la_boundaries,
    x.min=x_min_val, 
    x.max = x_max_val, 
    y.min = y_min_val, 
    y.max = y_max_val,
    transform = F, 
    dist_unit = "km",
    dist=5,
    location ="bottomleft",
    st.bottom = T
  ) +
  north(x.min=x_min_val, 
        x.max = x_max_val, 
        y.min = y_min_val, 
        y.max = y_max_val,
        location = "topright",
        symbol=12)

base_extra_and_scale <- wm_basemap_extra +
  scalebar(#data=wm_la_boundaries,
    x.min=x_min_val, 
    x.max = x_max_val, 
    y.min = y_min_val, 
    y.max = y_max_val,
    transform = F, 
    dist_unit = "km",
    dist=5,
    location ="bottomleft",
    st.bottom = T
  ) +
  north(x.min=x_min_val, 
        x.max = x_max_val, 
        y.min = y_min_val, 
        y.max = y_max_val,
        location = "topright",
        symbol=12)

#check it works
#base_and_scale
