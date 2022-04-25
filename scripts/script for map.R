###   ###   ###   ###   ###   ###   ###   ###   

###   ###   ### Make a map ###    ###   ###   ###


# Script created by Anita Giraldo - 09 October 2019
# Last modified by Anita Giraldo - 24 April 2022

###
### This script creates a map of Rottnest Island in Western Australia 
### to illustrate kelp tissue collections across different depths

### Map needs:
# 1. Map of Australia
# 2. Map of Rottnest Islands
# 3. collection sites

# Resources --
## https://geocompr.robinlovelace.net/adv-map.html 
## https://ecodiv.earth/post/creating-a-map-with-inset-using-tmap/



# libraries ----
library(sp)
library(sf) 
library(raster)
library(dplyr)
library(rgdal)
library(rgeos)
library(dplyr)
library(tmap)    
library(mapview) 
library(here) # to set working directories
library(grid)
library(shiny)  



# Clear envirooment 
rm(list = ls())


# Set directories ----

w.dir <- here()
d.dir <- here('data') 
s.dir <- here('shapefiles')
r.dir <- here('rasters')
o.dir <- here('outputs')
p.dir <- here('plots')


###

# 1. Read Australia Map ----

aus <- read_sf(paste(s.dir, "Australia_map.shp", sep ='/'))
aus
plot(aus$geometry)
st_crs(aus) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

###

# 2. Get extent of Western Australia ----

# get extent from Australia map --
wa.ext <- raster::drawExtent()
# drawExtent() is useful for manually drawing an extent, could be very useful later
#extent(xmin = 112.2425, xmax = 118.1279 , ymin =-35.4557, ymax = -21.54461)

# crop extent --
wa <- st_crop(aus, wa.ext)
plot(wa)

###

# 3. Read Rottnest Island shapefile ----
ri <- read_sf(paste(s.dir, "Rottnest_Island_map.shp", sep ='/'))
plot(ri$geometry)

###

# 4. Read locations sampling sites ----

# Read csv --
sites <- read.csv(paste(d.dir, "Collection_sites.csv", sep ='/'))
sites 
head(sites)

# make it spatial points --
sp_sites <- st_as_sf(x = sites, 
                  coords = c("longitude", "latitude"),
                  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

# add to current plot to check and make sure st_as_sf() worked correctly 
plot(sp_sites, add=T)


###

# 5. Creat main map ----

# Use colorspace to choose colors --
library(colorspace)
library(RColorBrewer)
library(grDevices)

# choose palette --
pal <- choose_palette()

# choose number of colors --
cols <- pal(4)

# or choose your own colors --
cs <- palette(c("#82CC6C","#007E7D","#255668","#EDEF5C"))


###


#### 5.1 Define extent of Rottnest Island map ----
extent(ri)

ri_region <- st_bbox(c(xmin = 115.4000, xmax = 115.5915,
                       ymin = -32.05514, ymax = -31.97838),
                     crs = st_crs(ri)) %>%
  st_as_sfc()


#### 5.2 create a base map ----
rotto_map <- tm_shape(ri, bbox = ri_region) + tm_grid(n.y = 2, lines = F) + 
  tm_polygons(border.col = "black", lwd = 4) +
  tm_shape(sp_sites) + tm_dots(col = "Depth", size = 0.6, shape = 21, 
                               #tmap_options(max.categories = 4), 
                               palette = cs, legend.show = T) +
  tm_compass(type = "arrow", position = c(0.1, 0.15), size = 2) +
  tm_scale_bar(breaks = c(0,2,5), text.size = 1, position = c(0.08, 0.001), size = 0.7) +
  tm_layout(title = "Rottnest Island", legend.text.size = 1, legend.title.size = 1.2, legend.title.fontface = "bold", 
            title.size = 1, title.fontface = "bold",
            legend.position = c("left", "top"),  title.position = c(0.5,0.65), frame = T) 



rotto_map


###


# 6. Create the inset maps ----

#### 6.1. Define extent of part of WA ----
extent(wa)

wa_region <- st_bbox(c(xmin = 112.9225, xmax = 117.1616,
                       ymin = -35.06435, ymax = -21.31016),
                     crs = st_crs(wa)) %>%
  st_as_sfc()


#### 6.2 WA inset map ----
wa_map <- tm_shape(wa) + tm_polygons(border.col = "black", lwd = 2) +
  tm_shape(ri_region) + # for the red box showing where Rottnest Island is
  tm_borders(col = "red", lwd = 3) +
  tm_layout(bg.color = "transparent",  frame = F,
            title = "WA", title.size = 3, title.color = "black", title.position = c(0.29, 0.70)) 


wa_map


#### 6.3. Australia inset map ----
a_map <- tm_shape(aus) + tm_polygons(col = "black", border.col = "black") +
  tm_shape(wa_region) + # for the red box showing part of Western Australia 
  tm_borders(col = "red",lwd = 3) +
  tm_layout(title = "Australia", title.size = 3,  title.color = "white", title.position = c(0.2,0.59),
            bg.color = "transparent", frame = F)
a_map


###

rotto_map

wa_map

a_map


# 7. combine the three maps  ----
# first arguments specify the centre location (x and y) and size (width and height) of inset map

#### 7.1. check how it looks ----
rotto_map

#print() adds new feature on top of existing map. here we specify where the new feature is placed  
print(wa_map, vp = viewport(x = 0.93, y = 0.43, w = 0.26, h = 0.26))
# create viewport for saving the maps together later on 
vp1 = viewport(x = 0.94, y = 0.44, w = 0.25, h= 0.25)

print(a_map, vp = viewport(x = 0.90, y = 0.63, w = 0.15, h= 0.15))
vp2 = viewport(x = 0.90, y = 0.63, w = 0.15, h= 0.15)



#### 7.2. save the map with insets ----
#tmap_save() is used to save a map output 
tmap_save(rotto_map, filename = paste(p.dir, "Rotto_map_Conner-test1.png", sep ='/'), dpi = 300, 
          height = 5.5, width = 5, units = "in",
          insets_tm = list(wa_map,a_map), 
          # here we use the view ports created earlier 
          insets_vp = list(vp1, vp2))



#### 7.3. Just Rottnest ----

tmap_save(rotto_map, paste(p.dir, "rotto_map.png", sep = '/'))

#map_save(a_map, paste(p.dir, "a_map.png", sep = '/'))

#

#tmap_save(wa_map, paste(p.dir, "wa_map.png", sep = '/'))



