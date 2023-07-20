
require(tidyverse)
require(raster)
require(rgdal)
require(sp)
require(sf)
require(mgcv)
library(ggplot2)
library(ggmap)
require(maps)
require(mapdata)
require(maptools)

#region_bnd <- readOGR(dsn = "../HUC8_CONUS/HUC8_US.shp")
#plot(region_bnd, col="cyan1", border="black", lwd= 3)

data_path <- "../data/"
output_path <- "../output/"

naspa_grid <- readRDS(file = paste0(data_path,"/naspa_grid.rds"))

huc_2 <- readOGR(dsn = paste0(data_path,"/WBD_05_HU2_Shape/Shape/WBDHU2.shp"))
ohio_sf<- read_sf(paste0(data_path,"/WBD_05_HU2_Shape/Shape/WBDHU2.shp"))

class(x = huc_2)

# x= isn't actually needed; it just specifies which object
# view features count
length(huc_2)

# view crs - note - this only works with the raster package loaded
crs(huc_2)

# view extent- note - this only works with the raster package loaded
extent(huc_2)

# view metadata summary
huc_2


## ----shapefile-attributes----------------------------------------------
# just view the attributes & first 6 attribute values of the data
head(huc_2@data)

# how many attributes are in our vector data object?
length(huc_2@data)
plot(huc_2, col="NA", border="black", lwd= 3)


usa <- map_data("usa")
state <- map_data("state")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")

basemap <- ggplot() + geom_polygon(data = usa, 
                                   aes(x=long, y = lat, group = group), 
                                   fill = "white", 
                                   color="black") +
  # geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
  #              fill = "white", color="black") + 
  # geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
  #              fill = "white", color="black") +
  #geom_polygon(data = huc_2, aes(x = long, y = lat, color = ""),fill= NA) + 
  coord_fixed(xlim = c(-133, -60),  ylim = c(15.25, 55), ratio = 1.2)

basemap

broom::tidy(huc_2)

pnts <- st_as_sf(naspa_grid, coords = c('lon', 'lat'), crs = st_crs(ohio_sf))

pnts_sf <- pnts %>% mutate(
  intersection = as.integer(st_intersects(geometry, ohio_sf))
  ,area = if_else(is.na(intersection), '', 'ohio')
) 

pnts_sf <- pnts_sf %>%
  mutate(index = c(1:dim(pnts_sf)[1]))

basemap + geom_sf(data = pnts_sf %>% filter(area == 'ohio'), 
          aes(geometry= geometry))

ohio <- pnts_sf %>%filter(area == 'ohio') 



