##############################################################################
### Calculate average precipitation fro 1990-2021 using CRU datasets       ###
###                                                                        ###
###                                                                        ###
##############################################################################
# Load the required libraries
library(ncdf4)
library(ggplot2)
library(extrafont)

require(tidyverse)
require(rgdal)
require(sf)
require(raster)
require(chronosphere)

showPal(pal = "all")
select <- dplyr::select

data_path <- "../data/"
output_path <- "../output/"

################extra font 
 font <- "Arial Unicode MS"

# The input file geodatabase
fgdb <- paste0(data_path,"WBD_National_GDB/WBD_National_GDB.gdb")

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

### Read only the HUC2 layer

fc <- sf::st_read(fgdb, layer = "WBDHU2")

# See if the boundarys are valid
st_is_valid(fc)
# If it is not valid, make it valid or simplify
fc <- st_make_valid(fc)
# Determine the FC extent, projection, and attribute information
summary(fc)

# Load the CRU precipitation data file
data <- nc_open(paste0(data_path,"/cru_ts4.06.1901.2021.pre.dat.nc"))
precip <- ncvar_get(data, "pre", start = c(1, 1, 1), count = c(-1, -1, -1))


lon_cru <- ncvar_get(data, "lon")
lat_cru <- ncvar_get(data, "lat")

begin_date <- ncatt_get(data, "time", attname = "units")$value
begin_date <- substring(begin_date,11,nchar(begin_date))

date_list <- ncvar_get(data, "time") + as.Date(begin_date)
length <- length(lon_cru) * length(lat_cru)

coord_cru <- data.frame(lon = rep(lon_cru, each = 360), lat = rep(lat_cru, times = 720), 
                        index = c(1:length)) 

pnts <- st_as_sf(coord_cru, coords = c('lon', 'lat'), crs = st_crs(fc))

# Perform a spatial join between the HUC-2 shapefile and the precipitation data
huc2_region <- st_join(fc,pnts)
huc2_region <- huc2_region %>%
  select(huc2, name, index)

# Calculate the annual average precipitation for each HUC-2 region

hucs <- sprintf("%02d", 1:22)

for (i in c(1:length(hucs))){
  
  huc2_temp <- huc2_region %>%
    filter(huc2 == hucs[i])
  
  temp <- coord_cru[huc2_temp$index,]
  
  temp <- temp %>%
    mutate(lon_col = match(lon,lon_cru)) %>%
    mutate(lat_col = match(lat, lat_cru))
  
  ave_pr_mat <- matrix(NA, ncol = 2, nrow = dim(temp)[1])
  ave_pr_mat[,1] = temp$index
  
   for ( j in 1:nrow(temp)) {
    lon_col = temp$lon_col[j]
    lat_col = temp$lat_col[j]
    
    precip_val <- precip[lon_col, lat_col, ]
    ave_precip <- mean(precip_val)
    ave_pr_mat[j,2] <- ave_precip
    
  }
   annual_ave <- data.frame(huc2 = hucs[i], precip_annual = mean(ave_pr_mat[,2], na.rm = TRUE))
   
   if ( i == 1) {
     huc2_annual <- annual_ave
   } else{
     huc2_annual <- bind_rows(huc2_annual, annual_ave)
   }
}

names(chronosphere_palettes())

fc <- fc %>% right_join(huc2_annual, by = "huc2")
# Plot the average annual precipitation values for each HUC-2 region on a map of the US
  ggplot() +
    geom_sf(data = huc2_annual_avg, aes(fill = annual_avg_precip)) +
    scale_fill_gradient(low = "white", high = "blue") +
    theme_void() +
    labs(title = "Average Annual Precipitation by HUC-2 Region")
  
  col_pal <- (wet(7))
  col_pal <- c("#DBF1D6","#699F8A", "#419EC8", "#145B9C",
   "#0D3F7B", "#0A3263","#061F3B")
  
  basemap <- ggplot(fc) %>%
    + geom_sf(aes(fill = precip_annual), alpha = 0.9) %>%
    + geom_sf_text(aes(label = huc2)) %>%
    + labs(fill = "Annual\nPrecipitation\n (mm)", 
           title = element_text(hjust =0.5), 
           x = "Longitude", y = "Latitude") %>%
    + scale_fill_gradientn(colors = col_pal) %>%
    + coord_sf(xlim = c(-130, -60), ylim = c(20, 55), expand = FALSE) %>%
    + theme_bw(base_size = 12) %>%
    + theme(text = element_text(family = font))
  basemap
  ggsave(basemap, file = paste0(output_path,"Huc2_prcp.png"))
  #ggsave(basemap, file = paste0(output_path,"Huc2_regions.svg"))
