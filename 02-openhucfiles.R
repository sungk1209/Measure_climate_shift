# *------------------------------------------------------------------
# | PROGRAM NAME: Download PMIP files to calculate SPEI
# | FILE NAME: 1.download_pmip.R
# | DATE: 2023.02.27
# | CREATED BY:  Kay Sung       
# *----------------------------------------------------------------
# | PURPOSE: Download max temp(tasmax), min temp(tasmin), 
# |          Relative humidity(hur), downward shortwave radiation(rsds)
# |          wind speed(ua), latent heat(rsds) from MIROC, MRI
# |         
# |          times scale: monthly
# *------------------------------------------------------------------

require(tidyverse)
require(rgdal)
require(sf)
require(raster)
require(ggsflabel)

select <- dplyr::select

data_path <- "../data/"
output_path <- "../output/"

# The input file geodatabase
fgdb <- paste0(data_path,"WBD_National_GDB/WBD_National_GDB.gdb")

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

### Read only the HUC2 layer
naspa_grid <- readRDS(file = paste0(data_path,"/naspa_grid.rds"))
fc <- sf::st_read(fgdb, layer = "WBDHU2")
#broom::tidy(fc)

# Determine the FC extent, projection, and attribute information
summary(fc)

### Make a custom plotting label 
fc <- fc %>%
  mutate(huc_plotlabel = as.character(paste0(huc2, "_", substr(name, 1, 5))))

pnts <- st_as_sf(naspa_grid, coords = c('lon', 'lat'), crs = st_crs(fc))
pnts <- pnts %>%mutate(index = c(1:dim(pnts)[1]))

#sf_use_s2(FALSE)

for (huc_num in c(1:22)) {

  subregion <- fc %>% filter(huc2 == fc$huc2[huc_num])
  name <- fc$huc_plotlabel[huc_num]
  
  intersection = as.integer(st_intersects(pnts, subregion))
  
  pnts_sf <- pnts %>% mutate(
  intersection = as.integer(st_intersects(geometry, subregion)),
  area = if_else(is.na(intersection), '', name)) %>%
  filter(area == name)
  
  temp <- list(pnts_sf$index)

  if (huc_num == 1){
    region_index <- temp
  }else{
  region_index <- append(region_index,temp)
  }
}
  saveRDS(region_index, file = paste0(output_path,"/region_index.rds"))

  #check if regionalisation works 

basemap <- ggplot(fc, aes()) %>%
  + geom_sf() %>%
  + geom_sf_text(aes(label = huc_plotlabel)) %>%
 #  + geom_sf(data = pnts_sf,aes(geometry= geometry)) %>%
          #  aes(geometry= geometry, fill = area)) %>%
  + coord_sf(xlim = c(-30e5, 30e5), ylim = c(0e5, 40e5), crs = 5070, expand = FALSE) %>%
  + theme_bw()

basemap
ggsave(basemap, file = paste0(output_path,"Huc2_regions.png"))
ggsave(basemap, file = paste0(output_path,"Huc2_regions.svg"))








