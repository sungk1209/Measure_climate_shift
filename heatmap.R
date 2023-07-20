
require(tidyverse)
library(ggplot2)
library(reshape2)
require(lubridate)
require(chronosphere)

select <- dplyr::select

data_path <- "../data/"
output_path <- "../output/"

write_figures_path <- file.path(output_path, "/figure/")
# Assuming your dataset is named "my_data"
# Melt the dataset so that SPI values are in a single column


for (huc_num in c(1, 5:22)){
  
  index <- region_index[huc_num][[1]]
  j <- index[1]
  modeled_df <- readRDS(file = paste0(gam_path,"/",j,"_modeled_ts.rds"))
  modeled_all <- modeled_df %>%
    mutate(loc.num = j)
  
  for (j in index[2:length(index)]) { 
    
    modeled_df <- readRDS(file = paste0(gam_path,"/",j,"_modeled_ts.rds"))
    modeled_df <- modeled_df %>%
      mutate(loc.num = j) 
    
    modeled_all <- bind_rows(modeled_all,modeled_df)
  }
  
  #######################remove redundancy of different model
  
  model_spi <- modeled_all %>%
    group_by(date, scenario, loc.num) %>%
    summarise(mean = mean(modGI), shape = mean(shapeGI), scale = mean(scaleGI),
              est_sd = mean(est_sd)) %>%
    mutate(month = month(date)) %>%
    ungroup()
  
  spi_1850 <- model_spi %>%
    filter(year(date) == 1850) %>%
    mutate(spi0_1850 = qgamma(0.5, shape  = shape, scale = scale)) %>% 
    mutate(month = month(date)) %>%
    select(loc.num, scenario, month, spi0_1850, est_sd)
  
  model_spi <- left_join(model_spi, spi_1850, by = c("month", "scenario","loc.num"))
  
  model_spi <- model_spi %>%
    mutate(dgam= pgamma(spi0_1850, shape = shape, scale = scale)) %>%
    mutate(spi_curr = qnorm(dgam, mean = 0, sd= 1)) 
   
  
  temp_df <- model_spi %>%
    group_by(date,scenario) %>%
    summarise(Ref_spi = median(spi_curr)) %>%
    ungroup()
  temp_df <- temp_df %>%
    mutate(huc = nums[huc_num])
  
  if (huc_num == 1){
    all_region_spi_df <- temp_df}
  else {all_region_spi_df = rbind(all_region_spi_df, temp_df)}
  
rm(modeled_all)
rm(model_spi)
}

saveRDS(all_region_spi_df, file = "all_region_spi_df.rds")
#melted_data <- melt(modeled_df, id = "date")
all_spi_df <- readRDS(paste0(data_path, "all_region_spi_df.rds"))


##OOB out of bounds  **OOB = squash
cool = rev(rainbow(25, start=rgb2hsv(col2rgb('darkslategrey'))[1], end=rgb2hsv(col2rgb('blue'))[1]))
warm = rev(rainbow(25, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('darkgoldenrod2'))[1]))
cols = c((cool),"#FFFFFF", (warm))
mypalette <- colorRampPalette(cols)(51)


plot_level <- (c("18","16","15","14","13","12","11","08","03","17","10",
                 "09","07","04","06","05","02","01"))

tmonth <- 11
spi_df <- all_spi_df %>%
 filter(scenario == "ssp585") %>%
 filter(huc != 19) %>%
 filter(month(date) == tmonth)

## Create a heatmap
p <- ggplot(spi_df %>%filter(year(date) >=1400), aes(x = year(date), y = huc, fill = Ref_spi)) +
  geom_tile() + 
  scale_fill_gradientn(colors = rev(ipccTemp(11)), limits = c(-0.3, 0.3), oob = scales::squish) +
  scale_x_continuous(breaks = seq(1400, 2100, 100), minor_breaks = seq(1450, 2050, 100)) +
  scale_y_discrete(limits = factor(plot_level)) +
  geom_vline(xintercept = c(1850,2020), linetype = "dotted", alpha = 0.5) +
  labs(title = paste0("ReferenceSPI_",tmonth),
       x = "Year",
       y = "HUC-2 Number",
       fill = "Reference\nSPI") +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  theme_bw(base_size = 12) 
 
p

ggsave(p, filename = paste0(write_figures_path,"/heatmap_",tmonth,"_rev3.png"),
       width = 11, height = 9, units = "cm")
ggsave(p, filename = paste0(write_figures_path,"/heatmap_",tmonth,"_rev3.svg"),
       width = 11, height = 9, units = "cm")
 