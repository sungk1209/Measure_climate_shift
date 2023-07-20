
require(tidyverse)
require(here)

require(zoo)
require(ggplot2)

require(dplyr)
require(fitdistrplus)

require(mgcv)
##Check location
library(ggplot2)
require(viridis)
require(ggridges)

select <- dplyr::select

data_path <- "../data/"
output_path <- "./output/"
gam_path <- "../output/Gam_result/"
dir.create(output_path, recursive = FALSE)

write_figures_path <- file.path(output_path, "/figure/")
dir.create(write_figures_path, recursive=FALSE, showWarnings = FALSE)

region_index <- readRDS("region_index.rds")

nums <- c("12", "21", "20", "22", "05", "06", "07", "08", "13", "14", 
          "11", "16", "18","15", "09", "17", "04", "19", "10", "03",
          "01","02")

for (huc_num in c(1:22)){
  
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
  
  model_spi <- modeled_all %>%
    group_by(date, scenario, loc.num) %>%
    summarise(mean = mean(modGI), shape = mean(shapeGI), scale = mean(scaleGI),
              est_sd = mean(est_sd)) %>%
    mutate(month = month(date)) %>%
    ungroup()

}

spi_grid <- data.frame(year_low = seq(1000,2000,200),
                       year_hig = seq(1099,2099,200))

for ( i in c(1:dim(spi_grid)[1])){
    temp15 <- data.frame(
      modeled_df %>%
        filter(year %in% c(spi_grid$year_low[i]:spi_grid$year_hig[i])) %>%
        group_by(scenario, month) %>%
        summarise(shape = mean(shapeGI), scale = mean(scaleGI))) %>%
        mutate(year = spi_grid$year_low[i])
 
    ifelse (i == 1, prcp_gamma <- temp15,
            prcp_gamma <- bind_rows(prcp_gamma, temp15))
  }
 
  prcp_gamma <- prcp_gamma %>%
    mutate(index = huc_num)
 
############Sort out dataset to create probability distribution for evert 100 years
# gamma probability distribution
  
   mons <- c(2,5,8,11)
   m_name <- c("DJF", "MAM", "JJA", "SON")
   
  # for (o_month in seq(1:4)){
   
   tmonth <- mons[o_month]
  
  dist_grid <- prcp_gamma %>%
    filter(month == tmonth & scenario == "ssp585")
  
plot_density <- data.frame(precip = seq(0,400,length.out = 600), 
                           year = dist_grid$year[1]) %>% 
  mutate(dens = dgamma(precip, shape= dist_grid$shape[1],scale= dist_grid$scale[1]))
  
    
for (i in c(2:6)) {
  
  temp <- data.frame(precip = seq(0,400,length.out = 600), 
                             year = dist_grid$year[i]) %>% 
    mutate(dens = dgamma(precip,shape= dist_grid$shape[i],scale= dist_grid$scale[i]))
  
  plot_density <- bind_rows(plot_density, temp)
  
}
  
plot_density$year <- as.factor(plot_density$year)
  

#geom_ridge line
dist_grid <- dist_grid %>%
  mutate(mean = shape * scale) %>%
  mutate(pr_05 = qgamma(0.5, shape  = shape, scale = scale)) %>%
  mutate(pr_93 = qgamma(0.933, shape  = shape, scale = scale)) %>%
  mutate(pr_067= qgamma(0.0668, shape = shape, scale = scale))

ver_lines <- data.frame(year = as.factor(dist_grid$year),
                        pr_05 = dist_grid$pr_05, 
                        pr_93 = dist_grid$pr_93,
                        pr_067 = dist_grid$pr_067)

plot_density <- right_join(plot_density, ver_lines, by = 'year', copy = TRUE)

plot_density <- plot_density %>%
  mutate(event = case_when(precip < pr_067 ~ 'dr',
                          precip > pr_93 ~ 'pl',
                          TRUE ~ 'no')) %>%
  mutate(dens2 = case_when(event == 'dr' ~ dens,
                           event == 'pl' ~ NA_real_,
                           event == 'no' ~ NA_real_)) %>%
  mutate(dens3 = case_when(event == 'dr' ~ NA_real_,
                           event == 'pl' ~ dens,
                           event == 'no' ~ NA_real_))

temp1850 <- data.frame(
  modeled_df %>%
    filter(year == 1850 & month == tmonth & scenario == "ssp585") %>%
    summarise(shape = mean(shapeGI), scale = mean(scaleGI))) 

temp1850 <- temp1850 %>%  mutate(mean = shape * scale) %>%
       mutate(pr_05 = qgamma(0.5, shape  = shape, scale = scale)) %>%
       mutate(pr_93 = qgamma(0.933, shape  = shape, scale = scale)) %>%
       mutate(pr_067= qgamma(0.0668, shape = shape, scale = scale))

p <- ggplot(data = plot_density %>%filter(precip < 200)) +
  geom_density_ridges(aes(x = precip, height = dens, y = year, fill = year, group = year,color = year),
                      color = 'grey',fill = 'grey', stat= "identity", scale = 2, alpha = 0.8) +
  geom_density_ridges(aes(x = precip, height = dens2, y = year, fill = year, group = year),
                    color = NA, fill = 'red',stat= "identity", scale = 0.85, alpha = 0.5) +
  geom_density_ridges(aes(x = precip, height = dens3, y = year, fill = year, group = year),
                      color = NA, fill = 'blue',stat= "identity", scale = 0.5, alpha = 0.5) +
  geom_vline(xintercept = c(temp1850$pr_067, temp1850$pr_93, temp1850$pr_05), linetype = "dotted", color = "grey30") +
  scale_fill_viridis_d(alpha = 0.5) +
  scale_color_viridis_d(alpha = 0.5) +
  labs(x = "Precipitation(mm)", y = "Year", title = paste0("huc num: ",nums[huc_num], " in month ", m_name[o_month] )) +
  theme_ridges() +
  theme(
    panel.background = element_rect(fill = NA, colour = "black",linetype = "solid",size = 0.7),
    axis.text = element_text(size = rel(1.0)),
    panel.grid.major.x = element_blank()
   )

p

ggsave(p, filename = paste0(write_figures_path,nums[huc_num],"_",m_name[o_month],"_distribution_rev3.png"),
             width = 10, height = 9, units = "cm")
ggsave(p, filename = paste0(write_figures_path,nums[huc_num],"_",m_name[o_month],"_distribution_rev3.svg"),
       width = 10, height = 9, units = "cm")

#}
   ggplot(data = plot_density %>%filter(precip < 250),
       aes(x = precip, height = dens, y = year,  group = year)) +
  geom_density_ridges_gradient(aes(fill= ..x..),
                      stat= "identity", scale = 3) +
  scale_fill_gradient2(low = "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 100 ,
                       name = "precipitation") +
  theme_ridges(font_size = 20, grid=TRUE, line_size=1, 
               center_axis_labels=TRUE) +
  theme(
    panel.background = element_rect(fill = NA, colour = "black",linetype = "solid"),
    axis.text = element_text(size = rel(1.0))
  )
p
geom_density_ridges_gradient(aes(fill = ..x..), scale = 0.9, size = 1) 

# p <- ggplot(data = pr,aes(x=prcp))+
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[1], scale= dist_grid$scale[1]), 
#                 alpha = 0.4, aes(fill = "1000-1200"), geom = "area")+
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[2], scale= dist_grid$scale[2]),
#                 alpha = 0.4, aes(fill = "1200-1400"), geom = "area") +
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[3], scale= dist_grid$scale[3]),
#                 alpha = 0.4,aes(fill = "1400-1600"),geom = "area") +
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[4], scale= dist_grid$scale[4]), 
#                 alpha = 0.4, aes(fill = "1600-1800"), geom = "area")+
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[5], scale= dist_grid$scale[5]),
#                 alpha = 0.4, aes(fill = "1800-2000"), geom = "area") +
#   stat_function(fun=dgamma, args=list(shape= dist_grid$shape[6], scale= dist_grid$scale[6]),
#                 alpha = 0.4,aes(fill = "2000-2100"),geom = "area") +
#   guides(fill = guide_legend(title = "Periods")) +
#   scale_fill_viridis(discrete = T, direction = -1)+
#   labs(x = "Precipitation(mm)", y = "f(x)", title = paste0("huc num: ",nums[huc_num], " in month ", tmonth )) +
#   theme_bw(base_size = 10) +
#   theme(panel.background = element_rect(size = 2))
# 
# ggplot(d, aes(x, y, height = height, group = y)) 
# 
# p
# ggsave(p, filename = paste0(write_figures_path,nums[huc_num],"_",tmonth,"_distribution_rev2.png"),
#        width = 10, height = 07, units = "cm")

########################################################################################
