
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

select <- dplyr::select

data_path <- "../data/"
output_path <- "../output/"
gam_path <- "D:/Paper3/output/GAM_result"
dir.create(output_path, recursive = FALSE)

write_figures_path <- file.path(output_path, "/figure_revision/")
dir.create(write_figures_path, recursive=FALSE, showWarnings = FALSE)

#naspa_grid <- readRDS(file = paste0(data_path, "/naspa_grid.rds"))

#Distribution in vertical every 30 years

# i <- 7
# 
# loc <- naspa_grid[ohio$index,]
# 
# modeled_df <- readRDS(file = paste0(gam_data_path,"/",j,"_modeled_ts.rds"))
# modeled_df <- modeled_df %>%
#   arrange(date)
# 
# spi_grid <- data.frame(loc = j, 
#                        year_low = seq(1601,2071,30),
#                        year_hig = seq(1630,2100,30))
# for ( i in c(1:dim(spi_grid)[1])){
#   temp15 <- data.frame(
#     modeled_df %>%
#       filter(year %in% c(spi_grid$year_low[i]:spi_grid$year_hig[i])) %>%
#       group_by(scenario, month) %>%
#       summarise(shape = mean(shapeGI), scale = mean(scaleGI))) %>%
#     mutate(year = spi_grid$year_low[i])
#   
#   ifelse (i == 1, prcp_gamma <- temp15,
#           prcp_gamma <- bind_rows(prcp_gamma, temp15))
# }
# 
# prcp_gamma<- prcp_gamma %>%
#   mutate(lat = loc$lat, lon = loc$lon, index = j) 
# 
# ###gamma probability distribution 
# prcp_gamma <- prcp_gamma %>% mutate(spi0 = qgamma(0.5, shape  = shape, scale = scale)) %>%
#                      mutate(spi_d15 = qgamma(0.0667, shape  = shape, scale = scale)) %>%
#                      mutate(spi_w15 = qgamma(0.933, shape  = shape, scale = scale))
# 
# prs <- seq(0,150,1)
# y    <- as.vector(sapply(1:dim(prcp_gamma)[1], function(i) dgamma(prs, prcp_gamma$shape[i], scale = prcp_gamma$scale[i])))
# df   <- data.frame(x = rep(prs,dim(prcp_gamma)[1]), y, group = rep(1:dim(prcp_gamma)[1], each = length(prs)),
#                    month = rep(prcp_gamma$month, each = length(prs)), 
#                    scenario = rep(prcp_gamma$scenario, each = length(prs)),
#                    year = rep(prcp_gamma$year, each = length(prs)))
# #labs <- sapply(1:dim(prcp_gamma)[1], function(i) {
# #  substitute(paste("year = ",s),
# #             list(s=prcp_gamma$year[i]))})
# 
# p <- ggplot(data = df %>%filter(scenario == "ssp585" ), aes(x, y)) + 
#   geom_line(aes(color = year, group = year)) +
#   ylab(expression("probability")) +
#   xlab("3-months average precipitation(mm)") +
#   #scale_color_manual(values = c(1800, 1900, 2000, 2070), labels = labs) +
#   scale_color_viridis(direction = -1 ) +
#   facet_wrap(vars(month), ncol = 4,scales = "free") + 
#  # coord_flip() +
#   theme_classic()
# p + geom_vline(data = prcp_gamma %>% filter(scenario == "ssp585"), 
#                aes(xintercept = spi0, color = year)) +
#  facet_wrap(vars(month), ncol = 4,scales = "free")


##########Time series of SPI = 0
###

region_index <- readRDS(paste0(output_path,"/region_index.rds"))

nums <- c("12", "21", "20", "22", "05", "06", "07", "08", "13", "14", 
          "11", "16", "18","15", "09", "17", "04", "19", "10", "03",
          "01","02")

for (huc_num in c(20:22)){

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
            std = mean(est_sd)) %>%
  mutate(month = month(date)) %>%
  ungroup()

spi_1850 <- model_spi %>%
  filter(year(date) == 1850) %>%
  mutate(spi0_1850 = qgamma(0.5, shape  = shape, scale = scale)) %>% 
  mutate(month = month(date)) %>%
  select(loc.num, scenario, month, spi0_1850, std)

model_spi <- left_join(model_spi, spi_1850, by = c("month", "scenario","loc.num"))

model_spi <- model_spi %>%
   mutate(dgam= pgamma(spi0_1850, shape = shape, scale = scale)) %>%
   mutate(spi_curr = qnorm(dgam, mean = 0, sd= 1))

saveRDS(model_spi, paste0(output_path,"model_spi_",nums[huc_num],".rds"))


#################Create graph with median value without stat summary
# open the result file

#model_spi <- readRDS(paste0(output_path,"/all_region_spi_df.rds"))


####################
for (tmonth in c(2,5,8,11)){
temp_df <- model_spi %>% filter(month == tmonth) %>%
  group_by(year(date),scenario) %>%
  summarise(median = median(spi_curr), iqr = IQR(spi_curr), shape = median(shape), scale = median(scale)) %>%
    ungroup()

# to make red and blue shades in the background
temp_df <- temp_df %>%
  mutate(
    sign = as.factor(case_when(
      median + iqr/2 >= 0 & median - iqr/2 >= 0 ~ 1,
      median + iqr/2 <= 0 & median - iqr/2 <= 0 ~ -1,
      TRUE ~ 0
    )))

colnames(temp_df) <- c("year", "scenario","median","iqr","shape","scale","sign")

temp_df <- temp_df %>%
  mutate(std = sqrt(shape * scale^2))

# data_breaks <- data.frame(startx = seq(850,2099),
#                           endx = seq(851,2100),
#                           change = temp_df %>%filter(scenario=="ssp585" & year > 849) %>% select(sign))

data_breaks <- data.frame(starty = c(-Inf, 0),  # Create data with breaks
                          endy = c(0,Inf),
                          startx = c(1400, 1400),
                          endx = c(2100, 2100),
                          change = c("Wetter","Drier"))

p <- ggplot(temp_df %>%filter((scenario == "ssp585" & year > 1400) | (scenario == "ssp126" & year > 1850))) + 
    geom_rect(data = data_breaks,
           aes(xmin = startx, xmax = endx, ymin =starty, ymax = endy, fill = change), alpha = 0.3) +
  # scale_fill_manual(values = c("-1" = "#0000FF", "1" = "firebrick","0" = NA,
  #                              "ssp126" = "#008000", "ssp585"= "black"),
  #                   labels = c("Wetter","No change","Drier","ssp126","ssp585")) +
  scale_fill_manual(values = c("Wetter" = "#0000FF", "Drier" = "firebrick","0" = NA,
                               "ssp126" = "#008000", "ssp585"= "black"),
                    labels = c("Wetter","Drier","No change","ssp126","ssp585")) +
  geom_ribbon(aes(ymax = median + iqr/2, ymin = median - iqr/2, x = year, 
                  group = scenario, fill = scenario), alpha = 0.4) +
  geom_line(aes(x = year, y = median, group = scenario, color = scenario))+
    scale_color_manual(values = c("ssp126" = "#008000", "ssp585"= "black")) +
  geom_vline(xintercept = 1850,color = "black", linetype = "dashed") +
  geom_vline(xintercept = 2022,color = "black", linetype = "dashed") +
  labs(title = paste0("3 months mean prcp at huc num: ",nums[huc_num], " in month ", tmonth )) +
  scale_y_continuous(trans = "reverse", limits =c(1.0,-1.0), name = "SPI-3") +
  scale_x_continuous(limits = c(1400,2100), name = "Year") +
  theme_bw(base_size = 22 )

p
ggsave(p, filename = paste0(write_figures_path,"/",nums[huc_num],"_",tmonth,"_pr_TS_1850.png"),
       width = 23, height = 17, units = "cm")
ggsave(p, filename = paste0(write_figures_path,nums[huc_num],"_",tmonth,"_pr_TS_1850.svg"),
       width = 23, height = 17, units = "cm")

# p <- ggplot(temp_df %>%filter((scenario == "ssp585") | (scenario == "ssp126"))) + 
#       geom_line(aes(x = year, y = std, group = scenario, color = scenario))+
#       scale_color_manual(values = c("ssp126" = "#008000", "ssp585"= "black")) +
#       geom_vline(xintercept = 1850,color = "black", linetype = "dashed") +
#       labs(title = paste0(huc_num, "_", tmonth)) +
#       scale_y_continuous(name = "ST.Deviation") +
#       scale_x_continuous(limits = c(850,2100), name = "Year") +
#       labs(title = paste0("st. deviation at huc num: ",nums[huc_num], " in month ", tmonth )) +
#       theme_bw(base_size = 14 )
# 
# p
# 
# ggsave(p, filename = paste0(write_figures_path,nums[huc_num],"_",tmonth,"_std_TS_1850.png"),
#        width = 20, height = 17, units = "cm")
}

}








