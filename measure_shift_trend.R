
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
gam_data_path <- ("D:/Paper3/output/Final_analysis")

output_path <- "../output/"
dir.create(output_path, recursive = FALSE)

write_figures_path <- file.path(output_path, "/figure/")
dir.create(write_figures_path, recursive=FALSE, showWarnings = FALSE)

naspa_grid <- readRDS(file = paste0(gam_data_path, "/naspa_grid.rds"))

#Distribution in vertical every 30 years

i <- 7

loc <- naspa_grid[ohio$index,]

modeled_df <- readRDS(file = paste0(gam_data_path,"/",j,"_modeled_ts.rds"))
modeled_df <- modeled_df %>%
  arrange(date)

spi_grid <- data.frame(loc = j, 
                       year_low = seq(1601,2071,30),
                       year_hig = seq(1630,2100,30))
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

prcp_gamma<- prcp_gamma %>%
  mutate(lat = loc$lat, lon = loc$lon, index = j) 

###gamma probability distribution 
prcp_gamma <- prcp_gamma %>% mutate(spi0 = qgamma(0.5, shape  = shape, scale = scale)) %>%
                     mutate(spi_d15 = qgamma(0.0667, shape  = shape, scale = scale)) %>%
                     mutate(spi_w15 = qgamma(0.933, shape  = shape, scale = scale))

prs <- seq(0,150,1)
y    <- as.vector(sapply(1:dim(prcp_gamma)[1], function(i) dgamma(prs, prcp_gamma$shape[i], scale = prcp_gamma$scale[i])))
df   <- data.frame(x = rep(prs,dim(prcp_gamma)[1]), y, group = rep(1:dim(prcp_gamma)[1], each = length(prs)),
                   month = rep(prcp_gamma$month, each = length(prs)), 
                   scenario = rep(prcp_gamma$scenario, each = length(prs)),
                   year = rep(prcp_gamma$year, each = length(prs)))
#labs <- sapply(1:dim(prcp_gamma)[1], function(i) {
#  substitute(paste("year = ",s),
#             list(s=prcp_gamma$year[i]))})

p <- ggplot(data = df %>%filter(scenario == "ssp585" ), aes(x, y)) + 
  geom_line(aes(color = year, group = year)) +
  ylab(expression("probability")) +
  xlab("3-months average precipitation(mm)") +
  #scale_color_manual(values = c(1800, 1900, 2000, 2070), labels = labs) +
  scale_color_viridis(direction = -1 ) +
  facet_wrap(vars(month), ncol = 4,scales = "free") + 
 # coord_flip() +
  theme_classic()
p + geom_vline(data = prcp_gamma %>% filter(scenario == "ssp585"), 
               aes(xintercept = spi0, color = year)) +
 facet_wrap(vars(month), ncol = 4,scales = "free")


##########Time series of SPI = 0
###testing for ohio basin 

j <- 5501

modeled_all <- modeled_df
indexes<- ohio$index
for (j in indexes[2:10]) {

modeled_df <- readRDS(file = paste0(data_path,"/",j,"_modeled_ts.rds"))
modeled_df <- modeled_df %>%
 mutate(loc.num = j) 

modeled_all <- bind_rows(modeled_all,modeled_df)
}

model_spi <- modeled_all %>%
  group_by(date, scenario, loc.num) %>%
  summarise(mean = mean(modGI), shape = mean(shapeGI), scale = mean(scaleGI),
            est_sd = mean(est_sd)) %>%
  mutate(month = month(date))

spi_1850 <- model_spi %>%
  filter(year(date) == 1850) %>%
  mutate(spi0_1850 = qgamma(0.5, shape  = shape, scale = scale)) %>% 
  mutate(month = month(date)) %>%
  select(loc.num, scenario, month, spi0_1850)

model_spi <- left_join(model_spi,spi_1850, by = c("month", "scenario","loc.num"))

model_spi <- model_spi %>%
   mutate(dgam= pgamma(spi0_1850, shape = shape, scale = scale)) %>%
   mutate(spi_curr = qnorm(dgam, mean = 0, sd= 1))

data_breaks <- data.frame(starty = c(0, Inf),  # Create data with breaks
                          endy = c(-Inf,0),
                          change = c("Wetter","Drier"))
model_spi <- model_spi %>% filter(scenario == "ssp585")

p <- ggplot(model_spi) +
  geom_rect(data = data_breaks,
            aes(xmin = -Inf,xmax = Inf, ymin =starty,ymax = endy, fill = change),
            alpha = 0.1) +
  scale_fill_manual(values = c("Wetter" = "#0000FF","Drier" = "#FF0000")) +
  geom_line(aes(x = year(date.x), y = spi_curr, group = loc.num)) +
  #scale_color_manual(values = c("ssp126" = "black","ssp585"= "red")) +
  geom_vline(xintercept = 1850,color = "black", linetype = "dashed") +
   facet_wrap(vars(month)) +
  scale_y_continuous(limits = c(-1.5,1.5)) +
  scale_x_continuous(limits = c(800,2100)) +
  theme_bw()

p  + stat_summary(fun = "median", aes(x = year(date.x), y = spi_curr), 
                  size = 0.1, color = "red")












