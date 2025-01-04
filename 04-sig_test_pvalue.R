# *------------------------------------------------------------------
# | PROGRAM NAME: #Significant test for region 
# | FILE NAME: sigtest_gammals.R
# | DATE: Mar.31.2024
# | CREATED BY: Kay Sung       
# *----------------------------------------------------------------
# | PURPOSE: Do a significant test for each HUC region
# |          Pick random 1000 mean from each time frame -> compare
# |          Null hypothesis rejected less than 5% agrees 
# |         
# |         We have SPI_curr(calcuate SPI of precip of SPI = 0 at 1850 )    
# *------------------------------------------------------------------


######################
require(tidyverse)
require(lubridate)
require(stats)
#require(mgcv)

select <- dplyr::select

data_path <- "../data/"
output_path <- "../output/figure_revision/"

region_index <- readRDS(paste0("../output/region_index.rds"))

nums <- c("12", "21", "20", "22", "05", "06", "07", "08", "13", "14", 
          "11", "16", "18","15", "09", "17", "04", "19", "10", "03",
          "01","02")

#### year and data of interest

# baseline_year <- c(1810:1839)
# compar_past_year <- c(1900:1929)
# compar_present_year <- c(1990:2019)
# compar_future_year <- c(2080:2099)

baseline_year <- c(1830:1859)
compar_past_year <- c(1910:1939)
compar_present_year <- c(1990:2019)
compar_future_year <- c(2070:2099)

sig_ttest <- data.frame()

for (huc_num in c(1,2,5:22)) {
  
  print(huc_num)
  
  model_spi <- readRDS(paste0(output_path,"model_spi_",nums[huc_num],".rds"))
  
  model_df <- model_spi %>% filter(scenario == "ssp585")
  
### Extract many draws from a baseline and comparison year and jdate

  for (month_interest in c(2,5,8,11)) {
  
  baseline_df <- model_df %>% 
    filter(year(date) %in% baseline_year & month(date) == month_interest) %>%
    select(date, scenario, loc.num, spi_curr) %>%
    arrange(loc.num)
  
  compar_past <- model_df %>% 
    filter(year(date) %in% compar_past_year & month(date) == month_interest)%>%
    select(date, scenario, loc.num, spi_curr)%>%
    arrange(loc.num)
  
  compar_present <- model_df %>% 
    filter(year(date) %in% compar_present_year & month(date) == month_interest)%>%
    select(date, scenario, loc.num, spi_curr)%>%
    arrange(loc.num)
  
  compar_future <- model_df %>% 
    filter(year(date) %in% compar_future_year & month(date) == month_interest)%>%
    select(date, scenario, loc.num, spi_curr)%>%
    arrange(loc.num)
    
   ### Quick plot of density around the extremes
  # ggplot() %>%
  #    + geom_density(data = baseline_df, aes(x = spi_curr),alpha = 0.5) %>%
  #    + geom_density(data = compar_past, aes(x = spi_curr),colour = "orange", alpha = 0.5) %>%
  #    + geom_density(data = compar_present, aes(x = spi_curr),colour = "red", alpha = 0.5) %>%
  #    + geom_density(data = compar_future, aes(x = spi_curr),colour = "blue", alpha = 0.5)
  # 
    ### T test
  
  # t.test.result <- data.frame(loc  = i,
  #                             month = month_interest,
  #                             t.past.drier  = past.drier,
  #                             t.past.wetter = past.wetter,
  #                             t.present.drier = present.drier,
  #                             t.present.wetter = present.wetter,
  #                             t.future.drier =  future.drier,
  #                             t.future.wetter = future.wetter
  # )
   t.test.result <- data.frame()
                              
   for (i in unique(compar_present$loc.num))  {
   
   past.drier <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                    compar_past %>% filter(loc.num  == i) %>% select(spi_curr),
                    alternative = "less")$p.value
   
   past.wetter <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                    compar_past %>% filter(loc.num  == i) %>% select(spi_curr),
                    alternative = "greater")$p.value
   
   present.drier <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                       compar_present %>% filter(loc.num  == i) %>% select(spi_curr),
                       alternative = "less")$p.value
   
   present.wetter <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                              compar_present %>% filter(loc.num  == i) %>% select(spi_curr),
                              alternative = "greater")$p.value
   
   future.drier <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                      compar_future %>% filter(loc.num  == i) %>% select(spi_curr),
                      alternative = "less")$p.value
   
   future.wetter <- t.test(baseline_df %>% filter(loc.num  == i) %>% select(spi_curr),
                      compar_future %>% filter(loc.num  == i) %>% select(spi_curr),
                      alternative = "greater")$p.value
  
   result_df <- data.frame(loc  = i,
                           month = month_interest,
                           t.past.drier  = past.drier,
                           t.past.wetter = past.wetter,
                           t.present.drier = present.drier,
                           t.present.wetter = present.wetter,
                           t.future.drier =  future.drier,
                           t.future.wetter = future.wetter
   )
   t.test.result <- bind_rows(t.test.result,result_df)
  
   }
  temp_sig_t <- t.test.result %>%
    summarise(across(t.past.drier:t.future.wetter, ~ mean(. < 0.25))) %>%
    mutate(huc = nums[huc_num], month = month_interest) %>%
    select(huc, month, everything())

  if (month_interest == 2) {grid_ttest <- temp_sig_t
  } else {grid_ttest <- bind_rows(grid_ttest,temp_sig_t)}
  
  }
  
  if (1 == huc_num) {sig_ttest <- grid_ttest
  } else {sig_ttest <- bind_rows(sig_ttest,grid_ttest)}
  
}

saveRDS(sig_ttest, file = paste0(output_path,"t_test_585.rds"))
### Non parametetric Mann-Whitney U test
#wilcox.test(baseline_draws$spi_low,comparison_draws$spi_low, paired = FALSE, alternative = "two.sided")
#wilcox.test(baseline_draws$spi_low,comparison_draws$spi_low, paired = FALSE, alternative = "two.sided")
