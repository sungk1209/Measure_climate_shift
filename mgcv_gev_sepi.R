
# *------------------------------------------------------------------
# | PROGRAM NAME:
# | FILE NAME: mgcv_gev_spei.R
# | DATE:
# | CREATED BY:  Kay Sung
# *----------------------------------------------------------------
# | PURPOSE:
# |
# |
# *------------------------------------------------------------------

###########################################################################
## Set the Paths
###########################################################################
require(here)

### Path for Data and Output
data_path <- file.path(here(), "data")
#output_path <- "/fs/ess/PAS1921"
output_path <- file.path(here(), "output")

### Set up output folders
write_output_path <- output_path
dir.create(write_output_path, recursive=TRUE, showWarnings = FALSE)

### Set up figure folder
write_figures_path <- file.path(output_path, "figures")
dir.create(write_figures_path, recursive=TRUE, showWarnings = FALSE)

###########################################################################
###  Load functions
###########################################################################
require(tidyverse)
require(tictoc)
require(viridis)

### For Dates
require(lubridate)

### USGS
require(dataRetrieval)

### For GAMS
require(mgcv)
require(gratia)
require(evgam)

logit_inv <- function(est){
  1.5 * binomial()$linkinv(est) - 1
}

###########################################################################
###  Test with Aspen, CO, USA
###########################################################################
loc <- c(lat = 39.2, col = 106.8)

spei <- 



###########################################################################
###  Fit using mgcv
###########################################################################

### GEV distribution
mgcv_model <- gam(list(values
                       ~ s(year_center, bs = "tp"),
                       ~ s(year_center, bs = "tp"),
                       ~ s(year_center, bs = "tp")),
                  data = rand_df,  #### This distribution is strictly positive
                  family=gevlss(link = list("identity", "identity", "identity")),
                  select = TRUE,
                  optimizer="efs")





