###############################################################################
## Project:  General template for sampling surveys from an optimized or current survey
## Author: Zack Oyafuso (with additions from Megsie Siple)
## Description: Sample a random set of survey points
###############################################################################
library(sp)
library(raster)
library(RColorBrewer)
library(tidyverse)

##################################################
####   Set working directory to the GitHub Repo
##################################################
setwd(here::here("..","Optimal_Allocation_GoA-master"))

##################################################
####   Load Spatial Grid
##################################################
load("data/Extrapolation_depths.RData")

##################################################
####   Load Optimization Results
##################################################
load(paste0("model_11/full_domain/Spatiotemporal_Optimization/",
            "optimization_knitted_results.RData"))

##################################################
####   Query which solution to used based on the
####   optimized 1-boat, 15 strata solution
##################################################

idx <- settings$id[which(settings$strata == 15 & settings$boat == 1)]
# MCS: 15 is the only solution in there at the moment.

##################################################
####  Extract survey information:
##################################################
strata_no <- as.numeric(as.character(strata_list[[idx]]$Stratum)) #unique stratum "ID"
nh <- strata_list[[idx]]$Allocation #allocated effort across strata (number of sites to visit)
nstrata <- length(nh) #Number of strata
solution <- res_df[, paste0("sol_", idx)] #Optimized solution

##################################################
####   Take a stratified random sample
##################################################
#Loop across strata, take a random sample based on how many were allocated
sample_vec <- c()
for (istrata in 1:nstrata) {
  sample_vec <- c(sample_vec,
                  sample(x = which(solution == strata_no[istrata]),
                         size = nh[istrata]))
  
  plot(goa_ras,
       col = c(RColorBrewer::brewer.pal(name = "Paired", n = 12),
               RColorBrewer::brewer.pal(name = "Paired", n = 3)) )
  
  points(Extrapolation_depths[sample_vec, c("E_km", "N_km")],
         pch = 16,
         cex = 0.5)
}

##################################################
####   Subset the spatial grid to only those that were sampled
##################################################
Extrapolation_depths[sample_vec,]

##################################################
####   Shortcut to plot solution and simulated survey locations
##################################################
goa <- sp::SpatialPointsDataFrame(
  coords = Extrapolation_depths[,c("E_km", "N_km")],
  data = data.frame(solution = solution) )
goa_ras <- raster::raster(x = goa,
                          resolution = 5)
goa_ras <- raster::rasterize(x = goa,
                             y = goa_ras,
                             field = "solution")

plot(goa_ras,
     col = c(RColorBrewer::brewer.pal(name = "Paired", n = 12),
             RColorBrewer::brewer.pal(name = "Paired", n = 3)) )

points(Extrapolation_depths[sample_vec, c("E_km", "N_km")],
       pch = 16,
       cex = 0.5)
