
# Libraries ---------------------------------------------------------------
library(tidyverse)


# Step 1: Get optimal survey design from Zack -----------------------------

# df containing the whole field of possible stations
# local
load(here::here("..","MS_OM_GoA-master","data","Extrapolation_depths.RData"))
head(Extrapolation_depths)

# Survey design objects

load(here::here("..","Optimal_Allocation_GoA-master", "model_11","full_domain","Spatiotemporal_Optimization","optimization_knitted_results.RData"))

# res_df (dataframe) - columns give the stratum to which each cell belongs
# settings (dataframe) - stratum, nboats, overall CV and species-specific CVs
# strata_list (list) - domain, stratum, population, allocation, sampling rate, and range of x1 and x2 stratifying variables for that solution
# strata_stats_list (list) - ? not totally sure

# Single boat solution ----------------------------------------------------
s <- 1
settings[s,]
head(res_df %>% 
  select(ends_with(as.character(s))))

head(strata_list[[s]])

length(unique(Extrapolation_depths$GOA_STRATUM)) # 59 strata here
nrow(unique(strata_list[[s]]["Stratum"])) # 15 strata here
# Get the points for solution 1 from the big dataframe
# Need to sample n points from the whole domain for each stratum