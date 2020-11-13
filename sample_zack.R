###############################################################################
## Project:  General template for sampling surveys from an optimized or current survey
## Author: Zack Oyafuso (with additions from Megsie Siple)
## Description: Sample a random set of survey points
###############################################################################
library(sp)
library(raster)
library(RColorBrewer)
library(tidyverse)
library(sf) #for distances


# Source helper functions for plotting ------------------------------------
source(here::here("..","HelperFns","lengthen_pal.R"))


# Load Spatial Grid -------------------------------------------------------
load(here::here("..","Optimal_Allocation_GoA-master",
                "data",
                "Extrapolation_depths.RData"))


# Load Optimization Results -----------------------------------------------
load(here::here("..","Optimal_Allocation_GoA-master",
                "model_11",
                "full_domain",
                "Spatiotemporal_Optimization",
                "optimization_knitted_results.RData"))

# Query which solution to used based on the optimized 1-boat, 15 strata solution-------------------------------
idx <- settings$id[which(settings$strata == 15 & settings$boat == 1)]
# MCS: 15 is the only solution in there at the moment



# Extract survey information ----------------------------------------------
strata_no <- as.numeric(as.character(strata_list[[idx]]$Stratum)) #unique stratum "ID"
nh <- strata_list[[idx]]$Allocation #allocated effort across strata (number of sites to visit)
nstrata <- length(nh) #Number of strata
solution <- res_df[, paste0("sol_", idx)] #Optimized solution


# Take a stratified random sample -----------------------------------------
#Loop across strata, take a random sample based on how many were allocated
nrow(Extrapolation_depths)
length(solution)

sample_vec <- c()
for (istrata in 1:nstrata) {
  #sample from the population of solution values that are that stratum
  sample_vec <- c(sample_vec,
                  sample(x = which(solution == strata_no[istrata]),
                         size = nh[istrata]))
}

# Subset the spatial grid to only those that were sampled -----------------
# Includes trawlable and non-trawlable
Extrapolation_depths[sample_vec,]


# Shortcut to plot solution and simulated survey locations ----------------
goa <- sp::SpatialPointsDataFrame(
  coords = Extrapolation_depths[,c("E_km", "N_km")],
  data = data.frame(solution = solution) )


goa_ras <- raster::raster(x = goa,
                          resolution = 5)
goa_ras <- raster::rasterize(x = goa,
                             y = goa_ras,
                             field = "solution")

strata_pal <- lengthen_pal(x = 1:nstrata,
                           shortpal = brewer.pal(9,"Pastel1"))

plot(goa_ras,
     col = strata_pal)

points(Extrapolation_depths[sample_vec, c("E_km", "N_km")],
       pch = 16,
       cex = 0.5)


# Calculate distances between points --------------------------------------
survey_pts <- Extrapolation_depths[sample_vec, c("Lon", "Lat","E_km", "N_km", "Id","stratum","trawlable")]
field_sf <- sf::st_as_sf(x = Extrapolation_depths,
                         coords = c("Lon","Lat"),
                         crs = 4326)

survey_sf <- sf::st_as_sf(x = survey_pts,
                          coords = c("Lon","Lat"),
                          crs = 4326, agr = "constant")

# Coordinate reference system has been set, so distances will be in units()
distance_matrix_km <- matrix(as.numeric(st_distance(survey_sf) / 1000),
                             nrow = length(sample_vec)) #pairwise distances between survey points
rownames(distance_matrix_km) <- 
  colnames(distance_matrix_km) <- survey_sf$Id

distance_df <- as.data.frame(distance_matrix_km) %>%
  add_column(surveyId = colnames(distance_matrix_km)) %>%
  pivot_longer(cols = colnames(distance_matrix_km))


# Calculate total survey time ---------------------------------------------
#According to N Laman, on average they do 4.7 tows/day
# Roughest estimate (274 survey points)
nrow(survey_pts)/4.7

# Sample each station, starting from furthest west point and sampling the nearest station next:
west_to_east <- survey_pts %>%
  arrange(Lon)
western_end <- west_to_east %>%
  slice(1)

# Check that this is the westernmost survey point:
points(western_end[,c("E_km", "N_km")],col="red")

# Test #1: After completing each survey station, three decision rules:
# 1) which station is closest and unsampled?
# 2) which station is the furthest west unsampled station? (i.e., don't skip over sites going west to east)
# 3) is one of the above 2 deeper than the other? If so, pick the deepest station (to prioritize deeper, longer trawls first per Ned Laman)
test.id <- as.character(survey_sf$Id[1])
get_next_station <- function(stationId = test.id,
                             already_sampled = "636174",
                             distances = distance_df,
                             depths = Extrapolation_depths[sample_vec,c('Id','DEPTH_EFH')], 
                             longs = Extrapolation_depths[sample_vec,c('Id','Lon')]){
  closest <- distances %>% 
    filter(surveyId == stationId) %>%
    filter(!name %in% already_sampled) %>%
    filter(value>0) %>%
    slice_min(value) %>%
    select(name) %>%
    as.character()
  
  furthest_w_unsampled <- longs %>%
    filter(Id != stationId) %>%
    filter(!Id %in% already_sampled) %>%
    slice_min(Lon) %>%
    select(Id) %>%
    as.character()
  
  if(closest == furthest_w_unsampled){
    selection = closest} else{
      depth1 <- depths %>% filter(Id == closest) %>% select(DEPTH_EFH)
      depth2 <- depths %>% filter(Id == furthest_w_unsampled) %>% select(DEPTH_EFH)
      ind <- which.min(c(depth1,depth2))
      selection <- c(closest,furthest_w_unsampled)[ind]
    }
  return(selection)
}

x <- get_next_station(stationId = western_end$Id,already_sampled = NA)
points(filter(survey_pts,Id==x)[,c("E_km", "N_km")],col = 'blue')

y <- get_next_station(stationId = x,already_sampled = c(western_end$Id, x))
points(filter(survey_pts,Id==y)[,c("E_km", "N_km")],col = 'green')

z <- get_next_station(stationId = y,already_sampled = c(western_end$Id, x, y))
points(filter(survey_pts,Id==z)[,c("E_km", "N_km")],col = 'yellow')


# Survey design 1: Pick stations based on proximity, depth, west-t --------
design1 <- rep(NA, times = length(sample_vec))
design1[1] <- western_end$Id
for(i in 2:length(sample_vec)){
  design1[i] <- get_next_station(stationId = design1[i-1],
                                 already_sampled = design1[1:i])
}

d1 <- data.frame(Id = design1, order = 1:length(design1))
d2 <- Extrapolation_depths %>%
  mutate(Id = as.character(Id)) %>%
  right_join(d1,Extrapolation_depths, by = "Id")

plot(goa_ras,
     col = strata_pal)

points(Extrapolation_depths[sample_vec, c("E_km", "N_km")],
       pch = 16,
       cex = 0.5,
       col = 'lightgrey')

# 1 knot = 1.852 km/hr
d3 <- d2 %>% 
  arrange(order) %>%
  add_column(distance_from_prev = NA,
             cumu_distance = 0)
d3$distance_from_prev[1] <- 0

for(i in 2:nrow(d3)){
  d3$distance_from_prev[i] <- distance_df %>%
    filter(surveyId == d3$Id[i], name == d3$Id[i-1]) %>%
    select(value) %>% 
    as.numeric()
  d3$cumu_distance[i] <- sum(d3$distance_from_prev[1:i])
}

tail(d3)
max(d3$cumu_distance) # This is the cumulative total distance traveled during the survey!


# Plot path of surveys ----------------------------------------------------
d3 %>%
  ggplot(aes(x=E_km,y=N_km)) +
  geom_point()

attempt1 <- ggplot() + 
  geom_sf(data = field_sf) + 
  geom_path(data=d3, aes(x=Lon,y=Lat, colour = order)) +
  geom_point(data=d3, aes(x=Lon,y=Lat, colour = order)) +
  scale_colour_viridis_c("Sampling order \n(1 = start of survey)")

attempt1

png(here::here("figures","Attempt1.png"),
    width = 8, height = 5,
    units = 'in',res = 250)
attempt1
dev.off()
# Compare to survey track from 2019 ---------------------------------------
# G:\GOA\GOA 2019\DATA_2019\Ocean Explorer\Leg 4\Globe\Tracks\



# Need to figure out how to make raster + map in ggplot -------------------
#ggplot() + geom_sf(data = field_sf) 
#ggplot(goa_ras)  + geom_raster(aes(x=))
