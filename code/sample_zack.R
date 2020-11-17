
# Project:  General template for sampling surveys from an optimize --------
# General template for sampling surveys from an optimized or current survey
# Author: Zack Oyafuso (with additions from Megsie Siple)
# Description: Sample a random set of survey points

library(sp)
library(raster)
library(RColorBrewer)
library(tidyverse)
library(sf)   # distances


# 1. Load helpers ---------------------------------------------------------
# Palette lengthener function 
source(here::here("code","lengthen_pal.R"))



# 2. Load optimized survey design -----------------------------------------

# * Spatial grid ----------------------------------------------------------
load(here::here("..","Optimal_Allocation_GoA-master",
                "data",
                "Extrapolation_depths.RData"))

# * Load Optimization Results ---------------------------------------------
load(here::here("..","Optimal_Allocation_GoA-master",
                "model_11",
                "full_domain",
                "Spatiotemporal_Optimization",
                "optimization_knitted_results.RData"))


# 3. Pick solution and get survey information -----------------------------
# * 3.1 Query which solution to use based on 1-boat, 15 strata solution----
idx <- settings$id[which(settings$strata == 15 & settings$boat == 1)]


# * 3.2 Extract survey information ----------------------------------------
strata_no <- as.numeric(as.character(strata_list[[idx]]$Stratum)) #unique stratum "ID"
nh <- strata_list[[idx]]$Allocation #allocated effort across strata (n of sites to visit)
nstrata <- length(nh)
solution <- res_df[, paste0("sol_", idx)] #Optimized solution


# * 3.3 Take a stratified random sample -----------------------------------
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

# Subset the spatial grid to only those that were sampled 
# Includes trawlable and non-trawlable
Extrapolation_depths[sample_vec,]


# Shortcut to plot solution and simulated survey locations
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

points(Extrapolation_depths[sample_vec,
                            c("E_km", "N_km")],
       pch = 16,
       cex = 0.5)


# * 3.4 Get sampled points ------------------------------------------------
survey_pts <- Extrapolation_depths[sample_vec,
                                   c("Lon", "Lat","E_km", "N_km",
                                     "Id","stratum","trawlable")]

field_sf <- sf::st_as_sf(x = Extrapolation_depths,
                         coords = c("Lon","Lat"),
                         crs = 4326)

survey_sf <- sf::st_as_sf(x = survey_pts,
                          coords = c("Lon","Lat"),
                          crs = 4326, agr = "constant")

# Pairwise distances between survey points in km
distance_matrix_km <- matrix(as.numeric(st_distance(survey_sf) / 1000),
                             nrow = length(sample_vec)) 

rownames(distance_matrix_km) <- 
  colnames(distance_matrix_km) <- 
  survey_sf$Id

#save(distance_matrix_km,file = here::here("data","DistanceMatrix1.RData"))
#load(here::here("data","DistanceMatrix1.RData"))

# Long-form distance matrix
distance_df <- as.data.frame(distance_matrix_km) %>% 
  add_column(surveyId = colnames(distance_matrix_km)) %>%
  pivot_longer(cols = colnames(distance_matrix_km))


#4.  Calculate total survey time ------------------------------------------
#According to N Laman, on average they do 4.7 tows/day
# Roughest estimate (274 survey points)
nrow(survey_pts)/4.2 #Wayne
nrow(survey_pts)/4.7 #Ned

# Sample each station, starting from furthest west point and sampling the nearest station next:
west_to_east <- survey_pts %>%
  arrange(Lon)
western_end <- west_to_east %>%
  slice(1)

# Check that this is the westernmost survey point:
points(western_end[,c("E_km", "N_km")],col="red")






# * 4.1 Option 1: Prioritize next station by proximity, depth, and W-to-E ----

source(here::here("code","stationdecisions","get_next_station_1.R"))

test.id <- as.character(survey_sf$Id[1])

x <- get_next_station_1(stationId = western_end$Id,already_sampled = NA)
points(filter(survey_pts,Id==x)[,c("E_km", "N_km")],col = 'blue')

y <- get_next_station_1(stationId = x,already_sampled = c(western_end$Id, x))
points(filter(survey_pts,Id==y)[,c("E_km", "N_km")],col = 'green')

z <- get_next_station_1(stationId = y,already_sampled = c(western_end$Id, x, y))
points(filter(survey_pts,Id==z)[,c("E_km", "N_km")],col = 'yellow')

# Setup survey plan
plan1 <- rep(NA, times = length(sample_vec))
plan1[1] <- western_end$Id

for(i in 2:length(sample_vec)){
  plan1[i] <- get_next_station_1(stationId = plan1[i-1],
                                 already_sampled = plan1[1:i])
}

d1 <- data.frame(Id = plan1, nwd_order = 1:length(plan1))

d2 <- Extrapolation_depths %>%
  mutate(Id = as.character(Id)) %>%
  right_join(d1,Extrapolation_depths, by = "Id")

d3 <- d2 %>% 
  arrange(nwd_order) %>%
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

cat("max survey distance (km) \n",
    max(d3$cumu_distance) )

# Plot survey path 

attempt1 <- ggplot() + 
  geom_sf(data = field_sf) + 
  geom_path(data = d3, aes(x = Lon,y = Lat, colour = nwd_order)) +
  geom_point(data = d3, aes(x = Lon,y = Lat, colour = nwd_order)) +
  scale_colour_viridis_c("Sampling order \n(1 = start of survey)") +
  labs(title = "Nearest neighbor / furthest west")

attempt1





# * 4.2 Option 2: Use "traveling salesperson" solution --------------------
source(here::here("code","stationdecisions","tsp.R"))

tsp_sol <- get_tsp_soln(x = distance_matrix_km)

d4 <- d3 %>%
  left_join(tsp_sol, by = c("Id" = "site"))
  
attempt2 <- ggplot() + 
  geom_sf(data = field_sf) + 
  geom_path(data = d4, aes(x = Lon,y = Lat, colour = tsp_order)) +
  geom_point(data = d4, aes(x = Lon,y = Lat, colour = tsp_order)) +
  scale_colour_viridis_c("Sampling order \n(1 = start of survey)") +
  labs(title = "Traveling salesperson")


library(patchwork)

#png(here::here("figures", "Attempt1_2.png"),
#    width = 8, height = 10,
#    units = 'in',res = 250)
attempt1 + attempt2 + plot_layout(ncol=1)
#dev.off()


# Other notes -------------------------------------------------------------
# 1 knot = 1.852 km/hr

# Need to figure out how to make raster + map in ggplot -------------------
#ggplot() + geom_sf(data = field_sf) 
#ggplot(goa_ras)  + geom_raster(aes(x=))
