# Project:  General template for sampling surveys from an optimize --------
# General template for sampling surveys from an optimized or current survey
# Author: Zack Oyafuso (with additions from Megsie Siple)
# Description: Sample a random set of survey points

library(sp)
library(raster)
library(RColorBrewer)
library(tidyverse)
library(sf)


# 1. Load helpers ---------------------------------------------------------
# Palette lengthener function
source(here::here("code", "lengthen_pal.R"))


# 2. Load optimized survey design -----------------------------------------

# * Spatial grid ----------------------------------------------------------
load(here::here(
  "..", "Optimal_Allocation_GoA-master",
  "data",
  "Extrapolation_depths.RData"
))
# dataframe: Extrapolation_depths

# * Load Optimization Results ---------------------------------------------
load(here::here(
  "..", "Optimal_Allocation_GoA-master",
  "model_11",
  "full_domain",
  "Spatiotemporal_Optimization",
  "optimization_knitted_results.RData"
))
# df's and lists: res_df, settings, strata_list, strata_stats_list

# 3. Pick solution and get survey information -----------------------------
# * 3.1 Query which solution to use based on 1-boat, 15 strata solution----
# Important! Number of boats.
nboats <- 2

idx <- settings$id[which(settings$strata == 15 & settings$boat == nboats)]

# * 3.2 Extract solution --------------------------------------------------
strata_no <- as.numeric(as.character(strata_list[[idx]]$Stratum)) # unique stratum "ID"
nh <- strata_list[[idx]]$Allocation # allocated effort across strata (n of locations to visit)
nstrata <- length(nh)
solution <- res_df[, paste0("sol_", idx)] # Optimized solution


# * 3.3 Take a stratified random sample -----------------------------------
# Loop across strata, take a random sample based on how many were allocated
nrow(Extrapolation_depths)
length(solution)

sample_vec <- c()
for (istrata in 1:nstrata) {
  sample_vec <- c(
    sample_vec,
    sample(
      x = which(solution == strata_no[istrata]),
      size = nh[istrata]
    )
  )
}

# Subset the spatial grid to only those that were sampled
# Includes trawlable and non-trawlable locations
Extrapolation_depths[sample_vec, ]


# Shortcut to plot solution and simulated survey locations
goa <- sp::SpatialPointsDataFrame(
  coords = Extrapolation_depths[, c("E_km", "N_km")],
  data = data.frame(solution = solution)
)


goa_ras <- raster::raster(
  x = goa,
  resolution = 5
)
goa_ras <- raster::rasterize(
  x = goa,
  y = goa_ras,
  field = "solution"
)

strata_pal <- lengthen_pal(
  x = 1:nstrata,
  shortpal = brewer.pal(9, "Pastel1")
)

plot(goa_ras,
     col = strata_pal
)

points(Extrapolation_depths[
  sample_vec,
  c("E_km", "N_km")
],
pch = 16,
cex = 0.5
)


# * 3.4 Get sampled points ------------------------------------------------
survey_pts <- Extrapolation_depths[
  sample_vec,
  c(
    "Lon", "Lat", "E_km", "N_km",
    "Id", "stratum", "trawlable",
    "DEPTH_EFH"
  )
]

field_sf <- sf::st_as_sf(
  x = Extrapolation_depths,
  coords = c("Lon", "Lat"),
  crs = 4326
)


# * 3.5 Assign boats to stations randomly ---------------------------------
depth_quants <- quantile(survey_pts$DEPTH_EFH)

if (nboats == 2) {
  print("2-boat solution")
  nperboat <- nrow(survey_pts) / 2
  if (!is.integer(nperboat)) {
    n1 <- floor(nperboat)
    n2 <- ceiling(nperboat)
    n3 <- 0
  } else {
    (n1 <- n2 <- nperboat)
    n3 <- 0
  }
  bb <- c(rep(1, times = n1), rep(2, times = n2))
  survey_pts <- survey_pts %>%
    #assign stations to boats by depth:
    mutate(whichboat = ifelse(DEPTH_EFH < depth_quants["50%"], 1, 2))
    #assign stations to boats randomly:
  # mutate(whichboat = sample(x = bb, size = n1 + n2,replace = FALSE))
}

if (nboats == 3) {
  print("3-boat solution")
  nperboat <- nrow(survey_pts) / 3
  if (!is.integer(nperboat)) {
    n1 <- floor(nperboat)
    n2 <- ceiling(nperboat)
    n3 <- nrow(survey_pts) - (n1 + n2)
  } else {
    (n1 <- n2 <- n3 <- nperboat)
  }
  bb <- c(
    rep(1, times = n1),
    rep(2, times = n2),
    rep(3, times = n3)
  )
  survey_pts <- survey_pts %>%
    # assign stations to boats by depth:
    mutate(whichboat = case_when(DEPTH_EFH < depth_quants["25%"] ~ 1,
                                 DEPTH_EFH > depth_quants["25%"] &
                                 DEPTH_EFH < depth_quants["75%"] ~ 2,
                                 DEPTH_EFH > depth_quants["75%"] ~ 3))
    # assign boats randomly: 
    # mutate(whichboat = sample(x = bb, size = n1 + n2 + n3,
   #                           replace = FALSE)) #randomly assign stations to boats 1 and 2
}

# Turn survey points into sf object for getting distances
survey_sf <- sf::st_as_sf(
  x = survey_pts,
  coords = c("Lon", "Lat"),
  crs = 4326, agr = "constant"
)

# Pairwise distances between survey points in km
# st_distance() provides distances in m
distance_matrix_km <- matrix(as.numeric(st_distance(survey_sf) / 1000),
                             nrow = nrow(survey_sf)
)

rownames(distance_matrix_km) <-
  colnames(distance_matrix_km) <-
  survey_sf$Id

# save(distance_matrix_km,file = here::here("data","DistanceMatrix1.RData"))
# load(here::here("data","DistanceMatrix1.RData"))

# Long-form distance matrix
distance_df <- as.data.frame(distance_matrix_km) %>%
  add_column(surveyId = colnames(distance_matrix_km)) %>%
  pivot_longer(cols = colnames(distance_matrix_km))
nearest_neighbor <- distance_df %>%
  arrange(value) %>%
  group_by(surveyId) %>%
  summarize(nn1 = nth(value,n = 2), # min distance will always be 0
            nn2 = nth(value,n = 3)) %>%
  add_column(year = "optimal")


save(nearest_neighbor,file = "data/processed/nearest_neighbors_optimal.RData")



# 4.  Calculate total survey time ------------------------------------------
# According to N Laman, on average each boat does 4.7 tows/day
# Roughest estimate (274 survey points)
nrow(survey_pts) / 4.2 / nboats # Wayne
nrow(survey_pts) / 4.7 / nboats # Ned

# Sample each station, starting from furthest west point and sampling the nearest station next:
western_end <- survey_pts %>%
  arrange(Lon) %>%
  slice(1)

# Check that this is the westernmost survey point:
points(western_end[, c("E_km", "N_km")], col = "red")




# * 4.1 Option 1: Prioritize next station by proximity, depth, and W-to-E ----

source(here::here("code", "stationdecisions", "get_next_station_1.R"))

test.id <- as.character(survey_sf$Id[1])

x <- get_next_station_1(stationId = western_end$Id, already_sampled = NA)
points(filter(survey_pts, Id == x)[, c("E_km", "N_km")], col = "blue")

y <- get_next_station_1(stationId = x, already_sampled = c(western_end$Id, x))
points(filter(survey_pts, Id == y)[, c("E_km", "N_km")], col = "green")

z <- get_next_station_1(stationId = y, already_sampled = c(western_end$Id, x, y))

# Double check to make sure order of points makes sense:
points(filter(survey_pts, Id == z)[, c("E_km", "N_km")], col = "yellow")




# Setup survey plan for multiple boats

df_list <- list()

for(b in 1:nboats){
# Boat 1
  surv_pts_boat <- survey_pts %>% 
  filter(whichboat == b)

  western_end <- surv_pts_boat %>%
    arrange(Lon) %>%
    slice(1)
  
  sample_size <- case_when(b==1 ~ n1,
                           b==2 ~ n2,
                           b==3 ~ n3)
  boat_plan <- rep(NA, times = sample_size)
  boat_plan[1] <- western_end$Id
  
  # Get Extrapolation depths rows just for that one boat
  edepths <- Extrapolation_depths %>% 
    filter(Id %in% surv_pts_boat$Id)
  
  for (i in 2:length(boat_plan)) {
    boat_plan[i] <- get_next_station_1(
      stationId = boat_plan[i - 1],
      already_sampled = boat_plan[1:i],
      depths = edepths[,c("Id","DEPTH_EFH")],
      longs = edepths[,c("Id","Lon")]
    )
  }
  
  d1 <- data.frame(Id = boat_plan, nwd_order = 1:sample_size)
  
  d2 <- Extrapolation_depths %>%
    mutate(Id = as.character(Id)) %>%
    right_join(d1, Extrapolation_depths, by = "Id")
  
  d3 <- d2 %>%
    arrange(nwd_order) %>%
    add_column(
      distance_from_prev = NA,
      cumu_distance = 0
    )
  d3$distance_from_prev[1] <- 0
  
  for (i in 2:nrow(d3)) {
    d3$distance_from_prev[i] <- distance_df %>%
      filter(surveyId == d3$Id[i], name == d3$Id[i - 1]) %>%
      dplyr::select(value) %>%
      as.numeric()
    d3$cumu_distance[i] <- sum(d3$distance_from_prev[1:i])
    d3$boat <- b
  }
  
  tail(d3)
  
  cat(
    "max survey distance (km) \n",
    max(d3$cumu_distance), "\n ",
    "max inter-station distance (km) \n",
    max(d3$distance_from_prev), "\n"
  )
  
  df_list[[b]] <- d3
}

length(df_list)
df_both <- do.call(rbind.data.frame, df_list)

# TO DO: 
# Add function to plot stations and station order from dataframe d3

# Plot survey path (2 vessels)

twoboats <- ggplot() +
  geom_sf(data = field_sf) +
  geom_path(data = df_list[[1]], aes(x = Lon, y = Lat, colour = nwd_order)) +
  geom_point(data = df_list[[1]], aes(x = Lon, y = Lat, colour = nwd_order)) +
  scale_colour_gradientn("Boat 1", colors = c('white', 'blue')) +
  ggnewscale::new_scale_color() +
  geom_path(data = df_list[[2]], aes(x = Lon, y = Lat, colour = nwd_order)) +
  geom_point(data = df_list[[2]], aes(x = Lon, y = Lat, colour = nwd_order)) +
  scale_colour_gradientn("Boat 2", colours = c("white", "red")) +
  labs(title = "Nearest neighbor / furthest west") +
  facet_wrap(~boat, nrow = 2)

png("figures/ByDepth.png",width = 7, height = 8,units = "in",res = 200)
twoboats
dev.off()

# Plot survey path
attempt1 <- ggplot() +
  geom_sf(data = field_sf) +
  geom_path(data = d3, aes(x = Lon, y = Lat, colour = nwd_order)) +
  geom_point(data = d3, aes(x = Lon, y = Lat, colour = nwd_order)) +
  scale_colour_viridis_c("Sampling order \n(1 = start of survey)") +
  labs(title = "Nearest neighbor / furthest west")

attempt1





# * 4.2 Option 2: Use "traveling salesperson" solution --------------------
source(here::here("code", "stationdecisions", "tsp.R"))

tsp_sol <- get_tsp_soln(x = distance_matrix_km)

d4 <- d3 %>%
  left_join(tsp_sol, by = c("Id" = "site"))

attempt2 <- ggplot() +
  geom_sf(data = field_sf) +
  geom_path(data = d4, aes(x = Lon, y = Lat, colour = tsp_order)) +
  geom_point(data = d4, aes(x = Lon, y = Lat, colour = tsp_order)) +
  scale_colour_viridis_c("Sampling order \n(1 = start of survey)") +
  labs(title = "Traveling salesperson")


library(patchwork)

# png(here::here("figures", "Attempt1_2.png"),
#    width = 8, height = 10,
#    units = 'in',res = 250)
attempt1 + attempt2 + plot_layout(ncol = 1)
# dev.off()



# 5. Distribution of two closest stations for each station ----------------
distance_df # distances between each pair

nearest_neighbor <- distance_df %>%
  arrange(value) %>%
  group_by(surveyId) %>%
  summarize(
    nn1 = nth(value, n = 2), # min distance will always be 0
    nn2 = nth(value, n = 3)
  )
nnlabs <- c("Nearest neighbor", "Second-nearest neighbor")
names(nnlabs) <- c("nn1", "nn2")

# Plot distribution of nearest neighbors
nnplot <- nearest_neighbor %>%
  pivot_longer(cols = nn1:nn2) %>%
  ggplot(aes(x = value, fill = name)) +
  geom_histogram(alpha = 0.2, binwidth = 5, position = "identity") +
  xlab("Distance (km)") +
  ylab("Frequency") +
  scale_fill_discrete("",
                      labels = c(
                        "Nearest station",
                        "Second-nearest station"
                      )
  )


#png(filename = here::here("figures", "Optimal.png"), width = 6, height = 5, units = "in", res = 150)
nnplot
#dev.off()




#6.  Get images of two realizations of survey for Wayne et al. ---------------
# Use the existing realization from above.


realization1 <- 
  ggplot() +
  geom_sf(data = field_sf, colour = 'white') +
  geom_point(data = survey_pts, aes(x = Lon, y = Lat),
             pch = 21, colour = 'black', fill = 'white')

sample_vec2 <- c()
for (istrata in 1:nstrata) {
  sample_vec2 <- c(
    sample_vec2,
    sample(
      x = which(solution == strata_no[istrata]),
      size = nh[istrata]
    )
  )
}

survey_pts2 <- Extrapolation_depths[
  sample_vec2,
  c(
    "Lon", "Lat", "E_km", "N_km",
    "Id", "stratum", "trawlable",
    "DEPTH_EFH"
  )
]

realization2 <- ggplot() +
  geom_sf(data = field_sf, colour = 'white') +
  geom_point(data = survey_pts2, aes(x = Lon, y = Lat),
             pch = 21, colour = 'black', fill = 'white')

png("figures/OptimizedDesigns.png",width = 12, height = 6, units = 'in',res = 200)
realization1 + realization2
dev.off()




# Other notes -------------------------------------------------------------
# 1 knot = 1.852 km/hr

# Need to figure out how to make raster + map in ggplot -------------------
# ggplot() + geom_sf(data = field_sf)
# ggplot(goa_ras)  + geom_raster(aes(x=))