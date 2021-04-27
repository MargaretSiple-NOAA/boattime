# Simulate several 2-boat realizations
# This script is used to get standard deviations for the distances (several realizations of the same 2-boat solution)
library(tidyverse)


source(here::here("code", "stationdecisions", "get_next_station_1.R"))


nsims <- 1000


# * Spatial grid ----------------------------------------------------------
load(here::here(
  "..", "Optimal_Allocation_GoA",
  "data",
  "Extrapolation_depths.RData"
))
# dataframe: Extrapolation_depths

# * Load Optimization Results ---------------------------------------------

load(here::here(
  "..", "Optimal_Allocation_GoA",
  "results",
  "MS_optimization_knitted_results.RData"
))
# df's and lists: res_df, settings, strata_list, strata_stats_list

# 3. Pick solution and get survey information -----------------------------
# * 3.1 Query which solution to use based on 2-boat, district-level solution with three strata per district----
# Important! Number of boats.
nboats <- 2

idx <- which(settings$strata == 3 & 
               settings$boat == nboats & 
               settings$domain == "district")

# * 3.2 Extract solution --------------------------------------------------
strata_no <- 1:nrow(strata_list[[idx]]) # stratum "ID"
nh <- strata_list[[idx]]$Allocation # allocated effort across strata (n of locations to visit)
nstrata <- length(nh)
solution <- res_df[, idx] # Optimized solution

sims_list <- list()

# START SIM LOOP
for(sim in 1:nsims){
  # Randomly draw samples from the survey
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
  
  # These are the stations to visit
  survey_pts <- Extrapolation_depths[
    sample_vec,
    c(
      "Lon", "Lat", "E_km", "N_km",
      "Id", "stratum", "trawlable",
      "DEPTH_EFH"
    )
  ]
  
  survey_sf <- sf::st_as_sf(
    x = survey_pts,
    coords = c("Lon", "Lat"),
    crs = 4326, agr = "constant"
  )
  
  # Pairwise distances between survey points in km
  # st_distance() provides distances in m
  distance_matrix_km <- matrix(as.numeric(sf::st_distance(survey_sf) / 1000),
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
  
  # Sort which boats go to which stations
  depth_quants <- quantile(survey_pts$DEPTH_EFH)
  
  # assume n = 2 boats for now
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
    
    # Get Extrapolation depths rows just for this one boat
    edepths <- Extrapolation_depths %>% 
      filter(Id %in% surv_pts_boat$Id)
    
    # Loop through stations, assigning a "next station" for each one
    for (i in 2:length(boat_plan)) {
      boat_plan[i] <- get_next_station_1(
        stationId = boat_plan[i - 1],
        already_sampled = boat_plan[1:i],
        depths = edepths[,c("Id","DEPTH_EFH")],
        longs = edepths[,c("Id","Lon")]
      )
    }
    
    d1 <- data.frame(Id = boat_plan, nwd_order = 1:sample_size)
    
    # Get locations/depths for all the stations
    d2 <- Extrapolation_depths %>%
      mutate(Id = as.character(Id)) %>%
      right_join(d1, Extrapolation_depths, by = "Id")
    
    # Get cumulative distance etc. for the boat plan
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
  df_both <- do.call(rbind.data.frame, df_list) %>%
    mutate(whichsim = sim)
  
  sims_list[[sim]] <- df_both
  print(sim/nsims)
}


simresults <- do.call(rbind.data.frame, sims_list) %>% 
  group_by(whichsim, boat) %>%
  summarize(cumu_distance = max(cumu_distance)) %>%
  ungroup() %>%
  group_by(boat) %>%
  summarize(avg = mean(cumu_distance),
            sd = sd(cumu_distance)) %>%
  ungroup()

simresults
