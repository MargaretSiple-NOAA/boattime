# Project:  Load and compare historical surveys ---------------------------
# Description: Load previous survey data from RACE_BASE (need to check w Lewis). Compare basic info about station number, boat number, station locations, and number of hauls
# Author: Megsie Siple


# 1. Libraries ------------------------------------------------------------
library(tidyverse)
library(patchwork)


# 2. Get processed haul data ----------------------------------------------
# Thank you Lewis
h <- readRDS(here::here("data", "processed", "AK_GOA_BTS_hauls.rds"))
head(h)
unique(h$YEAR)


# Does the HAUL column contain unique identifiers for each haul within a year?
h %>%
  group_by(YEAR, HAUL) %>%
  count()

h %>%
  group_by(YEAR, HAUL, STATIONID) %>%
  count()
# The answer is no! A lot of haul numbers appear in the data more than once. They show up a max of 3 times. I think this is the number of legs on a cruise.
# There are, for example, two "haul 3"s in the 1990 dataset

# Station ID corresponds to grid cell that the station is in (row X column)
# STATIONID x STRATUM = unique station ... some are sampled multiple times so look for hauls with performance >=0 to get the successful haul if there are multiple tries.

# EXAMPLE: 1990 survey
locations <- h %>%
  filter(YEAR == 1990) %>%
  distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE, HAULJOIN) %>%
  arrange(DATE, HAULJOIN) %>% # HAULJOIN to order sites by when they were visited (multiple sites/day)
  rowid_to_column("order")

loc_sf <- sf::st_as_sf(
  x = locations,
  coords = c("lon", "lat"),
  crs = 4326, agr = "constant"
)

locations %>%
  ggplot() +
  geom_sf(data = loc_sf) +
  geom_point(data = locations, aes(x = lon, y = lat, colour = order)) +
  scale_colour_viridis_c("Sampling order", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude")




# 3. Plot the order of the survey -----------------------------------------

get_map <- function(year, hauldata = h) {
  locs <- hauldata %>%
    filter(YEAR == year) %>%
    distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE, HAULJOIN, VESSEL) %>%
    arrange(DATE, HAULJOIN) %>%
    rowid_to_column("order")
  locs_sf <- sf::st_as_sf(
    x = locs,
    coords = c("lon", "lat"),
    crs = 4326, agr = "constant"
  )
  print(head(locs))
  mapplot <- locs %>%
    ggplot() +
    geom_sf(data = locs_sf) +
    geom_point(data = locs, aes(x = lon, y = lat, colour = order)) +
    scale_colour_viridis_c("Sampling order", direction = 1) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title = paste("Year = ", year)) +
    facet_wrap(~VESSEL, ncol = 1)
  
  return(mapplot)
}



# 4. Plot the distribution of distances to closest points------------------

get_dist <- function(year, hauldata = h) {
  locs <- hauldata %>%
    filter(YEAR == year) %>%
    distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE, HAULJOIN) %>%
    arrange(DATE, HAULJOIN) %>%
    rowid_to_column("order")
  locs_sf <- sf::st_as_sf(
    x = locs,
    coords = c("lon", "lat"),
    crs = 4326, agr = "constant"
  )
  distance_matrix_km <- matrix(as.numeric(sf::st_distance(locs_sf) / 1000),
                               nrow = nrow(locs)
  )
  print(nrow(locs))
  rownames(distance_matrix_km) <-
    colnames(distance_matrix_km) <-
    locs_sf$order
  
  distance_df <- as.data.frame(distance_matrix_km) %>%
    add_column(surveyId = colnames(distance_matrix_km)) %>%
    pivot_longer(cols = colnames(distance_matrix_km))
  
  nearest_neighbor <- distance_df %>%
    arrange(value) %>%
    group_by(surveyId) %>%
    summarize(
      nn1 = nth(value, n = 2), # min distance will always be 0
      nn2 = nth(value, n = 3)
    ) %>%
    add_column(year = year)
  
  return(nearest_neighbor)
}


get_dist(year = 1990)


plot_dist <- function(nearest_df) {
  # nearest_df is a dataframe with the columns 'surveyId', 'nn1', 'nn2'
  yrlab <- nearest_df$year[1]
  nndist <- nearest_df %>%
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
    ) +
    labs(title = paste0("Year = ", yrlab)) +
    theme(legend.position = "none") +
    xlim(c(0, 80))
  return(nndist)
}

plot_dist(nearest_df = get_dist(1990))
years_vec <- c(1990, 1993, 1996, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)

# * 4.1 combine distance data ---------------------------------------------
# This takes a minute
dists_allyrs <- years_vec %>%
  map_df(~ get_dist(.x))

load(here::here("data", "processed", "nearest_neighbors_optimal2.RData"))
dists_optimal <- nearest_neighbor


# Plot historical and surveys and optimized survey (entered as a future year)
dfcomp <- dists_optimal %>% 
  mutate(year = 2030) %>%
  bind_rows(dists_allyrs) %>%
  pivot_longer(cols=nn1:nn2) %>%
  mutate(yearcol = case_when(year<2020 ~ 'Historical',
                             year>2020 ~ 'Optimized'))

facet.labs <- c("Distance to closest station (km)", "Distance to second-closest station (km)")
names(facet.labs) <- c("nn1","nn2")

boxplotfig <- dfcomp %>% 
  ggplot(aes(x=year,y=value, group=year, colour = yearcol)) +
  geom_boxplot() +
  facet_wrap(~name, labeller = labeller(name = facet.labs)) +
  ggthemes::theme_base(base_size = 14) +
  xlab("Year") +
  ylab("Distance (km)") +
  scale_colour_brewer("", palette = "Paired") +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030),
                     labels = c(paste(c(1990, 2000, 2010, 2020)), "Optimized"))


# Save boxplots
png("figures/Boxplot1.png", width = 9, height = 5, units = "in", res = 200)
boxplotfig
dev.off()


# Test whether distance distributions are different -----------------------
ks.test(dists_allyrs$nn1, dists_optimal$nn1)
ks.test(dists_allyrs$nn2, dists_optimal$nn2)

# These tests are known to have tiny p-values even when the distributions look very similar. So not sure how helpful this is.
wilcox.test(dists_allyrs$nn1, dists_optimal$nn1)
wilcox.test(dists_allyrs$nn2, dists_optimal$nn2)



# 5. Save figures ---------------------------------------------------------

# Side by side maps and histograms
# for (i in 1:length(years_vec)) {
#   print(i)
#   unique_year <- years_vec[i]
#   map1 <- get_map(year = unique_year)
#   dist1 <- get_dist(year = unique_year)
#   plot1 <- plot_dist(nearest_df = dist1)
#   map1 + dist1 + plot_layout(ncol = 2, widths = c(2, 1))
# }


# Get info for each of the previous years' cruises. How many boats? How many stations? Total number of survey days?

histtable <- h %>%
  group_by(YEAR) %>%
  summarize(
    ndays = max(DATE) - min(DATE),
    nboats = length(unique(VESSEL)),
    nstations = length(unique(STATIONID)),
    nhauls = length(unique(HAULJOIN))
  )

histtable

write.table(
  histtable,
  file = "HistoricalHauls.tbl"
)



# 6. Get distance matrices etc for historical surveys ---------------------
# Each vessel gets one distance matrix for all the sites it visited
source(here::here("code", "get_distances.R"))

# list of distance matrices for a single year
histtable <- histtable %>% add_column(
  cumudistboat1 = NA,
  cumudistboat2 = NA,
  cumudistboat3 = NA,
  cumudistboat4 = NA,
  maxdistboat1 = NA,
  maxdistboat2 = NA,
  maxdistboat3 = NA,
  maxdistboat4 = NA
)


for (i in 1:nrow(histtable)) {
  year <- years_vec[i]
  yeardat <- h %>% filter(YEAR == year)
  distance_list <- yeardat %>%
    group_by(VESSEL) %>%
    group_split() %>%
    map(.f = get_distances)
  histtable$cumudistboat1[i] <- distance_list[[1]]$cumu_distance
  histtable$cumudistboat2[i] <- distance_list[[2]]$cumu_distance
  if (length(distance_list) == 3) {
    histtable$cumudistboat3[i] <- distance_list[[3]]$cumu_distance
  }
  if (length(distance_list) == 4) {
    histtable$cumudistboat4[i] <- distance_list[[4]]$cumu_distance
  }
  # max distances
  histtable$maxdistboat1[i] <- distance_list[[1]]$max_distance
  histtable$maxdistboat2[i] <- distance_list[[2]]$max_distance
  if (length(distance_list) >= 3) {
    histtable$maxdistboat3[i] <- distance_list[[3]]$max_distance
  }
  if (length(distance_list) == 4) {
    histtable$maxdistboat4[i] <- distance_list[[4]]$max_distance
  }
}

write.csv(histtable, file = "historical2.csv", row.names = FALSE)

# Summarize historical surveys for tech memo
histtable %>% 
  filter(nboats==2) %>% 
  select(YEAR, ndays, nstations, nhauls, cumudistboat1,cumudistboat2) %>%
  pivot_longer(cumudistboat1:cumudistboat2) %>%
  group_by(name) %>%
  summarize(mean(value), sd(value))


# 7. Apply new method for station order to historical data ----------------
# This is an attempt to make the new survey design more comparable to the historical ones-- apply the two methods to the sets of stations in the old survey designs and compare the distances between them.
yrs_to_compare <- histtable %>%
  filter(nboats <= 3) %>%
  select(YEAR) %>%
  deframe()

# Look thru yrs
full_stat <- vector()

for (y in 1:length(yrs_to_compare)) { # this takes a while!
  
  yr_selection <- yrs_to_compare[y]
  h_yr <- h %>% filter(YEAR == yr_selection)
  
  depth_quants <- quantile(h_yr$BOTTOM_DEPTH)
  
  h_yr <- h_yr %>%
    # assign stations to boats by depth:
    mutate(whichboat = case_when(
      BOTTOM_DEPTH < depth_quants["25%"] ~ 1,
      BOTTOM_DEPTH > depth_quants["25%"] &
        BOTTOM_DEPTH < depth_quants["75%"] ~ 2,
      BOTTOM_DEPTH > depth_quants["75%"] ~ 3
    ))
  
  # A lot of this is code from sample_stations.R:
  nboats <- length(unique(h_yr$VESSEL))
  
  if (nboats == 2) {
    print("2-boat solution")
    nperboat <- nrow(h_yr) / 2
    if (!is.integer(nperboat)) {
      n1 <- floor(nperboat)
      n2 <- ceiling(nperboat)
      n3 <- 0
    } else {
      (n1 <- n2 <- nperboat)
      n3 <- 0
    }
    bb <- c(rep(1, times = n1), rep(2, times = n2))
    h_yr <- h_yr %>%
      # assign stations to boats by depth:
      mutate(whichboat = ifelse(BOTTOM_DEPTH <= depth_quants["50%"], 1, 2)) %>%
      mutate(Id = 1:nrow(h_yr))
    # assign stations to boats randomly:
    # mutate(whichboat = sample(x = bb, size = n1 + n2,replace = FALSE))
  }
  
  if (nboats == 3) {
    print("3-boat solution")
    nperboat <- nrow(h_yr) / 3
    if (!is.integer(nperboat)) {
      n1 <- floor(nperboat)
      n2 <- ceiling(nperboat)
      n3 <- nrow(h_yr) - (n1 + n2)
    } else {
      (n1 <- n2 <- n3 <- nperboat)
    }
    bb <- c(
      rep(1, times = n1),
      rep(2, times = n2),
      rep(3, times = n3)
    )
    h_yr <- h_yr %>%
      # assign stations to boats by depth:
      mutate(whichboat = case_when(
        BOTTOM_DEPTH < depth_quants["25%"] ~ 1,
        BOTTOM_DEPTH >= depth_quants["25%"] &
          BOTTOM_DEPTH < depth_quants["75%"] ~ 2,
        BOTTOM_DEPTH >= depth_quants["75%"] ~ 3
      )) %>%
      mutate(Id = 1:nrow(h_yr))
    # assign boats randomly:
    # mutate(whichboat = sample(x = bb, size = n1 + n2 + n3,
    #                           replace = FALSE)) #randomly assign stations to boats 1 and 2
  }
  
  h_yr_sf <- sf::st_as_sf(
    x = h_yr,
    coords = c("lon", "lat"),
    crs = 4326, agr = "constant"
  )
  
  # Pairwise distances between survey points in km
  # st_distance() provides distances in m
  distance_matrix_km <- matrix(as.numeric(st_distance(h_yr_sf) / 1000),
                               nrow = nrow(h_yr_sf)
  )
  
  rownames(distance_matrix_km) <-
    colnames(distance_matrix_km) <-
    h_yr_sf$Id
  
  distance_df <- as.data.frame(distance_matrix_km) %>%
    add_column(surveyId = colnames(distance_matrix_km)) %>%
    pivot_longer(cols = colnames(distance_matrix_km))
  
  western_end <- h_yr %>%
    arrange(lon) %>%
    slice(1)
  
  source(here::here("code", "stationdecisions", "get_next_station_1.R"))
  
  test.id <- as.character(h_yr$Id[1])
  
  df_list <- stat_list <- list()
  
  
  for (b in 1:nboats) {
    # Boat 1
    h_yr_boat <- h_yr %>%
      filter(whichboat == b)
    
    western_end <- h_yr_boat %>%
      arrange(lon) %>%
      slice(1)
    
    sample_size <- nrow(h_yr_boat)
    boat_plan <- rep(NA, times = sample_size)
    boat_plan[1] <- western_end$Id
    
    for (i in 2:length(boat_plan)) {
      boat_plan[i] <- get_next_station_1(
        stationId = boat_plan[i - 1],
        already_sampled = boat_plan[1:i],
        depths = h_yr_boat[, c("Id", "BOTTOM_DEPTH")],
        longs = h_yr_boat[, c("Id", "lon")]
      )
    }
    
    d1 <- data.frame(Id = boat_plan, nwd_order = 1:sample_size)
    
    d2 <- h_yr_boat %>%
      mutate(Id = as.character(Id)) %>%
      right_join(d1, by = "Id")
    
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
    
    stat_list[[b]] <- data.frame(max_surv_dist = max(d3$cumu_distance),
                                 max_station_dist = max(d3$distance_from_prev),
                                 boat = b,
                                 year = yr_selection)
    
    df_list[[b]] <- d3
  }
  
  length(df_list)
  df_both <- do.call(rbind.data.frame, df_list)
  stat_both <- do.call(rbind.data.frame, stat_list)
  
  full_stat <- rbind(full_stat, stat_both)
} #/year loop for historical data


# Add the distances from the optimized design - these will vary w/ each realization (see sample_stations_sims.R for simulations).

# This info is contained in df_list from sample_stations.R
full_stat2 <- full_stat %>%
  add_row(
    max_surv_dist = 13337.41,
    max_station_dist = 314,
    year = 3000,
    boat = 1
  ) %>%
  add_row(
    max_surv_dist = 14495.82,
    max_station_dist = 236,
    year = 3000,
    boat = 2
  )

png("Distance_Comparison_all.png")
full_stat2 %>%
  ggplot(aes(x = max_surv_dist, y = max_station_dist, colour = year)) +
  geom_point(size = 3) +
  xlab("Maximum total distance per boat (km)") +
  ylab("Max distance between stations (km)") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
dev.off()


png("Distance_Comparison_2boats.png")
full_stat2 %>%
  filter(year %in% c(2001, 2011, 2013, 2017, 2019, 3000)) %>%
  ggplot(aes(x = max_surv_dist, y = max_station_dist, colour = year)) +
  geom_point(size = 3) +
  xlab("Maximum total distance per boat (km)") +
  ylab("Max distance between stations (km)") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none")
dev.off()

# Make the table for the tech memo
wdf <-  full_stat2 %>%
  pivot_wider(names_from = boat,
              values_from = max_surv_dist:max_station_dist) %>%
  dplyr::select(year, max_surv_dist_1:max_surv_dist_3)

fulltable <- histtable %>%
  dplyr::select(YEAR, nstations,
                cumudistboat1,cumudistboat2,cumudistboat3) %>% 
  right_join(wdf, by = c("YEAR" = "year")) %>%
  as.data.frame()

  

write.csv(fulltable, "tables/TechMemoTable.csv", row.names = FALSE)
