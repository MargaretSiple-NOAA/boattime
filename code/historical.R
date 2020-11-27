# Project:  Load and compare historical surveys ---------------------------
# Description: Load previous survey data from RACE_BASE (need to check w Lewis). Compare basic info about station number, boat number, station locations, and number of hauls
# Author: Megsie Siple


# 1. Libraries ------------------------------------------------------------
library(tidyverse)
library(patchwork)


# 2. Get processed haul data ----------------------------------------------
# Thank you Lewis
h <- readRDS(here::here("data","processed","AK_GOA_BTS_hauls.rds"))
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

loc_sf <- sf::st_as_sf(x = locations,
                       coords = c("lon","lat"),
                       crs = 4326, agr = "constant")

locations %>%
  ggplot() +
  geom_sf(data = loc_sf) + 
  geom_point(data = locations, aes(x = lon, y = lat, colour = order)) +
  scale_colour_viridis_c("Sampling order", direction = 1) +
  xlab("Longitude") +
  ylab("Latitude")



# 3. Plot the order of the survey -----------------------------------------

get_map <- function(year, hauldata = h){
  locs <- hauldata %>%
    filter(YEAR == year) %>% 
    distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE, HAULJOIN) %>%
    arrange(DATE, HAULJOIN) %>%
    rowid_to_column("order")
  locs_sf <- sf::st_as_sf(x = locs,
                          coords = c("lon","lat"),
                          crs = 4326, agr = "constant")
  mapplot <- locs %>%
    ggplot() +
    geom_sf(data = locs_sf) + 
    geom_point(data = locs, aes(x = lon, y = lat, colour = order)) +
    scale_colour_viridis_c("Sampling order", direction = 1) +
    xlab("Longitude") +
    ylab("Latitude") +
    labs(title = paste("Year = ",year))
  
  return(mapplot)
}

#get_map(year = 1990)


# 4. Plot the distribution of distances to closest points------------------

get_dist <- function(year, hauldata = h){
  locs <- hauldata %>%
    filter(YEAR == year) %>% 
    distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE, HAULJOIN) %>%
    arrange(DATE, HAULJOIN) %>%
    rowid_to_column("order")
  locs_sf <- sf::st_as_sf(x = locs,
                          coords = c("lon","lat"),
                          crs = 4326, agr = "constant")
  distance_matrix_km <- matrix(as.numeric(sf::st_distance(locs_sf) / 1000),
                               nrow = nrow(locs)) 
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
    summarize(nn1 = nth(value,n = 2), # min distance will always be 0
              nn2 = nth(value,n = 3))
  nnlabs <- c("Nearest neighbor","Second-nearest neighbor")
  names(nnlabs) <- c("nn1","nn2")
  
  nndist <- nearest_neighbor %>%
    pivot_longer(cols = nn1:nn2) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_histogram(alpha = 0.2, binwidth = 5, position = "identity") + 
    xlab("Distance (km)") +
    ylab("Frequency") +
    scale_fill_discrete("",
                        labels = c("Nearest station",
                                   "Second-nearest station")) +
    labs(title = paste0("Year = ",year)) +
    theme(legend.position = "none") +
    xlim(c(0, 80))
  return(nndist)
}


get_dist(year = 1990)

years_vec <- c(1990, 1993, 1996, 1999, 2001, 2003, 2005, 2007, 2009, 2011, 2013, 2015, 2017, 2019)


# 5. Save figures ---------------------------------------------------------

for(i in 1:length(years_vec)){
  print(i)
  unique_year = years_vec[i]
  map1 <- get_map(year = unique_year)
  dist1 <- get_dist(year = unique_year)
  #png(filename = here::here("figures",paste0("Year_",unique_year,".png")),width = 10, height = 6, units = 'in',res = 150)
  map1 + dist1 + plot_layout(ncol = 2, widths = c(2,1))
  #dev.off()
}  

plots <- list()
for(i in 1:length(years_vec)){
  print(i)
  unique_year = years_vec[i]
  dist1 <-  get_dist(year = unique_year)
  plots[[i]] <- dist1
}

# This part takes a while:
png(filename = here::here("figures","DistanceDistributions.png"),
    width = 12, height = 10,
    units = 'in',res = 150)
wrap_plots(plots)
dev.off()

# Get info for each of the previous years' cruises. How many boats? How many stations? Total number of survey days?

histtable <- h %>% group_by(YEAR) %>%
  summarize(ndays = max(DATE)-min(DATE),
            nboats = length(unique(VESSEL)),
            nstations = length(unique(STATIONID)),
            nhauls = length(unique(HAULJOIN))) 
histtable


# 6. Get distance matrices etc for historical surveys ---------------------
#Each vessel gets one distance matrix for all the sites it visited
source(here::here("code", "get_distances.R"))

# list of distance matrices for a single year
year <- years_vec[1]
yeardat <- h %>% filter(YEAR==year)

distance_list <- yeardat %>% 
  group_by(VESSEL) %>% 
  group_split() %>%
  map( .f = get_distances)
