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
              nn2 = nth(value,n = 3)) %>%
    add_column(year = year)
  
  return(nearest_neighbor)
}


get_dist(year = 1990)


plot_dist <- function(nearest_df){
  # nearest_df is a dataframe with the columns 'surveyId', 'nn1', 'nn2'
  yrlab <- nearest_df$year[1]
  nndist <- nearest_df %>%
    pivot_longer(cols = nn1:nn2) %>%
    ggplot(aes(x = value, fill = name)) +
    geom_histogram(alpha = 0.2, binwidth = 5, position = "identity") + 
    xlab("Distance (km)") +
    ylab("Frequency") +
    scale_fill_discrete("",
                        labels = c("Nearest station",
                                   "Second-nearest station")) +
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

load(here::here("data","processed","nearest_neighbors_optimal.RData"))
dists_optimal <- nearest_neighbor

nlabs <- c(nn1 = "Nearest neighbor", nn2 = "Second-nearest neighbor")

b1 <- dists_allyrs %>% 
  pivot_longer(cols = nn1:nn2) %>%
  filter(name == "nn1") %>%
  ggplot(aes(x=year, y = value, group = year)) +
  geom_boxplot() +
  ylab("Distance (km)") +
  facet_wrap(~name, labeller = labeller(name = nlabs))

b2 <- dists_allyrs %>% 
  pivot_longer(cols = nn1:nn2) %>%
  filter(name == "nn2") %>%
  ggplot(aes(x=year, y = value, group = year)) +
  geom_boxplot() +
  ylab("Distance (km)") +
  facet_wrap(~name, labeller = labeller(name = nlabs))


o1 <- dists_optimal %>%
  pivot_longer(cols = nn1:nn2) %>%
  filter(name == "nn1") %>%
  ggplot(aes(x=year, y = value, group = year)) +
  geom_boxplot(color = 'blue') +
  xlab("") + 
  ylab("") +
  facet_wrap(~name, labeller = labeller(name = nlabs)) +
  theme(axis.text.y = element_blank())

o2 <- dists_optimal %>%
  pivot_longer(cols = nn1:nn2) %>%
  filter(name == "nn2") %>%
  ggplot(aes(x=year, y = value, group = year)) +
  geom_boxplot(color = 'blue') +
  xlab("") + 
  ylab("") +
  facet_wrap(~name, labeller = labeller(name = nlabs)) +
  theme(axis.text.y = element_blank())


# patchwork
combined <- b1 + o1 + b2 + o2

# Set y limits to same among patchwork combos
p_ranges_y <- c(ggplot_build(combined[[1]])$layout$panel_scales_y[[1]]$range$range,
                ggplot_build(combined[[2]])$layout$panel_scales_y[[1]]$range$range)


# Save boxplots
png("figures/Boxplot1.png",width = 8, height = 5, units = 'in', res = 200)

combined + plot_layout(nrow = 1, widths = c(7,1,7,1)) & 
  ylim(min(p_ranges_y), max(p_ranges_y)) 

dev.off()


# 5. Save figures ---------------------------------------------------------

# Side by side maps and histograms
for(i in 1:length(years_vec)){
  print(i)
  unique_year <- years_vec[i]
  map1 <- get_map(year = unique_year)
  dist1 <- get_dist(year = unique_year)
  plot1 <- plot_dist(nearest_df = dist1)
  map1 + dist1 + plot_layout(ncol = 2, widths = c(2,1))
}


# Get info for each of the previous years' cruises. How many boats? How many stations? Total number of survey days?

histtable <- h %>% group_by(YEAR) %>%
  summarize(ndays = max(DATE)-min(DATE),
            nboats = length(unique(VESSEL)),
            nstations = length(unique(STATIONID)),
            nhauls = length(unique(HAULJOIN))) 

histtable

write.table(
  histtable, file = "HistoricalHauls.tbl"
)



# 6. Get distance matrices etc for historical surveys ---------------------
#Each vessel gets one distance matrix for all the sites it visited
source(here::here("code", "get_distances.R"))

# list of distance matrices for a single year
histtable <- histtable %>% add_column(cumudistboat1 = NA,
                                      cumudistboat2 = NA,
                                      cumudistboat3 = NA,
                                      cumudistboat4 = NA,
                                      maxdistboat1 = NA,
                                      maxdistboat2 = NA,
                                      maxdistboat3 = NA,
                                      maxdistboat4 = NA)


for(i in 1:nrow(histtable)){
  year <- years_vec[i]
  yeardat <- h %>% filter(YEAR==year)
  distance_list <- yeardat %>% 
    group_by(VESSEL) %>% 
    group_split() %>%
    map( .f = get_distances)
  histtable$cumudistboat1[i] <- distance_list[[1]]$cumu_distance
  histtable$cumudistboat2[i] <- distance_list[[2]]$cumu_distance
  if(length(distance_list)==3){
    histtable$cumudistboat3[i] <- distance_list[[3]]$cumu_distance
  }
  if(length(distance_list)==4){
    histtable$cumudistboat4[i] <- distance_list[[4]]$cumu_distance
  }
  
  histtable$maxdistboat1[i] <- distance_list[[1]]$max_distance
  histtable$maxdistboat2[i] <- distance_list[[2]]$max_distance
  if(length(distance_list)>=3){
    histtable$maxdistboat3[i] <- distance_list[[3]]$max_distance
  }
  if(length(distance_list)==4){
    histtable$maxdistboat4[i] <- distance_list[[4]]$max_distance
  }
}

write.csv(histtable,file = "historical2.csv",row.names = FALSE)
