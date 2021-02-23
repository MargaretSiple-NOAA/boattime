# Survey timing for Gulf of Alaska survey design
***
## Overview
This repository contains code for measuring distances between survey points and approximating the time it will take to complete a particular survey design. 

This repository uses outputs from Zack's Optimal Allocation repository: https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA

I keep a local clone of this repo in the same directory that contains this project folder, so to replicate it you need a clone of this directory (`boattime/`) and a clone of the optimization code (`Optimal_Allocation_GoA/`) in the same folder (or just change the directories).

### File structure
| Folder           | Contents|
| -------------    |:-------------|
| `code/`          | Code for processing data, sampling from survey designs, and calculating                       distances |
| `data/`          | Raw and processed historical data from previous cruises      |
| `figures/`  | All figures, tech memo and otherwise      |

*** 

## Script overview
stationdecisions/ : contains functions for determining what station should be sampled "next"

sample_stations.R : get survey order based on decision rules and make plots

historical.R : load historical cruise data for comparison

Process_AK_BTS_data_all_hauls_megsie.R : process raw historical haul data (from Lewis)

## Input
This code is intended to work with any survey design (optimized or historical). For the optimized survey design part, this script draws from Zack's optimized GoA survey design, which is the full domain spatiotemporal optimization ([Optimal_Allocation_GoA-master](https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA) -- make sure you have an updated copy of this dataset for running the code). 


## Analysis flow
### Determine which stations to sample
This script samples randomly from the optimized survey design, randomly selecting stations from each stratum based on the optimal allocation. Depth and Lat/Long are given in the Extrapolation_depths.RData file, which contains a 5-km resolution survey grid with N = 22832 cells. There is more information about the Extrapolation_depths dataframe [here](https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA#input-data----spatial-domain). 

### Determine sampling order
These include (so far):

1. **Proximity and depth** (get_next_station_1.R) Between closest and furthest west unsampled station, pick the deeper one
2. **Traveling Salesperson Problem** (tsp.R) Stations are sampled to minimize the total distance traveled, starting at the westernmost station
3. **Proximity and depth, split boats by depth** (get_next_station_1.R, with boats assigned to stations by depth before the station order is determined) Boats are split by depth: if it's a two-boat survey, one boat visits all sites below the median bottom depth, the other visits all sites above the median bottom depth. If there are three boats, the stations are split up into <25th, 25-50th, and >50th depth quantiles, with each boat visiting one depth range. Then each boat visits stations according to Method 1.

### Examine distances between stations
The total distance traveled for a survey should be comparable between a given optimized design and the historical surveys. The sample_stations.R script calculates the total distance traveled for a given order of locations, based on a distance matrix between all stations. 

### Comparing historical surveys to the new design
Because the historical surveys chose stations based on many more factors than we can consider for a hypothetical future survey (weather, boat technical issues, decision to "lane" boats or not), I also used the survey points from historical surveys as if they were new:

1. Get the map of stations visited by each boat in a given year

2. Treat this as a candidate design, feeding the coordinates into the same "station choice" algorithm used for the optimized design. 

### Figures
The figures generated in this script are saved in the figures/ directory. They include: 
1) Maps of the Gulf with the whole field, plus all the sites that have been sampled in a given allocation, and 
2) The distribution of distances from each point to its nearest neighbor and second-nearest neighbor
3) Comparison of max inter-station distance and max cumulative distance between historical and optimized survey designs, using the same method for both.