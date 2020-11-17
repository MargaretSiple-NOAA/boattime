# Timing and spacing for Gulf of Alaska survey design
***
## Overview
This repository contains code for measuring distances between survey points and approximating the time it will take to complete a particular survey design. 

This repository uses outputs from Zack's Optimal Allocation repository: https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA

*** 
## Script overview
stationdecisions/ : contains functions for determining what station should be sampled "next"

sample_stations.R : get survey order based on decision rules and make plots
historical.R : load historical cruise data for comparison


## Input
This code is intended to work with any survey design (optimized or historical). For the optimized survey design part, this script draws from Zack's optimized GoA survey design, which is the full domain spatiotemporal optimization from model 11 ([Optimal_Allocation_GoA-master](https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA)). 


## Analysis flow
### Determine which stations to sample
This script samples randomly from the optimized survey design, randomly selecting stations from each stratum based on the optimal allocation. Depth and Lat/Long are given in the Extrapolation_depths.RData file, which contains a 5-km resolution survey grid with N = 22832 cells. There is more information about the Extrapolation_depths dataframe [here](https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA#input-data----spatial-domain). 

### Station decisions
These include (so far):

1. **Proximity and depth** (get_next_station_1.R) Between closest and furthest west unsampled station, pick the deeper one
2. **Traveling Salesperson Problem** (tsp.R) Stations are sampled to minimize the total distance traveled, starting at the westernmost station


### Distances
The total distance traveled for a survey should be comparable between a given optimized design and the existing surveys. The sample_stations.R script gets a distance matrix for all the locations, and calculates the total distance traveled for a given order of locations. 

### Figures
The figures generated in this script are saved in the figures/ directory. They include: 
1) Maps of the Gulf with the whole field, plus all the sites that have been sampled in a given allocation, and 2) The distribution of distances from each point to its nearest neighbor and second-nearest neighbor.