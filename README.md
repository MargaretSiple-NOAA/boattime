# Timing and spacing for Gulf of Alaska survey design
***
## Overview
This repository contains code for measuring distances between survey points and approximating the time it will take to complete a particular survey design. 

This repository uses outputs from Zack's Optimal Allocation repository: https://github.com/zoyafuso-NOAA/Optimal_Allocation_GoA

*** 
## Script overview
stationdecisions/ : contains functions for determining what station should be sampled "next" once you're at a given station. These include (so far):
1. *Proximity and depth* (get_next_station_1.R) Between closest and furthest west unsampled station, pick the deeper one
2. *Traveling Salesperson Problem* (tsp.R) Stations are sampled to minimize the total distance traveled, starting at the westernmost station

## 

