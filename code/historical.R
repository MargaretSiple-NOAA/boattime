# Want to compare 



# Get processed haul data -------------------------------------------------
# Thank you Lewis
# G:\GOA\GOA 2019\DATA_2019\Ocean Explorer\Leg 4\Globe\Tracks\
h <- readRDS(here::here("data","processed","AK_GOA_BTS_hauls.rds"))

library(tidyverse)
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

# Do the station IDs correspond to different Lat/Longs?
# Per Ned: station ID corresponds to grid cell that the station is in (row X column)
# STATIONID x STRATUM = unique station ... some are sampled multiple times so look for hauls with performance >=0 to get the successful haul if there are multiple tries.
locations <- h %>%
  filter(YEAR == 1990) %>%
  distinct(STATIONID, lat, lon, END_LATITUDE, END_LONGITUDE, HAUL, DATE)

