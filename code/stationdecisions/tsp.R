# Traveling salesperson option
# By Megsie Siple (it was Zack Oyafuso's idea)

# Libraries ---------------------------------------------------------------
library(TSP)

# Load distance matrix in km ----------------------------------------------
get_tsp_soln <- function(x){
  # x is a distance matrix
  tsp <- TSP(x)
  tour <- solve_TSP(x = tsp, control = "nearest_insertion")
  tour_df <- data.frame(site = names(tour),tsp_order = as.numeric(tour))
  return(tour_df)
}
