#' Get distances between survey points
#'
#' @param survey_df A dataframe of sampling locations with Lon and Lat. 
#' @details If the dataframe is from a historical dataset, Id has to be added and lat/lon converted to title case. If it's from an optimized survey design, Id is the unique identifier for that location.
#' @return a dataframe with the distances (in km) between each pair of points
#' @export
#'
#' @examples

library(tidyverse)

get_distances <- function(survey_df){
  # Add Id if the survey data does not contain one (i.e., if it's a historical dataset)
  if(!"Id" %in% names(survey_df)){
    print("This is a historical dataset; Id column has been added and lat/lon columns have been capitalized");
    survey_df <- survey_df %>% 
      rename(Lon = "lon", Lat = "lat") %>%
      mutate(Id = 1:nrow(survey_df))
    }
  
  survey_sf <- sf::st_as_sf(
    x = survey_df,
    coords = c("Lon", "Lat"),
    crs = 4326, agr = "constant"
  )
  
  distance_matrix_km <- matrix(as.numeric(sf::st_distance(survey_sf) / 1000),
                               nrow = nrow(survey_sf))
  rownames(distance_matrix_km) <-
    colnames(distance_matrix_km) <-
    survey_sf$Id
  distance_df <- as.data.frame(distance_matrix_km) %>%
    add_column(surveyId = colnames(distance_matrix_km)) %>%
    pivot_longer(cols = colnames(distance_matrix_km))
  return(distance_df)
}

