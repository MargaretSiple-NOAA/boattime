#' Get distances between survey points
#'
#' @param sf_obj An sf object of all the survey stations visited by a given boat
#'
#' @return a dataframe with the distances (in km) between each pair of points
#' @export
#'
#' @examples

library(tidyverse)

get_distances <- function(sf_obj){
  # 
  distance_matrix_km <- matrix(as.numeric(sf::st_distance(sf_obj) / 1000),
                               nrow = nrow(sf_obj))
  rownames(distance_matrix_km) <-
    colnames(distance_matrix_km) <-
    survey_sf$Id
  distance_df <- as.data.frame(distance_matrix_km) %>%
    add_column(surveyId = colnames(distance_matrix_km)) %>%
    pivot_longer(cols = colnames(distance_matrix_km))
  return(distance_df)
}