# get_next_station_1
# Survey design 1: Pick stations based on proximity, depth, west-to-east --------
# After completing each survey station, three decision rules:
# 1) which station is closest and unsampled?
# 2) which station is the furthest west unsampled station? (i.e., don't skip over sites going west to east)
# 3) is one of the above 2 deeper than the other? If so, pick the deepest station (to prioritize deeper, longer trawls first per Ned & Wayne)

get_next_station_1 <- function(stationId = test.id,
                               already_sampled = "636174",
                               distances = distance_df,
                               depths = Extrapolation_depths[sample_vec,c('Id','DEPTH_EFH')], 
                               longs = Extrapolation_depths[sample_vec,c('Id','Lon')]){
  closest <- distances %>% 
    filter(surveyId == stationId) %>%
    filter(!name %in% already_sampled) %>%
    filter(value>0) %>%
    slice_min(value) %>%
    dplyr::select(name) %>%
    as.character()
  
  furthest_w_unsampled <- longs %>%
    filter(Id != stationId) %>%
    filter(!Id %in% already_sampled) %>%
    slice_min(Lon) %>%
    dplyr::select(Id) %>%
    as.character()
  
  if(closest == furthest_w_unsampled){
    selection = closest} else{
      depth1 <- depths %>% 
        filter(Id == closest) %>% 
        dplyr::select(DEPTH_EFH)
      depth2 <- depths %>% 
        filter(Id == furthest_w_unsampled) %>% 
        dplyr::select(DEPTH_EFH)
      
      ind <- which.min(c(depth1,depth2))
      selection <- c(closest,furthest_w_unsampled)[ind]
    }
  return(selection)
}
