lengthen_pal <- function(x=1:10,shortpal){
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}