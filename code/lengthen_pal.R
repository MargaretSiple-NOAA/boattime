#' Title
#'
#' @param x a vector of 1:n, where n is the number of unique colors you want
#' @param shortpal a short color palette, e.g., one from RColorBrewer()
#'
#' @return a vector of the same length as x, with n unique colors
#' @export
#'
#' @examples
lengthen_pal <- function(x = 1:10, shortpal) {
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}