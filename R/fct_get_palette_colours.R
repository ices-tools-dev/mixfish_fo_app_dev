#' get_palette_colours 
#'
#' @description A function for getting palette colours for an input variable
#' 
#' @param palette Character. The name of the palette to be used
#' 
#' @param plot_variable vector. 
#'
#' @return a vector of colours, equal in length to the number of unique values in plot_variable
#'

get_palette_colours <- function(palette, plot_variable){
  
  stopifnot(length(palette) == 1, "argument 'palette' should be of length 1")
  
  palette_min_lookup <- data.frame(pal = c("spectral", "paired", "cols25", "set1", "set2", "set3"),
                                   fun = c("brewer.spectral", "brewer.paired", "cols25", "brewer.set1", "brewer.set2", "brewer.set3"),
                                   min = c(3, 3, 1, 3, 3, 3))
  selected_pal <- palette_min_lookup[palette_min_lookup['pal'] == palette,]

  var <- unique(plot_variable)
  n_colours <- length(var)

  if (n_colours < selected_pal$min){
  pal <- do.call(selected_pal$fun, args = list(n = selected_pal$min))[1:n_colours]  
} else {
  pal <- do.call(selected_pal$fun, args = list(n = n_colours))
  }
  
}
