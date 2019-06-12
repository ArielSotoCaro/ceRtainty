#' RAC Sequence generator
#'
#' @param ini The initial value for the RAC
#' @param fin The final value for the RAC
#' @param len The Length of the vector to creates
#'
#' @return Vector of RACs

rac_seq <- function(ini,fin,len){

  if(length(ini) != 1L | length(fin) != 1L | length(len) != 1L){
    stop("\nAll parameters should be scalars.\n")

  }

  rac_vector <- seq(ini , fin , length.out = len)

  return(rac_vector)
}
