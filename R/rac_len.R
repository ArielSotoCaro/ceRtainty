#' Define the length of the Risk Aversion Coefficient (RAC) vector.
#'
#' @param ini The initial value of the RAC sequence.
#' @param fin The final value of the RAC sequence.
#' @param data Original data, could be a vector or a matrix.
#'
#' @return Two elements are generated: \code{r} is the RAC vector, and \code{length} is
#' a scalar with the number of elements on RAC vector.

rac_len <- function(ini,fin,data){

  if(length(ini) != 1L | length(fin) != 1L ){
    stop("\nInitial and final values should be scalars.\n")
  }


  len <- nrow(as.matrix(data))
  x   <- rac_seq(ini , fin , len)

  return(list(r      = x,
              lenght = len)
  )
}
