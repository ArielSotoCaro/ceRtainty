#' RAC Generator function.
#'
#' @export
#'
#' @param ini The initial value of the risk aversion coefficient (RAC) sequence
#' @param fin The final value of the risk aversion coefficient (RAC) sequence
#' @param data \code{data.frame} object to weight the RAC
#'
#' @return Produce a single vector of adjusted RACs.
#'
#' @details Create a vector with the adjusted relative risk aversion
#' coefficients to be used in the CE computation, under Power utility
#' function.
#'
#' @examples
#' # Example
#' data("profitSWG")
#' rac_generator(data = profitSWG$control, ini = 0.5, fin = 4.0)

rac_generator <- function(data,ini,fin){

  if(length(ini) != 1L | length(fin) != 1L){
    stop("\nValues at 'ini' and 'fin' must be scalars.\n")

  }

  ini <- ini/mean(sapply(data,mean), na.rm = T)
  fin <- fin/mean(sapply(data,mean), na.rm = T)


  eg <- rac_len(ini = ini , fin = fin , data = data)

  name_treat <- names(data)

  eg_lst  <- list()
  eg_vec  <- matrix(0,eg$lenght,1)
  ncol    <- ncol(as.matrix(data))
  data_m  <- as.matrix(data)

  for(i in 1:ncol){

    for(j in 1:eg$lenght){

      eg_vec[j] <- RACa(rac  = eg$r[j],
                        data = data_m[,i]
      )

    }

    eg_lst[[i]] <- eg_vec

  }

  rac        <- as.data.frame(eg_lst)
  names(rac) <- name_treat

  return(rac)
}
