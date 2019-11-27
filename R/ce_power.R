#' Certainty Equivalent Computation using Power Utility Function
#'
#' @param data \code{data.frame} with profit values
#' @param rac_ini Initial value for the RAC sequence
#' @param rac_fin Final value for the RAC sequence
#' @param weight Original wealth
#'
#' @return Generate three objects: A table with the CEs, a vector
#' of risk aversion coefficients RAC, and a plot to compare the CEs.
#'

ce_power <- function(data,rac_ini,rac_fin,weight=0){

  names_treatments <- names(data)
  n_treatments     <- ncol(as.matrix(data))

  RAC <- rac_generator(ini  = rac_ini,
                       fin  = rac_fin,
                       data = data)
  n   <- nrow(data)

  ce_lst <- list()

  if(n_treatments == 1){

    ce     <- ce_p(profit = data,
                   rac    = unlist(RAC[1,1]),
                   weight = weight)
    ce_lst <- ce

  }else if(n_treatments > 1){

    for(t in 1:n_treatments){

      ce <- matrix(0,n,1)

      for(h in 1:n){

        ce[h] <- ce_p(profit = data[,t],
                      rac    = RAC[h,t],
                      weight = weight)

      }

      ce_lst[[t]] <- ce

    }

  }

  cePower        <- as.data.frame(ce_lst)
  names(cePower) <- names_treatments


  invisible(list(CE_table = cePower,
                 RRAC     = RAC))



}
