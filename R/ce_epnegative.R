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

ce_epnegative <- function(data,rac_ini,rac_fin,weight=0){

  names_treatments <- names(data)

  if(rac_ini == rac_fin){

    RAC <- rac_ini

  }else{

    RAC <- rac_len(ini  = rac_ini,
                   fin  = rac_fin,
                   data = data)

  }


  data         <- as.matrix(data)
  n            <- nrow(data)
  n_treatments <- ncol(data)

  ce_lst <- list()

  if(n_treatments == 1){

    ce       <- matrix(0,n,1)
    ce       <- ce_en(profit = data,
                      rac    = unlist(RAC),
                      weight = weight)
    ceExpNeg <- ce

    plot_CE_epneg <- function(){plot_ce_en(ceExpNeg,
                                           unlist(RAC))}

    invisible(list(CE_table = ceExpNeg,
                   CE_plot  = plot_CE_epneg,
                   ARAC     = RAC))

  }else{
    for(t in 1:n_treatments){

      ce <- matrix(0,n,1)

      for(h in 1:n){

        ce[h] <- ce_en(profit = data[,t],
                       rac    = RAC$r[h],
                       weight = weight)

      }

      ce_lst[[t]] <- ce

    }

    ceExpNeg        <- as.data.frame(ce_lst)
    names(ceExpNeg) <- names_treatments


    if(rac_ini == rac_fin){

      plot_CE_epneg <- function(){plot_ce_en(data = ceExpNeg,
                                             rac  = RAC)}

    } else if(rac_ini != rac_fin){

      plot_CE_epneg <- function(){plot_ce_en(data    = ceExpNeg,
                                             rac     = 0,
                                             rac_ini = rac_ini,
                                             rac_fin = rac_fin,
                                             rac_len = n)}

    }

    invisible(list(CE_table = ceExpNeg,
                   CE_plot  = plot_CE_epneg,
                   ARAC     = RAC$r))
  }

}
