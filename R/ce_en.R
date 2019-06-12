#' Certainty Equivalent Function for Negative Exponential Function
#'
#' @param profit \code{data.frame} with profit values
#' @param rac scalar of RAC value
#' @param weight original wealth
#'
#' @return Scalar with the CE value
#'

ce_en <- function(profit,rac,weight=0){

  profit <- as.matrix(profit + weight)

  if(rac == 0){

    CE_en <- mean(profit, na.rm = T)

  } else{

    su    <- sum(exp(-rac*profit), na.rm = T)
    CE_en <- log(su/length(profit))*(1/(-1*rac))

  }

  return(CE_en)

}
