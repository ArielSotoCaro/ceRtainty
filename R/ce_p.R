#' Certainty Equivalent Function with Power Utility Function
#'
#' @param profit \code{data.frame} with profit values
#' @param rac scalar of RAC value
#' @param weight original wealth
#'
#' @return Scalar with the CE value
#'

ce_p <- function(profit,rac,weight=0){

  profit <- as.matrix(profit)

  if(rac == 0){

    CEpower <- mean(profit,na.rm=T)

  } else {

    su      <- sum((profit+weight)^(1-rac), na.rm=T)
    CEpower <- ((su/length(profit))^(1/(1-rac))) - weight

  }

  return(CEpower)

}
