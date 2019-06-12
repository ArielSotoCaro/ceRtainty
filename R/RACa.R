#' Adjusted Risk Aversion Coefficient (RACa)
#'
#' @param rac An scalar with the value of the relative RAC
#' @param data Dataset to weight the RAC
#'
#' @return This function create an adjustment to the relative risk
#' aversion coefficient, following Hardaker et al (2004).

RACa <- function(rac,data){

  maxim <- max(data)
  RAC   <- 1 - rac*10^(nchar(abs(as.integer(maxim)))-1)

  return(RAC)
}
