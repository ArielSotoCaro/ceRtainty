#' Certainty equivalent computation
#'
#' @export
#'
#' @param data \code{data.set} with profit for each treatment/project.
#' Each column is a treatment and each row a different profit
#' observation.
#' @param ival The initial value for the RAC vector to employ (scalar).
#' @param fval The final value for the RAC vector to employ (scalar).
#' @param wealth The initial agent wealth. By default is zero.
#' @param utility Indicator of utility function: "ExpNeg" for the
#' Exponential Negative utility, and "Power" for the Power utility
#' function.
#'
#' @return This function produces three objects: \code{CE_values}
#' is a table with treatment by columns and certainty values by row;
#' \code{RAC} is a vector with the absolute risk aversion
#' coefficients (ARAC) if the Power utility function was implemented,
#' or the relative risk aversion coefficient (RRAC) if the
#' Exponential Negative utility function was implemented. The length
#' of this vector is the same as the number of profit observations
#' in the original dataset; and, \code{CE_plot} is a graph using plot
#' function, to compare the different CEs computed.
#'
#' @references Hardaker, J.B., Richardson, J.W., Lien, G., &
#' Schumann, K.D. (2004). Stochastic efficiency analysis with risk
#' aversion bounds: a simplified approach. Australian Journal of
#' Agricultural and Resource Economics, 48(2), 253-270.
#'
#' @details This function computes the certainty equivalent values
#' using profit as inputs. Works with \code{data.frames} with 3 or
#' more observations. Consider each column as a different treatment
#' or project.
#'
#' @examples
#' ## Example 1. Using profit data from ceRtainty package
#' data(profitSWG)
#'
#' # Storing CE values using Power utility function
#' c1 <- certainty(data    = profitSWG,
#'                 ival    = .5,
#'                 fval    = 4,
#'                 utility = "Power")
#' c1$CE_values # Table with CE values
#' c1$RAC       # RAC vector used in CE computation
#' plot(c1)     # Invoking the CE plot
#'
#' # To use the ExpNeg function, it is required the RRAC (ARAC/wealth)
#' # so we can compute the mean value among all profit in the dataset.
#'
#' # Mean value among all profit value
#' mean(sapply(profitSWG,mean)) # 5081.844
#'
#' # Storing CE values using Power utility function
#' c1 <- certainty(data    = profitSWG,
#'                 ival    = .5/5082,
#'                 fval    = 4/5082,
#'                 utility = "ExpNeg")
#'
#' c1$CE_values # Table with CE values
#' c1$RAC       # RAC vector used in CE computation
#' plot(c1)
#'
#' ## Example 2. Using the example values of Hardaker et al. (2004)
#' dt <- data.frame(treatment=c(100,125,135,142,147,150,153,158,163,175,195))
#' # Storing CE values using Power utility function. Hardaker use an
#' # unique RAC value (0.005)
#' c2 <- certainty(data    = dt,
#'                 ival    = .005,
#'                 fval    = .005,
#'                 utility = "Power")
#' # or
#' c2 <- certainty(data    = dt,
#'                 ival    = .005,
#'                 fval    = .005,
#'                 utility = "ExpNeg")
#'
#' c2$CE_values
#' c2$RAC
#' plot(c2)


certainty <- function(data,ival,fval,utility,wealth=0){

  if(is.data.frame(data)==FALSE){
    stop("\nDataset should be a data.frame object.\n")
  }

  if(ncol(data)<1){
    stop("\nDataset should have 1 column as minimum.\n")
  }

  if(utility == "ExpNeg"){
    ce <- ce_epnegative(data    = data,
                        rac_ini = ival,
                        rac_fin = fval,
                        weight  = wealth)

    tb <- ce$CE_table
    rr <- ce$ARAC

  } else if(utility == "Power"){
    ce <- ce_power(data    = data,
                   rac_ini = ival,
                   rac_fin = fval,
                   weight  = wealth)

    tb <- ce$CE_table
    rr <- ce$RRAC

  } else if(utility != "ExpNeg" | utility != "Power"){
    stop("\nThe utility function is incorrect")
  }

  rr <- list(racVector = rr,
             rac_ini   = ival,
             rac_fin   = fval,
             rac_len   = nrow(data)
  )

  invisible(structure(
    class = 'ceRtainty',
    list(CE_values = tb,
         RAC       = rr,
         Utility   = utility))

  )

}
