#' Risk Premium computation
#'
#' @export
#'
#' @param tbase Name of the base treatment/project
#' @param ce_data \code{data.frame} with CE values previously computed
#' @param rac Vector with RAC sequence used in the CE computation
#' @param utility The utility function: "ExpNeg" if CE it was computed
#' using Exponential Negative utility function. "Power" if the utility
#' function was Power
#'
#' @details This function computes the risk premium values, regarding
#' a project or treatment arbitrarily chosen by the user, using a CEs
#' dataset (a `certainty` object) already computed.
#'
#' @return Generates three objects: A data.frame with the total
#' values of the premium risks; a \code{data.frame} with the percentage of
#' difference with respect the base treatment; and a plot with the
#' treatments' premium risk.
#'
#' @import dplyr
#'
#' @examples
#' ## Example using profit dataset
#' data(profitSWG)
#'
#' # First, compute the CE values
#' c1 <- certainty(data = profitSWG,ival = .5,fval = 4,utility = "Power")
#'
#' ce_values <- c1$CE_values  # CE table
#' ce_rac <- c1$RAC           # RAC vector
#'
#' # The Risk premium values respect to Serenade treatment
#' rp <- premium(tbase = "serenade", ce_data = ce_values,rac = ce_rac, utility = "Power")
#'
#' rp$PremiumRisk       # absolute values
#' rp$PremiumRiskPer100 # values in percentage
#' rp$RP_plot()         # plot

premium <- function(tbase,ce_data,rac,utility){

  if(is.character(tbase)!=TRUE | length(tbase)>1){
    stop("\ntbase must be a character object and scalar.\n")
  }

  if(ncol(as.matrix(rac))>1){
    stop("\nrac vector must have only one column.\n")
  }

  if(nrow(as.matrix(ce_data))<2){
    stop("\nTo compute Premium risk is requiered\n
         more than 1 projects. ce_data contains less\n
         than 2 alternatives.\n")
  } else {

    ini <- rac$rac_ini
    fin <- rac$rac_fin
    len <- rac$rac_len

    n_base <- as.character(tbase)
    n      <- nrow(ce_data)
    t      <- ncol(ce_data)-1

    base  <- ce_data %>% select(n_base)  %>% as.matrix()
    other <- ce_data %>% select(-n_base) %>% as.matrix()
    nms   <- ce_data %>% select(-n_base) %>% names()

    # Preparing the loop
    pre_risk     <- list()
    pre_risk_per <- list()
    pr           <- matrix(0,n,1)
    pr_per       <- matrix(0,n,1)

    for(c in 1:t){

      for(j in 1:n){
        pr[j]     <- base[j] - other[j,c]
        pr_per[j] <- 100 - 100*(base[j] / other[j,c])
      }

      pre_risk[[c]]     <- pr
      pre_risk_per[[c]] <- pr_per

    }

    premium_risk            <- as.data.frame(pre_risk)
    premium_risk_per        <- as.data.frame(pre_risk_per)
    names(premium_risk)     <- nms
    names(premium_risk_per) <- nms

    if(utility=='ExpNeg'){

      plot_rp <- function(){plot_risk_premium_en(data    = premium_risk,
                                                 rac_ini = ini,
                                                 rac_fin = fin,
                                                 rac_len = len)
      }

    } else if(utility=='Power') {

      plot_rp <- function(){plot_risk_premium_p(data    = premium_risk,
                                                rac_ini = ini,
                                                rac_fin = fin,
                                                rac_len = len)
      }
    } else {stop("\nIncorrect utility function!\n")}
  }

  invisible(list(PremiumRisk       = premium_risk,
                 PremiumRiskPer100 = premium_risk_per,
                 RP_plot           = plot_rp)
  )
}
