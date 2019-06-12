#' Plot of the Risk Premium values using Exponential Negative Utility Function
#'
#' @param data \code{data.frame} of CE computed by Exponential Negative function
#' @param rac_ini Initial RAC values used in the CE computation
#' @param rac_fin Final RAC values used in the CE computation
#' @param rac_len Length of the RAC vector used in the CE computation
#'
#' @return Plot object
#'
#' @import RColorBrewer dplyr
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics legend


plot_risk_premium_en <- function(data,rac_ini,rac_fin,rac_len){

  names_treatments <- names(data)
  n_treat          <- ncol(data)

  par(mar = c(5, 4, 2.2, 0.5))

  co <- brewer.pal(n = n_treat, name = 'Accent')

  plot(rev(seq(rac_ini , rac_fin , length.out = rac_len)),
       data[,1],
       ylim = c(min(data),max(data)),
       xlim = c(rac_ini,rac_fin),
       pch  = 16,
       type = 'b',
       col  = co[1],
       ylab = "Risk Premium",
       xlab = "ARAC")

  for(p in 1:n_treat-1){
    points(rev(seq(rac_ini , rac_fin , length.out = rac_len)),
           data[,p+1],
           col  = co[p+1],
           type = 'b',
           pch  = 16)
  }

  add_legend("topright",
             legend = names_treatments,
             pch    = 16,
             col    = co,
             horiz  = TRUE,
             bty    = 'n',
             cex    = 0.8)
}
