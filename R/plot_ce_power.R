#' Plot for CE using Power Utility Function
#'
#' @param data Data set with CE already computed
#' @param rac Scalar with the RAC to use in the CE computation.
#' When the analysis consider only one value of RAC
#' @param rac_ini Vector of the RAC to use in the CE computation.
#' When the analysis consider a sequence of RAC values
#' @param rac_fin Final value for the RAC vector
#' @param rac_len RAC vector length
#'
#' @return Plot of CE to compare treatments/projects
#'
#' @importFrom graphics plot
#' @importFrom graphics points
#' @importFrom graphics legend


plot_ce_power <- function(data,rac=0,rac_ini=0,rac_fin=1,rac_len=10){

  names_treatments <- names(data)
  n_treat          <- ncol(data)
  n                <- nrow(data)

  par(mar = c(5, 4, 2.2, 0.5))

  if(n_treat == 1 & n == 1){
    plot(unlist(rac[1,1]),
    # plot(rac[1],
         unlist(data[1,1]),
         pch  = 16,
         type = 'b',
         col  = 'red',
         ylab = "Certainty Equivalent",
         xlab = "RAC")

  } else if (n_treat>1 & n>1){
    co <- brewer.pal(n = n_treat, name = 'Accent')
    plot(rev(seq(rac_ini , rac_fin , length.out = rac_len)),
         data[,1],
         ylim = c(min(data),max(data)),
         pch  = 16,
         type = 'b',
         col  = co[1],
         ylab = "Certainty Equivalent",
         xlab = "RAC")

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

}
