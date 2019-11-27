#' Plot class function
#' @param data ...
#' @param rac ...
#' @param rac_ini ...
#' @param rac_fin ...
#' @param rac_len ...
#'
#'


plot.ceRtainty <- function(CER){

if(CER$Utility == 'Power'){



  if(CER$RAC$rac_ini == CER$RAC$rac_fin){

  plot_ce_power(data = CER$CE_values,
                rac  = CER$RAC$racVector)

  } else if(CER$RAC$rac_ini != CER$RAC$rac_fin){

    plot_ce_power(data    = CER$CE_values,
                  rac     = 0,
                  rac_ini = CER$RAC$rac_ini,
                  rac_fin = CER$RAC$rac_fin,
                  rac_len = CER$RAC$rac_len)

  }
}
  else if(CER$Utility == 'ExpNeg'){

    if(CER$RAC$rac_ini == CER$RAC$rac_fin){

      plot_ce_en(data = CER$CE_values,
                 rac  = CER$RAC$racVector)

    } else if(CER$RAC$rac_ini != CER$RAC$rac_fin){

      plot_ce_en(data    = CER$CE_values,
                 rac     = 0,
                 rac_ini = CER$RAC$rac_ini,
                 rac_fin = CER$RAC$rac_fin,
                 rac_len = CER$RAC$rac_len)

    }

  }

  }
