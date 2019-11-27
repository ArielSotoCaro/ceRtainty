#' Summary Method function
#'
#'

summary.ceRtainty <- function(CER,details = FALSE){

  if(details==FALSE){

    cat('================================\n')
    cat('Certainty Equivalent Computation\n')
    cat('================================\n')
    cat('\n')
    cat('Utility function:       ',CER$Utility,'\n')
    cat('Number of projects:     ',ncol(CER$CE_values),'\n')
    cat('Number of Observations: ',nrow(CER$CE_values),'\n')
    cat('\n')

    if(ncol(CER$CE_values)>1){
      namesCE <- names(CER$CE_values)
    } else if(ncol(CER$CE_values)==1){
      namesCE <- c('Treatment')
    }

    # Computing some statistics
    meanCE  <- sapply(CER$CE_values,FUN = mean)
    sdCE    <- sapply(CER$CE_values,FUN = sd)
    minCE   <- sapply(CER$CE_values,FUN = min)
    maxCE   <- sapply(CER$CE_values,FUN = max)

    # Preparing the table of statistics
    t <- rbind(meanCE,sdCE,minCE,maxCE)

    colnames(t) <- namesCE
    rownames(t) <- c('Mean','Std Dev','Min','Max')


    cat('--------------------------------\n')
    cat('CE Descriptive statistic:\n')
    cat('--------------------------------\n')
    print(t)
  }

  else if(details==TRUE){

    cat('================================\n')
    cat('Certainty Equivalent Computation\n')
    cat('================================\n')
    cat('\n')
    cat('Utility function:       ',CER$Utility,'\n')
    cat('Number of projects:     ',ncol(CER$CE_values),'\n')
    cat('Number of Observations: ',nrow(CER$CE_values),'\n')
    cat('\n')

    if(ncol(CER$CE_values)>1){
      namesCE <- names(CER$CE_values)
    } else if(ncol(CER$CE_values)==1){
      namesCE <- c('Treatment')
    }

    # Computing some statistics
    meanCE  <- sapply(CER$CE_values,FUN = mean)
    sdCE    <- sapply(CER$CE_values,FUN = sd)
    minCE   <- sapply(CER$CE_values,FUN = min)
    maxCE   <- sapply(CER$CE_values,FUN = max)

    # Preparing the table of statistics
    t <- rbind(meanCE,sdCE,minCE,maxCE)

    colnames(t) <- namesCE
    rownames(t) <- c('Mean','Std Dev','Min','Max')


    cat('--------------------------------\n')
    cat('CE Descriptive statistic:\n')
    cat('--------------------------------\n')
    print(t)

    cat('\n')
    cat('--------------------------------\n')
    cat('CE Detailed output:\n')
    cat('--------------------------------\n')

    cat('\n')
    print(data.frame('CE' = CER$CE_values))

    cat('\n')
    print(data.frame('RAC' = CER$RAC$racVector))

  }

}

