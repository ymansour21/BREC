#' clean_5_perCent_chromosome_data
#'
#' clean chromosome data by deleting 5\% of the detected outliers
#'
#' @param chromosome xxx
#' @param genomeName xxx
#' @param chrID xxx
#'
#' @importFrom grDevices boxplot.stats
#' @import stats
#' @return cleanedChromosome_5_perCent
#' @export

clean_5_perCent_chromosome_data <- function(chromosome, genomeName, chrID){

  # boxplots on the genetic map (cm) : les écarts sur les centiMorgan
  ecartsGenVector = c(chromosome$cm[1])
  for(k in 2:(nrow(chromosome))){ # for all points included in current sliding window (sw)
    ecartsGenVector = c(ecartsGenVector, abs(chromosome$cm[k] - chromosome$cm[k-1]))
  }
  normalDistri = dnorm(ecartsGenVector, mean = mean(ecartsGenVector), sd = sd(ecartsGenVector))
  b = boxplot.stats(ecartsGenVector)
  # /-- get the outliers from boxplot and delete them from the chromosome

  outliersSelonEcarts = b$out
  nbr_5_perCent_outliers = floor(0.05*nrow(chromosome))
  cat("\n Number of outliers detected : ", nbr_5_perCent_outliers, "\n")
  top_5_perCent = sort(outliersSelonEcarts, decreasing = TRUE)[1:nbr_5_perCent_outliers]
  outlierIndexList_5_perCent = c()
  for(j in unique(top_5_perCent)){
    outlierIndexList_5_perCent = c(outlierIndexList_5_perCent, which(j == ecartsGenVector))
  }
  print(outlierIndexList_5_perCent)
  print("Here are their coordinates :")
  print(chromosome[outlierIndexList_5_perCent,])
  if (!is.null(nbr_5_perCent_outliers)) { # if outliers are defined
    outliersFrame_5_perCent = data.frame()
    for(j in outlierIndexList_5_perCent){
      outliersFrame_5_perCent = rbind(outliersFrame_5_perCent, chromosome[j,])
    }
    cleanedChromosome_5_perCent = chromosome[-outlierIndexList_5_perCent, ]

  }else{ # no outliers are found
    print("Wow ! your data present no outliers ")
    cleanedChromosome_5_perCent = chromosome
  }
  return(cleanedChromosome_5_perCent)
}
