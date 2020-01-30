#' clean_chromosome_data
#'
#' clean chromosome data by deleting all detected outliers
#'
#'
#' @param chromosome xxx
#' @param genomeName xxx
#' @param chrID xxx
#'
#' @importFrom graphics boxplot
#' @return cleanedChromosome
#' @export

clean_chromosome_data <- function(chromosome, genomeName, chrID){

  # boxplots on the genetic map (cm) representing ranges on the centiMorgan values
  ecartsGenVector = c(chromosome$cm[1])
  for(k in 2:(nrow(chromosome))){ # for all points included in current sliding window (sw)
    ecartsGenVector = c(ecartsGenVector, abs(chromosome$cm[k] - chromosome$cm[k-1]))
  }
  # png(file = stringr::str_replace_all( paste( "plots/dataCleaning/", genomeName,"- Chr", chrID, ".png"), fixed(" "), ""))

  b = boxplot(ecartsGenVector, horizontal = TRUE, plot = FALSE)


  # /-- identify the outliers using the boxplot and delete them from the chromosome

  outliersSelonEcarts = b$out
  outlierIndexList = c()
  for(i in unique(outliersSelonEcarts)){
    outlierIndexList = c(outlierIndexList, which(i == ecartsGenVector))
  }
  outliers_nbr = length(outlierIndexList) # nbr of all outliers detected
  cat("\n Number of outliers detected : ", outliers_nbr)
  print("Here are their coordinates :")
  print( chromosome[outlierIndexList, ])
  if (!is.null(outlierIndexList)) { # if outliers are defined
    outliersFrame = data.frame()
    for(i in outlierIndexList){
      outliersFrame = rbind(outliersFrame, chromosome[i,])
    }
    cleanedChromosome = chromosome[-outlierIndexList, ]
    cat("\n Size of cleaned chromosome : ", nrow(cleanedChromosome))

    # p = plot_ly(type = "box")%>%
    #     add_boxplot(y = ~ecartsGenVector,  boxpoints = 'identified outliers',
    #                 marker = list(color = "red", outliercolor = "red"),
    #                 # line = list(outliercolor = 'rgba(219, 64, 82, 1.0)', outlierwidth = 2)) #,
    #                 line = list(color = 'rgb(8,81,156)'))
    # p
    # p = ggplot2::ggplot(data = cleanedChromosome) +
    #         ggplot2::aes(x=cleanedChromosome$mb, y=cleanedChromosome$cm, colour= 'black', size=1) +
    #     ggplot2::geom_point(data = outliersFrame) +
    #         ggplot2::aes(x = outliersFrame$mb, y = outliersFrame$cm, color = 'red', size=1 )
    # print(p)
    # points(outliersFrame$mb, outliersFrame$cm, col = "red", pch=19)
    # mtext(paste(outliers_nbr, '/', nrow(chromosome), '=>',  round(outliers_nbr*100/nrow(chromosome), 2), '%'))
    # cat(outliers_nbr, '/', nrow(chromosome), '=>', round(outliers_nbr*100/nrow(chromosome), 2), '%')

    # ggplot(chromosome, aes(x = mb, y1 = cm, color = "blue")) + geom_point()

  }else{ # no outliers are found
    print("Wow ! your data currently present no outliers ")
    cleanedChromosome = chromosome
  }
  return(cleanedChromosome)
}
