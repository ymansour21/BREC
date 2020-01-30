#' Get local recombination rate values for a chromsome
#'
#' get recombination rate values for a specific chromsome and save as csv
#'
#'
#' @param inputGenomeName xx
#' @param inputData ...
#' @param inputChrID xx
#' @param plots_path xx
#'
#' @import plotly
#' @return none ..
#' @export

get_RR_values_for_chromsome <- function(inputGenomeName, inputData, inputChrID, plots_path){

  finalRRObject = Brec_chromosome(inputGenomeName,inputData, inputChrID)
  print(finalRRObject)
  print('from get_RR_values_for_chromsome ')
  utils::write.table(finalRRObject, plots_path, "/my_final_RRestimates_Dmel_R6.csv", sep = "\t", row.names = FALSE, col.names = TRUE)
  # plot_ly(data = finalRRObject, x=~mb,y = ~rr, type = "scatter", mode = "lines", name = "Recombiantion rates")

}
