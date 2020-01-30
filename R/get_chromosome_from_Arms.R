#' get_chromosome_from_Arms
#'
#' ...
#'
#' @param inputData xx
#' @param chrID xxx
#'
#' @importFrom dplyr filter
#'
#' @return chromosome
#' @export

get_chromosome_from_Arms <- function(inputData, chrID){

  chromosome = filter(inputData, inputData$chr== chrID)
  chromosome = chromosome[order(chromosome$mb),]  #sort by ascendaing mb which is always true (logically)
  rownames(chromosome) <- NULL
  return(chromosome)
}
