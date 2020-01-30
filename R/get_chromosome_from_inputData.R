#' get_chromosome_from_inputData
#'
#' get the data of specified chromosome from the input dataset
#'
#'
#' @param inputData xx
#' @param chrID xx
#'
#' @importFrom dplyr filter
#'
#' @return chromosome : a dataframe of two columns cm and mp
#' @export

get_chromosome_from_inputData <- function(inputData, chrID){

  chromosome = filter(inputData, inputData$chr == chrID)
  chromosome = within(chromosome, rm('chr')) # remove firt col : redundant info with no use beacuse we already know which chr is ths from chrID
  chromosome = chromosome[order(chromosome$mb),]  #sort by ascendaing mb which is always true (logically)
  rownames(chromosome) <- NULL
  return(chromosome)
}
