#' get_list_of_chromosomes
#'
#' get the list of chromosomes from the given dataset
#'
#'
#' @param inputData xx
#'
#' @importFrom stringr str_sort
#'
#' @return chrList a vector of IDs of chromosomes
#' @export


get_list_of_chromosomes <- function(inputData){
  chrList = str_sort(unique(inputData$chr), numeric = TRUE)
  return(chrList)
}
