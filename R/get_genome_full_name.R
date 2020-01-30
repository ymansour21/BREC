#' get_genome_full_name
#'
#' get the species full name from the input genome short name
#'
#'
#' @param inputGenomeName : string
#'
#' @return genomeName : string
#' @export

get_genome_full_name <- function(inputGenomeName){

  switch (stringr::str_to_upper(inputGenomeName),
          'AEDES'={
            genomeName = "Aedes_aegypti"
          },
          'CULEX'={
            genomeName = "Culex_pipiens"
          },
          'DROSOPHILA_R5'={
            genomeName = "Drosophila_melanogaster_R5"
          },
          'DROSOPHILA_R6'={
            genomeName = "Drosophila_melanogaster_R6"
          },
          'APIS'={
            genomeName = "Apis_mellifera"
          },
          'SALMO'={
            genomeName = "Salmo_trutta"
          },
          'HUMAN'={
            genomeName = "Homo_sapiens"
          },
          'ARABIDOPSIS'={
            genomeName = "Arabidopsis_ARABIDOPSIS"
          },
          { # default
            stop("speciesList")
          }
  )
  return(genomeName)
}
