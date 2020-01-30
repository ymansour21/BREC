#' data_density_test
#'
#' test data density
#'
#'
#' @param testChromosome the chromosome data to be tested
#'
#'
#' @return boolean value saying weather data density is good/TRUE or not/FALSE
#' @export


data_density_test <- function(testChromosome) {

    goodDataDensity = FALSE

    minDensity = 2 # try with 5 later  # (nbr_mrkrs / Mb) this parameter fixed from experimenting on DmelR6_chr2 simulations

    testChromosomeSize = nrow(testChromosome)
    testChromosome_density = testChromosomeSize / testChromosome$mb[testChromosomeSize]

    cat("testChromosome_density = " , testChromosome_density)

    if (nrow(testChromosome) >= minDensity){
        goodDataDensity = TRUE
        print(c("OK! Enough data points ==> density = ", testChromosome_density, "minDensity = ,", minDensity))
    }else{
        print(c("Oups! Not enough data points ==> density = ", testChromosome_density, "minDensity = ,", minDensity))
    }

    return(goodDataDensity) # boolean
}
