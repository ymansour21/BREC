#' data_quality_test
#'
#' test data quality based on markers number/density and distribution
#'
#'
#' @param testChromosome xx
#'
#' @return goodDataQuality a boolean value
#' @export



data_quality_test<- function(testChromosome) {

    goodDataDistribution = FALSE
    enoughData = FALSE # this means markersDEnsity >= minDensity

    enoughData = data_density_test(testChromosome)

    if(enoughData){
        goodDataDistribution = data_distribution_test(testChromosome) # testing data distribution quality with Chi-2 test, take p-value > 0.05
    }else{
        chi2Pvalue = 0
    }

    if(enoughData & goodDataDistribution){
        goodDataQuality = TRUE
    }else{
        goodDataQuality = FALSE
    }

  return(goodDataQuality) # boolean
}
