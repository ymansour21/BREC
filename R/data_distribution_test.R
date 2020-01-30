#' data_distribution_test
#'
#' test data distribution using a chi-squared test with a multivariate normal distribution
#'
#'
#' @param testChromosome the chromosome data to be tested
#'
#' @import stats
#'
#' @return boolean value saying weather data distribution is good (TRUE) or not (FALSE)
#' @export


data_distribution_test <- function(testChromosome) {

    goodDataDistribution = FALSE

    # add new DQC_2

    if(chi2Pvalue > 0.05){
        goodDataDistribution = TRUE
        print("OK! Good data distribution !")
    }else{
        print("Oups! Not a good data distribution !")
    }


# ==== old version of DQC ==========================
  # goodDataDistribution = FALSE
  # testChromosomeAsMatrix = as.matrix.data.frame(subset(testChromosome, select=c("cm", "mb")), rownames.force = NA)
  # n = nrow(testChromosome) # number of data needed to generate the mvnormal distribution
  # mu = c(50, 50) # vector of means
  # Sigma <- matrix(c(10,6,6,16),2,2) # matrix of 2*2
  # normalDistribution = MASS::mvrnorm(n, mu, Sigma) # multivariate normal distribution
  # chi2 = chisq.test(testChromosomeAsMatrix, p = normalDistribution, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
  # print(chi2)
  # chi2Pvalue = chi2$p.value
  # print(chi2Pvalue)
  # if(chi2Pvalue > 0.05){
  #   goodDataDistribution = TRUE
  #   print("OK! Good data distribution !")
  # }else{
  #   print("Oups! Not a good data distribution !")
  # }
# ==================================================

  return(goodDataDistribution) # boolean
}
