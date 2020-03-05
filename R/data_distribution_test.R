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

    # add new DQC_2-------------------------------------
    # step1: centromeric gap exist? -> what size?


    # step2: pvalue from chi-sq test ?
    # simulate uniform distribution with same chrSize and number of mrkrs as in testChromosome
    inputData = read.csv(file = paste0("../data/BREC_byArm_MB_marey_map_R5.36_removeOutliers.csv"), header = T, sep = "\t")
    refChromosome_2L = get_chromosome_from_inputData(inputData, "2L")
    refChromosome_2R = get_chromosome_from_inputData(inputData, "2R")

    # from transform_inputData_to_chromosomes() ---------------------------------------
    lenghtOfFirstArm = 22.87981 # size(2L_R5)
    RightArm = refChromosome_2R
    LeftArm = refChromosome_2L
    # refChromosome = transform_2arms_to_chromosome(firstArm, secondArm, lenghtOfFirstArm) # merge both arms into one chr : remember coord normalisation

    RightArm$mb = RightArm$mb + lenghtOfFirstArm
    refChromosome = rbind(LeftArm, RightArm)
    refChromosome$chr = "2" # extract the chr number from it's string name

    # cleaning step
    # refChromosome = clean_chromosome_data(refChromosome, genomeName = "DmelR5", chrID) #=> chr2 : 240 instead of 267
    MB = refChromosome$mb
    cM = refChromosome$cm
    MB = as.numeric(MB)
    testChromosome_size = nrow(testChromosome)
    refChromosome_size = nrow(refChromosome)
    # regression function = Loess using 2nd degree polynomial
    model = loess(cM ~ MB, span = 0.30, degree = 2)
    newMB = seq(0.015, by = 0.2, length.out = testChromosome_size)
    newMB = c(newMB, rep(NA, refChromosome_size - testChromosome_size))
    regFn = predict(`model`, data.frame(newMB))
    regFn[regFn<0] = 0
    ref_distribution_Chromosome = data.frame(mb = newMB, cm = regFn)

    # apply chi-sq test and get pvalue-------
    correlation_test = cor.test(testChromosome$cm, ref_distribution_Chromosome$cm[1:testChromosome_size], method = "spearman")
    chi2Pvalue = correlation_test$p.value
    # -------------------------------------

    if(chi2Pvalue < 0.05){
        goodDataDistribution = TRUE
        print("OK! Good data distribution !")
    }else{
        print("Oups! Not a good data distribution !")
    }

  return(goodDataDistribution) # boolean
}
