#' #' Dev_paper_comparing_methods_test_with_chisquare_comeron
#' #'
#' #' test data distribution using a chi-squared test with a multivariate normal distribution
#' #'
#' #'
#' #' @param testChromosome the chromosome data to be tested
#' #'
#' #' @import stats
#' #'
#' #' @return boolean value saying weather data distribution is good (TRUE) or not (FALSE)
#' #' @export
#'
#'
#' Dev_paper_comparing_methods_test_with_chisquare_comeron <- function(refData, testData) {
#'
#'     # here, test and ref data are both vectors of recombination rate values to compare
#'     # for example, testData = RR_using_polynomial and refData = RR_using_Loess0.25
#'
#'     #======== Read Comeron data from tables ===================================
#' getwd()
#'
#'     comeron_plots_path = "../plots/"
#'     comeronDataFilesPath = "~/ProBook/_ISEM/Thesis_work/Recombination_BREC_project_thesis_part_1/BREC_paper_writing/Oxford_Bioinformatics_submission/December_comeback/New_validation_and_comparison_tests/my_versions/Comeron_tables/"
#'
#'     # chromosomal arms --------------------------------------
#'     comeron_X = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chrX.txt"), header = F, sep = "\t")
#'     comeron_2L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2L.txt"), header = F, sep = "\t")
#'     comeron_2R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2R.txt"), header = F, sep = "\t")
#'     comeron_3L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3L.txt"), header = F, sep = "\t")
#'     comeron_3R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3R.txt"), header = F, sep = "\t")
#'
#'     ggX = ggplot(comeron_X) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr X", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg2L = ggplot(comeron_2L) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 2L", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg2R = ggplot(comeron_2R) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 2R", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg3L = ggplot(comeron_3L) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 3L", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg3R = ggplot(comeron_3R) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 3R", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'
#'     ggsave(filename = paste0("X-Comeron_plots_2019.png"), plot = ggX, path = comeron_plots_path)
#'     ggsave(filename = paste0("2L-Comeron_plots_2019.png"), plot = gg2L, path = comeron_plots_path)
#'     ggsave(filename = paste0("2R-Comeron_plots_2019.png"), plot = gg2R, path = comeron_plots_path)
#'     ggsave(filename = paste0("3L-Comeron_plots_2019.png"), plot = gg3L, path = comeron_plots_path)
#'     ggsave(filename = paste0("3R-Comeron_plots_2019.png"), plot = gg3R, path = comeron_plots_path)
#'
#'     # whole chromosomes --------------------------------------
#'     comeron_X = read.csv(file = paste0(comeronDataFilesPath, "/perChr/Comeron_100kb_chrX.txt"), header = F, sep = "\t")
#'     comeron_2 = read.csv(file = paste0(comeronDataFilesPath, "/perChr/Comeron_100kb_chr2.txt"), header = F, sep = "\t")
#'     comeron_3 = read.csv(file = paste0(comeronDataFilesPath, "/perChr/Comeron_100kb_chr3.txt"), header = F, sep = "\t")
#'
#'     ggX = ggplot(comeron_X) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr X", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg2 = ggplot(comeron_2) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 2", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'     gg3 = ggplot(comeron_3) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr 3", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'
#'     ggsave(filename = paste0("X-Comeron_plots_2019.png"), plot = ggX, path = comeron_plots_path) #plot = last_plot()
#'     ggsave(filename = paste0("2-Comeron_plots_2019.png"), plot = gg2, path = comeron_plots_path)
#'     ggsave(filename = paste0("3-Comeron_plots_2019.png"), plot = gg3, path = comeron_plots_path)
#'
#'     # =========================================================================
#'
#'
#'
#'
#'     # read dmel R5 data
#'     inputData = read.csv(file = "../data/BREC_byArm_MB_marey_map_R5.36_removeOutliers.csv", header = T, sep = "\t")
#'     chrList = get_list_of_chromosomes(inputData)
#'
#'     chrID = "X"
#'
#'     # for (chrID in chrList) {
#'         print(c("=========== chr in process ====> ", chrID, "==================="))
#'         chromosome = get_chromosome_from_inputData(inputData, chrID)
#'         # print(c("refChromosomeSize",nrow(testChromosome)))
#'
#'         # estimate RR with all scenarios
#'         RR_object_polynomial = estimate_recombination_rates_third_degree_polynomial(chromosome)
#'         RR_object = RR_object_polynomial
#'
#'         minRR_object = get_min_RR_value_based_on_polynomial(chromosome)
#'         print(" get minRR done !")
#'
#'         chrType = get_chromosome_type(chromosome, minRR_object)
#'         # RR_object2 = RR_object
#'
#'         R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
#'         print("R2 done")
#'         print(R2DataFrame2D)
#'
#'
#'         # if(using_slidingWindowApproach_for_HCB) {
#'         print("Extracting CB for this chromosome ...")
#'         heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
#'         print("extract centroCB done")
#'         telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType, minRR_object)
#'         print("extract telo CB done")
#'         RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType, minRR_object)
#'         print("extrapolation done")
#'
#'         # -------------------------------------------
#'         #  here, before being able to plot together, BREC results needs to follow the same bins as Comeron data => 100Kb = 0.0001Mb
#'         ggX +
#'         ggX = ggplot(RR_object) + geom_line(aes(x = V1/10^6, y = V2, color = 'red')) + labs(title = "chr X", x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)", color = "Comeron map")
#'
#'
#'         # 8888888888888888888888888888888888888888888888888888888888888888888888888888888888888
#'         RR_object_loess0.25 = estimate_recombination_rates_loess(testChromosome, span = 0.25)
#'         RR_object_loess0.5 = estimate_recombination_rates_loess(testChromosome, span = 0.5)
#'         RR_object_loess0.75 = estimate_recombination_rates_loess(testChromosome, span = 0.75)
#'         print("all RR done")
#'
#'     # }
#'
#'
#'
#'     # apply chi-square test ===================
#'
#'     testDataAsMatrix = as.matrix.data.frame(subset((RR_object_polynomial$regDr)), rownames.force = NA)
#'     refDataAsMatrix = as.matrix.data.frame(subset(RR_object_loess0.25$regDr), rownames.force = NA)
#'     chi2_test = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE)
#'
#'     chi2_1 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.75$regDr, rescale.p = TRUE) # to rescale p to sum up to 1
#'     chi2_1
#'     chi2_1_inv = chisq.test(RR_object_loess0.75$regDr, p = RR_object_polynomial$regDr, rescale.p = TRUE) # to rescale p to sum up to 1
#'     chi2_1_inv
#'
#'     chi2_2 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'     chi2_3 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'
#'     chi2_4 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'     chi2_5 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'
#'     print(c("chi2_1 = ", chi2_1, "chi2Pvalue_1 = ", chi2_1$p.value ))
#'     print(c("chi2_2 = ", chi2_2, "chi2Pvalue_2 = ", chi2_2$p.value ))
#'     print(c("chi2_3 = ", chi2_3, "chi2Pvalue_3 = ", chi2_3$p.value ))
#'     print(c("chi2_4 = ", chi2_4, "chi2Pvalue_4 = ", chi2_4$p.value ))
#'     print(c("chi2_5 = ", chi2_5, "chi2Pvalue_5 = ", chi2_5$p.value ))
#'
#'     significativellyDifferent = FALSE
#'
#'
#'     # testDataAsMatrix = as.matrix.data.frame(subset(testData, select=c("cm", "mb")), rownames.force = NA)
#'     # refDataAsMatrix = as.matrix.data.frame(subset(refData, select=c("cm", "mb")), rownames.force = NA)
#'     # chi2 = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'
#'
#'
#'     chi2 = chisq.test(testData, p = refData, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'     print(chi2)
#'     chi2Pvalue = chi2$p.value
#'     print(chi2Pvalue)
#'
#'     if(chi2Pvalue > 0.05){
#'         significativellyDifferent = TRUE
#'         print("OK! not significativelly Different .. statistically significant !?")
#'     }else{
#'         print("Oups! significativelly different")
#'     }
#'
#'     return(significativellyDifferent) # boolean
#' }
