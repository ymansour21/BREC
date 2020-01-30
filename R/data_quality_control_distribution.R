#' #' data_quality_control_distribution
#' #'
#' #' test data quality based on markers number and distribution
#' #'
#' #'
#' #' @param refChromosome xx
#' #' @importFrom reshape2 melt
#' #'
#' #' @return goodDataQuality a boolean value
#' #' @export
#'
#'
#' data_quality_control_distribution <- function(refChromosome) {
#'
#'     plots_path =  paste0(getwd(),"/DQC2/")  #, Sys.Date(), "/" )
#'
#'     densityList= list()
#'     # load inputs data
#'     inputData = read.csv(file = "data/Dmel_R6_formatted_v2.csv", header = T, sep = "\t")
#'     chrList = get_list_of_chromosomes(inputData)
#'
#'     # transform_inputData_to_chromosomes() adapted : here using specific arm sizes
#'     newInputData = data.frame()
#'     for (i in seq(1, length(chrList)-1, by =2 )){ # refine stop condition for the loop as per number of chromosomic arms + lonly arms must be at the end of the list
#'         if(i == 1){
#'             lenghtOfFirstArm = 23.513712 # size(2L_R6)
#'         }else if(i == 3){
#'             lenghtOfFirstArm = 28.110227 # size(3L_R6)
#'         }
#'         firstArm = get_chromosome_from_Arms(inputData, chrList[i])
#'         secondArm = get_chromosome_from_Arms(inputData, chrList[i+1])
#'         chromosome = transform_2arms_to_chromosome(firstArm, secondArm, lenghtOfFirstArm) # merge both arms into one chr : remember coord normalisation
#'         newInputData = rbind.data.frame(newInputData, chromosome)
#'     }
#'     chromosome = get_chromosome_from_Arms(inputData, chrList[length(chrList)])
#'     newInputData = rbind.data.frame(newInputData, chromosome)
#'     inputData = newInputData
#'     chrList = get_list_of_chromosomes(inputData)
#'
#'     for (chrID in "X") { #chrList
#'             ## assign ref cyto boundaries according to chr in process
#'             if(chrID == "2"){# --------- for chr 2 dmel6
#'                 # refHCB_centro = cyto_R6
#'                 cl = 17.400000 # old : 21.17693
#'                 cr = 32.826207 # old : 26.22948
#'                 # refHCB_telo = Brec_chr100%
#'                 tl = 0.5
#'                 tr = 48.426189
#'             }else if(chrID == "3"){# --------- for chr 3 dmel6
#'                 # refHCB_centro = cyto_R6
#'                 cl = 19.906900
#'                 cr = 41.984505
#'                 # refHCB_telo = Brec_chr100%
#'                 tl = 0.7
#'                 tr = 59.184505
#'             }else if(chrID == "X"){ # --------- for chr X dmel6
#'                 # refHCB_centro = cyto_R6
#'                 cr = 20.928973
#'                 # refHCB_telo = Brec_chr100%
#'                 tl = 2.405967
#'             }
#'
#'             print(c("=========== chr in process ====> ", chrID, "==================="))
#'             refChromosome = get_chromosome_from_inputData(inputData, chrID)
#'             print(c("refChromosomeSize",nrow(refChromosome)))
#'             # cleaning step
#'             refChromosome = clean_chromosome_data(refChromosome, genomeName = "DmelR6", chrID) #=> chr2 : 240 instead of 267
#'             refChromosomeSize = nrow(refChromosome)
#'             print(c("refChromosome after cleaning --> size = ", refChromosomeSize))
#'
#'
#'             #prepare refchr for DQC
#'             # refChromosomeAsMatrix = as.matrix.data.frame(subset(refChromosome, select=c("cm", "mb")), rownames.force = NA)
#'
#'             # print(c("frac = ", frac, "n = ", nrow(testChromosome)))
#'             # ------------- simu1List : number of markers per chromosome ---------------------------------
#'             # enoughData = FALSE
#'             # simu1List = list()  # simulated chromosomes baesd on number of markers
#'             # simu1ListOfPvals = c()
#'
#'         tictoc::tic()
#'
#'             testChromosome = refChromosome
#'             decacl = c() # centro_left decalage_of_Brec_estimated_HCB_compared_To_ref_cyto_HCB
#'             decacr = c() # centro_right
#'             decatl = c() # telo_left
#'             decatr = c() # telo_right
#'             mkrDensity = c()
#'             densityFD = data.frame()
#'
#'
#'
#'             for(frac in testSizes) {#testSizes each 5%
#'                 hcb_cl = c()
#'                 hcb_cr = c()
#'                 hcb_tl = c()
#'                 hcb_tr = c()
#'
#'                 if (frac != 1) { ## not the case of 100%
#'                     for (i in c(1:30)) {
#'                         print(c("Test for : frac =", frac," No ", i, "  ---------------------------------"))
#'                         testChromosome = dplyr::sample_frac(refChromosome, frac)
#'                         testChromosome = testChromosome[order(testChromosome$mb),]  #sort by ascendaing mb which is always true (logically)
#'
#'                         # simu1List = rlist::list.append(simu1List, testChromosome)
#'                         # chi2 = chisq.test(refChromosomeAsMatrix, p = testChromosome, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'                         # simu1ListOfPvals = c(simu1ListOfPvals, chi2$p.value)
#'
#'                         gg1 = ggplot(data = testChromosome, mapping = aes(x=mb, y=cm)) + geom_point()
#'                         ggsave(filename = paste0("chr_",chrID, "_SimuChr-NbrMrks=", frac*100, "percent_testNo_",i,".png"), plot = gg1, path = plots_path) #plot = last_plot()
#'
#'                         # run BREC to get new HCB
#'                         RR_object = estimate_recombination_rates(testChromosome)
#'                         print("RR done")
#'                         chrType = get_chromosome_type(testChromosome, RR_object$regDr)
#'                         # RR_object2 = RR_object
#'                         R2DataFrame2D = compute_cumulated_R_squared_2directions(testChromosome)
#'                         print("R2 done")
#'
#'                         # if(using_slidingWindowApproach_for_HCB) {
#'                         print("Extracting CB for this chromosome ...")
#'                         heteroChromatinBoundaries = extract_CB(testChromosome, RR_object, R2DataFrame2D, chrID, chrType)
#'                         print("extract centroCB done")
#'                         telomeres_boundaries = extract_telomeres_boundaries(testChromosome, R2DataFrame2D, chrID, chrType)
#'                         print("extract telo CB done")
#'
#'                         hcb_cl = c(hcb_cl, heteroChromatinBoundaries$heteroBoundLeft)
#'                         hcb_cr = c(hcb_cr, heteroChromatinBoundaries$heteroBoundRight)
#'                         hcb_tl = c(hcb_tl, telomeres_boundaries$extrapolPhysPos_left)
#'                         hcb_tr = c(hcb_tr, telomeres_boundaries$extrapolPhysPos_right)
#'                     }
#'
#'                 }else{ ## case of  100% --> run only once not 30 times since it's the same
#'                     print("Test for : original_cleaned_chromosome ---------------------------------")
#'                     # simu1List = rlist::list.append(simu1List, testChromosome)
#'                     # chi2 = chisq.test(refChromosomeAsMatrix, p = testChromosome, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#'                     # simu1ListOfPvals = c(simu1ListOfPvals, chi2$p.value)
#'
#'                     gg2 = ggplot(data = testChromosome, mapping = aes(x=mb, y=cm)) + geom_point()
#'                     ggsave(filename = paste0("chr_",chrID, "_SimuChr-NbrMrks=", frac*100, "percent_original_cleaned_chromosome.png"), plot = gg2, path = plots_path)
#'
#'                     # run BREC to get new HCB
#'                     RR_object = estimate_recombination_rates(testChromosome)
#'                     print("RR done")
#'                     chrType = get_chromosome_type(testChromosome, RR_object$regDr)
#'                     # RR_object2 = RR_object
#'                     R2DataFrame2D = compute_cumulated_R_squared_2directions(testChromosome)
#'                     print("R2 done")
#'
#'                     # if(using_slidingWindowApproach_for_HCB) {
#'                     print("Extracting CB for this chromosome ...")
#'                     heteroChromatinBoundaries = extract_CB(testChromosome, RR_object, R2DataFrame2D, chrID, chrType)
#'                     print("extract centroCB done")
#'                     telomeres_boundaries = extract_telomeres_boundaries(testChromosome, R2DataFrame2D, chrID, chrType)
#'                     print("extract telo CB done")
#'
#'                     hcb_cl = c(hcb_cl, heteroChromatinBoundaries$heteroBoundLeft)
#'                     hcb_cr = c(hcb_cr, heteroChromatinBoundaries$heteroBoundRight)
#'                     hcb_tl = c(hcb_tl, telomeres_boundaries$extrapolPhysPos_left)
#'                     hcb_tr = c(hcb_tr, telomeres_boundaries$extrapolPhysPos_right)
#'
#'                     RR_plot_100pc = plot_all(testChromosome, RR_object, genomeName ="DmelR6_DQC1_SimuDmelR6Chr2_100pc", toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize, plots_path)
#'                     # save_plot_as_png(RR_plot_100pc, plots_path, genomeName = "DmelR6", chrID)
#'                     print(RR_plot_100pc)
#'                 }
#'
#'                 print(c("frac = ", frac, "n = ", nrow(testChromosome)))
#'                 mkrDensity = c(mkrDensity, nrow(testChromosome))
#'
#'                 mhcb_cl = mean(abs(cl - hcb_cl))
#'                 mhcb_cr = mean(abs(cr - hcb_cr))
#'                 mhcb_tl = mean(abs(tl - hcb_tl))
#'                 mhcb_tr = mean(abs(tr - hcb_tr))
#'
#'                 decacl = c(decacl, mhcb_cl)
#'                 decacr = c(decacr, mhcb_cr)
#'                 decatl = c(decatl, mhcb_tl)
#'                 decatr = c(decatr, mhcb_tr)
#'             }
#'
#'             mkrDensity
#'             densityList = rlist::list.append(densityList, mkrDensity )
#'
#'             # library(ggplot2)
#'             # library(reshape2) ## for melt fct
#'
#'             fractions = testSizes*100  #[1:10]
#'             res = data.frame(fractions, telo_left = decatl, centro_left = decacl, centro_right = decacr, telo_right = decatr)
#'             d <- melt(res, id="fractions")
#'
#'             gg3 = ggplot(d, aes(x=fractions, y = value, color=variable)) +
#'                 geom_line(aes(linetype=variable), size=1) +
#'                 geom_point(aes(shape=variable, size=4)) +
#'                 scale_linetype_manual(values = c(2,1,1,2)) +
#'                 scale_shape_manual(values= c(15,17,17,15))
#'             ggsave(filename = paste0("chr_",chrID,  "_SimuResuts_1.png"), plot = gg3, path = plots_path)
#'
#'         tictoc::toc()
#'     }
#'
#' RR_plot_smallest_chr = plot_all(testChromosome, RR_object, genomeName ="dmel", toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize, plots_path)
#' print(RR_plot_smallest_chr)
#'
#' }
#' # ********************************************************************************************************************************
#' # ********************************************************************************************************************************
#' # ********************************************************************************************************************************
#'
#'
#'
#' # print(simu1ListOfPvals)
#'
#' # ----------------- simu2List : distribution of markers along the chromosome ------------------------------
#' # goodDataDistribution = FALSE
#' #
#' #     muMB = mean(refChromosome$mb)
#' #     muCM = mean(refChromosome$cm)
#' #     covMat = cov(refChromosome[4:3]) # matrix of 2*2  = covariance matrix cov(mb, cm)
#' #     sigmaMB = sqrt(covMat[1]) # sd(refChromosome$mb)
#' #     sigmaCM = sqrt(covMat[4]) #  sd(refChromosome$cm)
#' #
#' #     n = refChromosomeSize # number of data needed to generate the mvnormal distribution
#' #
#' #     testParSigMB = c(2*sigmaMB, 3*sigmaMB)
#' #     testParSigCM = c(2*sigmaCM, 3*sigmaCM)
#' #     testParMuMB = c(2*muMB, 0.5*muMB)
#' #     testParMuCM = c(2*muCM, 0.5*muCM)
#' #
#' #     simu2List = list()  # simulated chromosomes baesd on distribution
#' #     simu2ListOfPvals = c()
#' #
#' #     mu = c(muMB, muCM) # vector of means
#' #     sigmaMat = covMat
#' #     for (sigMBpar in testParSigMB) {
#' #         sigmaMat[1] = sigMBpar^2
#' #         print(mu)
#' #         print(sigmaMat)
#' #         normalDistribution = MASS::mvrnorm(n, mu, sigmaMat, empirical = T) # multivariate normal distribution
#' #         normalDistribution[normalDistribution<0] = 0
#' #         simu2List = rlist::list.append(simu2List, normalDistribution)
#' #         chi2 = chisq.test(refChromosomeAsMatrix, p = normalDistribution, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#' #         simu2ListOfPvals = c(simu2ListOfPvals, chi2$p.value)
#' #         png(filename = paste0(getwd(),"/DQC/", "SimulatedChromosome-sigMBpar = ", round(sigMBpar,2), ".png"))
#' #         plot(normalDistribution)
#' #         dev.off()
#' #     }
#' #
#' #     for (sigCMpar in testParSigCM) {
#' #         sigmaMat[4] = sigCMpar^2
#' #         print(mu)
#' #         print(sigmaMat)
#' #         normalDistribution = MASS::mvrnorm(n, mu, sigmaMat, empirical = T) # multivariate normal distribution
#' #         normalDistribution[normalDistribution<0] = 0
#' #         simu2List = rlist::list.append(simu2List, normalDistribution)
#' #         chi2 = chisq.test(refChromosomeAsMatrix, p = normalDistribution, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#' #         simu2ListOfPvals = c(simu2ListOfPvals, chi2$p.value)
#' #         png(filename = paste0(getwd(),"/DQC/", "SimulatedChromosome-sigMBpar = ", round(sigMBpar,2), ".png"))
#' #         plot(normalDistribution)
#' #         dev.off()
#' #     }
#' #
#' #     sigmaMat = covMat
#' #     for (muMBpar in testParMuMB) {
#' #         mu = c(muMBpar, muCM)
#' #         print(mu)
#' #         print(sigmaMat)
#' #         normalDistribution = MASS::mvrnorm(n, mu, sigmaMat, empirical = T) # multivariate normal distribution
#' #         normalDistribution[normalDistribution<0] = 0
#' #         simu2List = rlist::list.append(simu2List, normalDistribution)
#' #         chi2 = chisq.test(refChromosomeAsMatrix, p = normalDistribution, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#' #         simu2ListOfPvals = c(simu2ListOfPvals, chi2$p.value)
#' #         png(filename = paste0(getwd(),"/DQC/", "SimulatedChromosome-muCMpar = ", round(muCMpar,2), ".png"))
#' #         plot(normalDistribution)
#' #         dev.off()
#' #     }
#' #
#' #     for (muCMpar in testParMuCM) {
#' #         mu = c(muMB, muCMpar)
#' #         print(mu)
#' #         print(sigmaMat)
#' #         normalDistribution = MASS::mvrnorm(n, mu, sigmaMat, empirical = T) # multivariate normal distribution
#' #         normalDistribution[normalDistribution<0] = 0
#' #         simu2List = rlist::list.append(simu2List, normalDistribution)
#' #         chi2 = chisq.test(refChromosomeAsMatrix, p = normalDistribution, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#' #         simu2ListOfPvals = c(simu2ListOfPvals, chi2$p.value)
#' #         png(filename = paste0(getwd(),"/DQC/", "SimulatedChromosome-muCMpar = ", round(muCMpar,2), ".png"))
#' #         plot(normalDistribution)
#' #         dev.off()
#' #     }
#' #     print(simu2ListOfPvals)
#' #
#' #     saveRDS(simu1List,paste0(getwd(),"/DQC/", "simu1List.RDS"))
#' #     saveRDS(simu1ListOfPvals,paste0(getwd(),"/DQC/", "simu1ListOfPvals.RDS"))
#' #     saveRDS(simu2List,paste0(getwd(),"/DQC/", "simu2List.RDS"))
#' #     saveRDS(simu2ListOfPvals,paste0(getwd(),"/DQC/", "simu2ListOfPvals.RDS"))
#' #
#' #
#' #     # if(chi2Pvalue > 0.05){
#' #     #     goodDataDistribution = TRUE
#' #     #     print("OK! Good data distribution !")
#' #     # }else{
#' #     #     print("Oups! Not a good data distribution !")
#' #     # }
#' #
#' #
#' #     #### Begin Brec_chromosome()--------------------------------
#' #         chrID = 2
#' #         chromosome = testChromosome
#' #         RR_object = estimate_recombination_rates(chromosome)
#' #         print("RR done")
#' #         chrType = get_chromosome_type(chromosome, RR_object$regDr)
#' #         # RR_object2 = RR_object
#' #
#' #         R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
#' #         print("R2 done")
#' #
#' #         # if(using_slidingWindowApproach_for_HCB) {
#' #         print("Extracting CB for this chromosome ...")
#' #         heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType)
#' #         print("extract centroCB done")
#' #         telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType)
#' #         print("extract telo CB done")
#' #         RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType)
#' #         print("extrapolation done")
#' #
#' #         # }else{
#' #         #     heteroChromatinBoundaries = data.frame( heteroBoundLeft = 0, heteroBoundRight = 0, swSize = 0)
#' #         #     telomeres_boundaries = data.frame(index_minR2_left = 0, extrapolPhysPos_left = 0, index_minR2_right = 0, extrapolPhysPos_right = 0)
#' #         #     print("No chromatin boundaraies estimation and no extrapolation !!! ")
#' #         # }
#' #
#' #         # RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
#' #         # HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)
#' #
#' #         # cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")
#' #
#' #         # RR_plot = plot_steps_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize)
#' #
#' #         # save interactive plot as html
#' #         # saveWidget(RR_plot, paste0(plots_path,"RR_plot.html"))
#' #
#' #         # saveRDS(RR_plot,paste0(plots_path,"RR_plot.RDS"))
#' #
#' #         # display Stats + save them in external file.txt
#' #         # generate_stats(chromosome, RR_object, HCB_fileContent, do_cleaning)
#' #
#' #         # BrecResultsList = list(RR_plot, RR_object, heteroChromatinBoundaries, telomeres_boundaries)
#' #
#' #
#' #     ### End Brec_chromosome
#' #     # }
#' #     # print(simu1List)
#' #
#' #
#' #     # ---------------------------------  start DQC -----------------------------------------------
#' #
#' #     # ---------------------------------  start DQC -----------------------------------------------
#' # }
#' #
#' # # data_quality_control(refChromosome)
#' #
#' #     # refChromosome = testChromosome # DmelR6_chr2
#' #     # testChromosomeSize = nrow(testChromosome)
#' #
#' #     # dataNbrMinThreshold = nrow(testChromosome) #  5% of the original refChromosome length
#' #     # cat("dataNbrMinThreshold = " , dataNbrMinThreshold)
#' #     #
#' #     # if (nrow(testChromosome) >   ){
#' #     #     enoughData = TRUE
#' #     #     print("OK! Enough data points")
#' #     #     goodDataDistribution = data_distribution_test(testChromosome) # testing data distribution quality with Chi-2 test, take p-value > 0.05
#' #     # }else{
#' #     #     print("Oups! Not enough data points ")
#' #     #     chi2Pvalue = 0
#' #     # }
#' #     # if(enoughData & goodDataDistribution){
#' #     #     goodDataQuality = TRUE
#' #     # }else{
#' #     #     goodDataQuality = FALSE
#' #     # }
#' #     # return(enoughData) # boolean
#' # # }
