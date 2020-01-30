# # Dev_paper_comparing_methods_test_with_chisquare
#
# Dev_paper_comparing_methods_test_with_chisquar_only_Comeron <- function(chromosome, refData, testData) {
#
#     # here, test and ref data are both vectors of recombination rate values to compare
#     # for example, testData = RR_using_polynomial and refData = RR_using_Loess0.25
#
#     # uplad required libraries---------
#     pacman::p_load(Brec, na.tools, ggplot2, tidyverse)
#
#     # set input and output path---------
#     plots_path = "plots"
#     comeronDataFilesPath = "~/ProBook/_ISEM/Thesis_work/Recombination_BREC_project_thesis_part_1/BREC_paper_writing/Oxford_Bioinformatics_submission/December_comeback/New_validation_and_comparison_tests/my_versions/Comeron_tables/"
#
#     # upload DmelR5 data (in arms) and get included chromosomes IDs ---------------------------------------
#     inputData = read.csv(file = "data/BREC_byArm_MB_marey_map_R5.36_removeOutliers.csv", header = T, sep = "\t")
#     # chrList = get_list_of_chromosomes(inputData)
#     newChrList = c("X", "2L", "2R", "3L", "3R")
#
#     # get Comeron chromosomal arms --------------------------------------
#     comeron_2L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2L.txt"), header = F, sep = "\t")
#     comeron_2R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2R.txt"), header = F, sep = "\t")
#     comeron_3L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3L.txt"), header = F, sep = "\t")
#     comeron_3R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3R.txt"), header = F, sep = "\t")
#     comeron_X = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chrX.txt"), header = F, sep = "\t")
#
#     # ===================================================================================================================================
#     # build wholeGenome_Comeron ========================================================
#     wholeGenome_Comeron = data.frame()
#
#     lastMB_X = comeron_X$V1[nrow(comeron_X)]
#     adjusted_comeron_2L = comeron_2L
#     adjusted_comeron_2L$V1 = adjusted_comeron_2L$V1 + lastMB_X
#
#     lastMB_adj2L = adjusted_comeron_2L$V1[nrow(adjusted_comeron_2L)]
#     adjusted_comeron_2R = comeron_2R
#     adjusted_comeron_2R$V1 = adjusted_comeron_2R$V1 + lastMB_adj2L
#
#     lastMB_adj2R = adjusted_comeron_2R$V1[nrow(adjusted_comeron_2R)]
#     adjusted_comeron_3L = comeron_3L
#     adjusted_comeron_3L$V1 = adjusted_comeron_3L$V1 + lastMB_adj2R
#
#     lastMB_adj3L = adjusted_comeron_3L$V1[nrow(adjusted_comeron_3L)]
#     adjusted_comeron_3R = comeron_3R
#     adjusted_comeron_3R$V1 = adjusted_comeron_3R$V1 + lastMB_adj3L
#
#     wholeGenome_Comeron = rbind(comeron_X, adjusted_comeron_2L, adjusted_comeron_2R, adjusted_comeron_3L, adjusted_comeron_3R)
#
#     chrSeparators = c(lastMB_X, lastMB_adj2L, lastMB_adj2R,lastMB_adj3L)/10^6
#
#     # compute mean_RR(WG_comeron)------------
#     meanRR_wholeGenome_Comeron = mean(wholeGenome_Comeron$V2)
#     print(paste0("meanRR_wholeGenome_Comeron = ", meanRR_wholeGenome_Comeron))
#
#     gg_wholeGenome_Comeron = ggplot(data=wholeGenome_Comeron) + geom_line(aes(x=V1/10^6, y=V2, color="red")) #, breaks = c(0:5:120) )
#     # gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_hline(aes(yintercept = meanRR_wholeGenome_Comeron), color = "blue", linetype = "dashed")
#     gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_vline(xintercept = chrSeparators, color = "black") #, linetype = "twodash"
#     # gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_text(aes(y = meanRR_wholeGenome_Comeron, label = "mean")) # not working yet!
#     gg_wholeGenome_Comeron
#
#     ## ====Langley data =================================================================
#     langleyData <- read.csv(file = "datasets/Langley_TableS12.txt", skip= 3, sep="\t", header=TRUE)
#
#     langleyData_X = langleyData[ , c(1:4)]
#     langleyData_2L = langleyData[ , c(6:9)]
#     langleyData_2R = langleyData[ , c(11:14)]
#     langleyData_3L = langleyData[ , c(16:19)]
#     langleyData_3R = langleyData[ , c(21:24)]
#
#     for (chrID in newChrList) {
#         if(chrID == "X"){
#             langleyData = langleyData_X
#         }else if(chrID == "2L"){
#             langleyData = langleyData_2L
#         }else if(chrID == "2R"){
#             langleyData = langleyData_2R
#         }else if(chrID == "3L"){
#             langleyData = langleyData_3L
#         }else if(chrID == "3R"){
#             langleyData = langleyData_3R
#         }
#
#         names(langleyData)[c(1:4)] = c("Cytogenetic", "cM", "end", "M.bp")
#
#         # plotting -------------
#         p = ggplot(langleyData, aes(x = end/10^6)) + geom_point(aes( y = cM, color = "Genetic markers"))
#
#         # adding the relative humidity data, transformed to match roughly the range of the temperature
#         p = p + geom_line(aes(y = M.bp*10^9, color = "Recombination rate"))
#
#         # now adding the secondary axis, following the example in the help file ?scale_y_continuous
#         # and, very important, reverting the above transformation
#         p = p + scale_y_continuous(sec.axis = sec_axis(~./10, name = "Recombination rate cM/Mb")) #, limits = c(-1, 8))
#
#         # modifying colours and theme options
#         p = p + scale_colour_manual(values = c("black", "red"))
#         p = p + labs(y = "Genetic distance (cM)",
#                      x = "Physical location (Mb)",
#                      colour = paste(chrID, " - Langley data"))
#         p = p + theme(legend.position = c(0.1, 0.9))
#         p
#
#         ggsave(filename = paste0("chr_",chrID,"_Langley_", ".png") #
#                , plot = p
#                , path = plots_path #plot = last_plot()
#                , height = 8
#                , width = 12
#         )
#
#     }
#
#     ## ==================================================================================
#     for (loess_span in c(0.15, 0.25, 0.5, 0.75,1)) {
#
#         loess_span = 0.15
#         # loess_span =
#         phenocamr::optimal_span(y=chromosome$cm, x=chromosome$mb ,step =0.05, plot = F)
#         print(loess_span)
#         newChrList = c("X", "2L", "2R", "3L", "3R")
#
#         ## initialising all wholeGenome variables--------------
#         # wholeGenome_Comeron = data.frame()
#         wholeGenome_RRvect= c()
#         wholeGenome_MBvect = c()
#         chrId_vect = c()
#         chrSeparators_RR = c()
#         chrSeparators_c = c()
#
#         ##------loop for all chromosomes  -----------------------------------
#         chrID = "X"
#         chrID = "2L"
#
#         # for (chrID in newChrList) {
#         #
#         #    for (loess_span in c(0.15, 0.25, 0.5, 0.75,1)) {
#         ## run BREC code chunks to get loess RR estimates ----------------------------
#         chromosome = get_chromosome_from_inputData(inputData, chrID)
#         chrLength = nrow(chromosome)
#         print(paste("======== chr in process =>", chrID, "==== chrLength", chrLength, "========="))
#
#         # get comeronData for corresponding chr -----------------------------------
#         # if(chrID == "X"){
#         #     comeronData = comeron_X
#         # }else if(chrID == "2L"){
#         #     comeronData = comeron_2L
#         # }else if(chrID == "2R"){
#         #     comeronData = comeron_2R
#         # }else if(chrID == "3L"){
#         #     comeronData = comeron_3L
#         # }else if(chrID == "3R"){
#         #     comeronData = comeron_3R
#         # }
#
#         # get comeronData for corresponding chr -----------------------------------
#         # maxMb = max(comeronData$V1) ##====>>> ERROR fixed for same bins number as in comeron !!  # maxMb = max(chromosome$mb)
#         # chr_100kb_bins = seq(from = 1, by = 10^5, to = maxMb) # by=10^5 means one bin per 100kb
#         # MBvect = chr_100kb_bins/10^6 # convert into Mb required to compute RR (cM/MB)
#         # print(paste("length(MBvect) = ", length(MBvect)))
#         # print(c("tail(MBvect) = ", tail(MBvect)))
#         #
#
#         ## ==================================================================================
#         maxMb = max(langleyData$end)
#         chr_200kb_bins = seq(from = 1, by = 2*10^5, to = maxMb+10^5) # by=10^5 means one bin per 100kb
#         MBvect = chr_200kb_bins/10^6 # convert into Mb required to compute RR (cM/MB)
#         print(paste("length(MBvect) = ", length(MBvect)))
#         print(c("tail(MBvect) = ", tail(MBvect)))
#
#
#         ## ============================================================================================================================
#         MBvect25 = langleyData$end # use directly the physical map of LangleyData to generate BREC estimates on the same positions
#         print(paste("length(MBvect25) = ", length(MBvect25)))
#         print(c("tail(MBvect25) = ", tail(MBvect25)))
#
#         MBvect = MBvect25/10^6
#         # estimating RR for needed bins -------------
#         x = chromosome$mb
#         y = chromosome$cm
#
#         newLangModel = langleyData
#
#         x = langleyData$end/10^6
#         y = langleyData$cM
#
#
#         model = loess(y ~ x, span = loess_span, degree = 2)#, na.action = na.exclude
#         regFn = predict(model, MBvect)  ## needed only for plotting
#         MBvectplus1 = MBvect+1
#         MBvectminus1 = MBvect-1
#         RR_predectedplus1 = predict(model, MBvectplus1)
#         RR_predectedminus1 = predict(model, MBvectminus1)
#
#         na_indecies_RR_predectedplus1 = which_na(RR_predectedplus1)
#
#         if(!is_empty(na_indecies_RR_predectedplus1)){
#             for (elem in na_indecies_RR_predectedplus1) {
#                 if(elem > length(MBvect)/2){
#                     RR_predectedplus1[elem] = max(RR_predectedplus1, na.rm = TRUE)
#                 }else{
#                     RR_predectedplus1[elem] = min(RR_predectedplus1, na.rm = TRUE)
#                 }
#             }
#         }
#
#         na_indecies_RR_predectedminus1 = which_na(RR_predectedminus1)
#
#         if(!is_empty(na_indecies_RR_predectedminus1)){
#             for (elem2 in na_indecies_RR_predectedminus1) {
#                 if(elem2 > length(MBvect)/2){
#                     RR_predectedminus1[elem2] = max(RR_predectedminus1, na.rm = TRUE)
#                 }else{
#                     RR_predectedminus1[elem2] = min(RR_predectedminus1, na.rm = TRUE)
#                 }
#             }
#         }
#         #
#         #             which(na_indecies_RR_predectedminus1> length(MBvect)/2)
#         #
#         #             RR_predectedminus1 = na.replace(], max(RR_predectedminus1, na.rm = TRUE)
#         #
#         #             na_indicies_RR_predectedminus1 = which_na(RR_predectedminus1)
#         #             if(!is_empty(na_indecies_RR_predectedminus1)){
#         #                 RR_predectedminus1[na_indecies_RR_predectedminus1 > length(MBvect)/2] = na.replace(RR_predectedminus1, max(RR_predectedminus1, na.rm = TRUE)) # max(5, 'na') => 'na'
#         #                 RR_predectedminus1[na_indecies_RR_predectedminus1 < length(MBvect)/2] = na.replace(RR_predectedminus1, min(RR_predectedminus1)) # min(5, 'na') => 5
#         #             }
#
#         regDr <- mapply(function(x1, x2, y1, y2) {(y2-y1)/(x2-x1)}, MBvectminus1, MBvectplus1, RR_predectedminus1, RR_predectedplus1) # {round((y2-y1)/(x2-x1 ), 2)}
#         RRvect = regDr
#
#         RRvect
#         length(RRvect[RRvect<0])
#
#         comeronData$V2 = comeronData$V2*10^6
#         RRvect = RRvect*10^6
#
#         # set negative RR values to zero (only to avoid chi-sq test errors)-------
#         # RRvect[RRvect<0] = 0
#
#
#         # which(RRvect!=regDr) # same as length(which(regDr<0))
#         # length(which(RRvect!=regDr))
#
#         print("----------->> RRvect is ready !! ---------------")
#
#         my_chisq_test = chisq.test(RRvect, langleyData$M.bp)
#         my_chisq_test
#
#
#         install.packages("paleoMAS")
#         library(paleoMAS)
#
#         akaike.l(chromosome$mb, chromosome$cm, interval = c(0.15, 0.15, 0))
#         akaike.l(langleyData$end, langleyData$M.bp, interval = c(0.15, 0.15, 0))
#
#         # newlomodel <-loess(chromosome$cm ~ chromosome$mb, span = loess_span, degree = 2)
#         # newlomodel <- loess.as(cbind(x21, x22), y2, plot=TRUE))
#         # AIC(model) # => not working beacause not glm
#
#         # build newRR_object to use for plotting ----------------------------------
#         newRR_object = data.frame(MBvect, RRvect)
#
#         # plotting ----------------------------------------------------------------
#         chi2 = my_chisq_test
#
#         # myggPlot = ggplot(comeronData) + geom_line(aes(x = V1/10^6, y = V2), color = 'red') + labs(title = paste0("chr ",chrID) , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#         # ggcomparison = myggPlot + geom_line(data = newRR_object, aes(x = MBvect, y = RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#         #                           , label = paste0("Loess_span = ", loess_span, "\n"
#         #                                , "P-value = ", round(chi2$p.value, 4), "\n"
#         #                                , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#         #                                , "df = ", chi2$parameter
#         #                           ))
#
#         myggPlot = ggplot(langleyData) + geom_line(aes(x = langleyData$end/10^6, y = langleyData$M.bp*10^8), color = 'red') + labs(title = paste0("chr ",chrID) , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#         ggcomparison = myggPlot + geom_line(data = newRR_object, aes(x = MBvect, y = RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#                                                                                                                               , label = paste0("Loess_span = ", loess_span, "\n"
#                                                                                                                                                , "P-value = ", round(chi2$p.value, 4), "\n"
#                                                                                                                                                , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#                                                                                                                                                , "df = ", chi2$parameter
#                                                                                                                               ))
#         ggcomparison
#
#         ggsave(filename = paste0("chr_",chrID,"_chi2Pvalue_", round(chi2$p.value, 4) ,"_comparison_BREC_Loess_", loess_span,"_pc_vs_Langley", ".png") #
#                , plot = ggcomparison
#                , path = plots_path #plot = last_plot()
#                , height = 8
#                , width = 12
#         )
#
#
#         #   }
#         #
#         # }
#
#         # both tests ==========================================================
#
#         hist(RRvect)
#         hist(comeronData$V2)
#
#         sd(RRvect)
#         sd(comeronData$V2)
#
#         summary(RRvect)
#         summary(comeronData$V2)
#
#         # rug(comeronData$V2, col='red')
#
#         # start Fisher's testing ==========================================================
#         my_fisher_test = fisher.test( RRvect , comeronData$V2, simulate.p.value=TRUE)
#         #  , rescale.p = TRUE ) # to rescale p to sum up to 1
#         my_fisher_test
#
#         my_fisher_test_inv = fisher.test(log(comeronData$V2), log(RRvect) , simulate.p.value=TRUE)
#         #  , rescale.p = TRUE ) # to rescale p to sum up to 1
#         my_fisher_test_inv
#
#         # start chi-square testing ==========================================================
#         my_chisq_test = chisq.test(RRvect, p=comeronData$V2  , rescale.p = TRUE ) # to rescale p to sum up to 1
#         my_chisq_test
#
#         my_chisq_test_inv = chisq.test(comeronData$V2, p=RRvect  , rescale.p = TRUE) # to rescale p to sum up to 1
#         my_chisq_test_inv
#
#         # chisq for WG on gg3 data----------------------------------
#         my_chisq_test_wg_gg3 = chisq.test(wholeGenome_newRR_object22$wholeGenome_RRvect22, p=wholeGenome_Comeron$V2  , rescale.p = TRUE ) # to rescale p to sum up to 1
#         my_chisq_test_wg_gg3
#
#
#         # build newRR_object to use for plotting ----------------------------------
#         newRR_object = data.frame(MBvect, RRvect)
#
#         # plotting ----------------------------------------------------------------
#         chi2 = my_chisq_test
#         myggPlot = ggplot(comeronData) + geom_line(aes(x = V1/10^6, y = V2), color = 'red') + labs(title = paste0("chr ",chrID) , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#         ggcomparison = myggPlot + geom_line(data = newRR_object, aes(x = MBvect, y = RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#                                                                                                                               , label = paste0("Loess_span = ", loess_span, "\n"
#                                                                                                                                                , "P-value = ", round(chi2$p.value, 4), "\n"
#                                                                                                                                                , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#                                                                                                                                                , "df = ", chi2$parameter))
#         ggcomparison
#
#         ggsave(filename = paste0("chr_",chrID, "_chi2Pvalue_", round(chi2$p.value, 4) , "_comparison_BREC_Loess_", loess_span,"_pc_vs_Comeron", ".png")
#                , plot = ggcomparison
#                , path = plots_path #plot = last_plot()
#                , height = 8
#                , width = 12
#         )
#
#         ## --------------------------------------------------------------------------------------------------------------------------------------------
#         ## build wholeGenome for BREC by concatenating all chr after adjusting physical distances (else they will overlap)-----------------------------
#
#
#         # MBvect = MBvect*10^6
#         wholeGenome_RRvect = c(wholeGenome_RRvect, RRvect)
#
#
#         # gg_test = ggplot(data=data.frame(adjusted_current_MBvect, wholeGenome_RRvect)) + geom_line(aes(x=adjusted_current_MBvect, y=wholeGenome_RRvect), color="darkgreen")
#         gg_test
#
#         MBvect22 = seq(from = 1, by=10^4 ,to = length(wholeGenome_RRvect)*10^6 )
#         length(MBvect22)
#         length(wholeGenome_RRvect)
#         plot(MBvect22, wholeGenome_RRvect[1:1140], type="l")
#         wholeGenome_RRvect22 = wholeGenome_RRvect[1:1140]
#         wholeGenome_newRR_object22 = data.frame(MBvect22, wholeGenome_RRvect22)
#         print(paste0("wholeGenome_RRvect22 = ",mean(wholeGenome_RRvect22)))
#
#         gg_wholeGenome_newRR_object22 = ggplot(data=wholeGenome_newRR_object22) + geom_line(aes(x=MBvect22, y=wholeGenome_RRvect22), color="darkgreen")
#         gg_wholeGenome_newRR_object22
#
#         # chrId_vect = c(chrId_vect, chrID)
#         # wholeGenome_Comeron = rbind(wholeGenome_Comeron, adjusted_comeron)
#
#         wholeGenome_MBvect = c(wholeGenome_MBvect, adjusted_current_MBvect)
#         # chrSeparators_c = c(chrSeparators_c, lastMB_adj_comeron)
#         chrSeparators_RR = c(chrSeparators_RR, lastMB_current_MBvect)
#
#         # build wholeGenome_newRR_object to use for plotting -------------------------------------------
#         wholeGenome_newRR_object = data.frame(wholeGenome_MBvect, wholeGenome_RRvect)
#
#         # compute meanRR(wholeGenome_RRvect)------------------
#         print(paste0("wholeGenome_RRvect = ",mean(wholeGenome_RRvect)))
#
#     }#--END----loop for all chromosomes = whole genome-----------------------------------
#
#
#     # chrSeparators1 = c(lastMB_X, lastMB_adj2L, lastMB_adj2R,lastMB_adj3L)/10^6 ## only for plotting
#
#
#     # STOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
#
#     # plotting ===============================================================================================================================
#
#     # one chr -----------------------
#     gg = gg_wholeGenome_Comeron + geom_line(data=wholeGenome_newRR_object22, aes(x=MBvect22, y=wholeGenome_RRvect22), color="darkgreen")
#     gg33
#
#
#     # WG -----------------------
#     gg33 = gg_wholeGenome_Comeron + geom_line(data=wholeGenome_newRR_object22, aes(x=MBvect22, y=wholeGenome_RRvect22), color="darkgreen")
#     gg33
#
#
#
#     # gg_wholeGenome_Comeron = ggplot(data=wholeGenome_Comeron) + geom_line(aes(x=V1/10^6, y=V2, color="red")) #, breaks = c(0:5:120) )
#
#
#     gg_wholeGenome_newRR_object = ggplot(data=wholeGenome_newRR_object) + geom_line(aes(x=wholeGenome_MBvect, y=wholeGenome_RRvect), color="darkgreen")
#     gg_wholeGenome_newRR_object
#
#     # plot within the same grid ---------------------------------------------------------------------
#     gg_grid = gridExtra::grid.arrange(gg_wholeGenome_newRR_object22, gg_wholeGenome_Comeron, ncol=1)
#     gg_grid
#
#
#     ### whole genome ====================================================================
#     wholeGenome_Comeron =  comeronData[c(1:length(wholeGenome_RRvect)),]
#     wholeGenome_newRR_object = data.frame(wholeGenome_MBvect, wholeGenome_RRvect)
#     chi2 = chisq.test(wholeGenome_Comeron$V2, p=wholeGenome_RRvect, rescale.p = TRUE) # to rescale p to sum up to 1
#     chi2
#     wholeGenome_myggPlot = ggplot(wholeGenome_Comeron) + geom_line(aes(x = V1/10^6, y = V2), color = 'red') + labs(title = paste0("whole_genome") , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#     wholeGenome_ggcomparison =  wholeGenome_myggPlot + geom_line(data = wholeGenome_newRR_object, aes(x = wholeGenome_MBvect, y = wholeGenome_RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#                                                                                                                                                                                        , label = paste0("Loess_span = ", loess_span, "\n"
#                                                                                                                                                                                                         , "P-value = ", round(chi2$p.value, 4), "\n"
#                                                                                                                                                                                                         , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#                                                                                                                                                                                                         , "df = ", chi2$parameter))
#     wholeGenome_ggcomparison
#
#     ggsave(filename = paste0("whole_genome", "_chi2Pvalue_", round(chi2$p.value, 4) , "_comparison_BREC_Loess_", loess_span,"_pc_vs_Comeron", ".png")
#            , plot =  wholeGenome_ggcomparison
#            , path = plots_path #plot = last_plot()
#            , height = 8
#            , width = 12
#     )
#     # =====================================================================================
# }
#
#
#
#
#     length(wholeGenome_MBvect[is.na(wholeGenome_MBvect) == T])
#     length(wholeGenome_RRvect[is.na(wholeGenome_RRvect) == T])
#
#
#
#     # chi2_1inv = chisq.test(comeronData, RRvect) #, rescale.p = TRUE) # to rescale p to sum up to 1
#     # chi2_1inv  # ==> chi-sq(a,b) = chi-sq(b,a)
#
#     # chi2_1_inv = chisq.test(RR_object_loess0.75$regDr, p = RR_object_polynomial$regDr, rescale.p = TRUE) # to rescale p to sum up to 1
#     # chi2_1_inv
#
#     # chi2_2 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#     # chi2_3 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#     #
#     # chi2_4 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#     # chi2_5 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#
#
#     print(c("chi2_1 = ", chi2_1, "chi2Pvalue_1 = ", chi2_1$p.value ))
#     print(c("chi2_2 = ", chi2_2, "chi2Pvalue_2 = ", chi2_2$p.value ))
#     print(c("chi2_3 = ", chi2_3, "chi2Pvalue_3 = ", chi2_3$p.value ))
#     print(c("chi2_4 = ", chi2_4, "chi2Pvalue_4 = ", chi2_4$p.value ))
#     print(c("chi2_5 = ", chi2_5, "chi2Pvalue_5 = ", chi2_5$p.value ))
#
#     significativellyDifferent = FALSE
#
#
#     # testDataAsMatrix = as.matrix.data.frame(subset(testData, select=c("cm", "mb")), rownames.force = NA)
#     # refDataAsMatrix = as.matrix.data.frame(subset(refData, select=c("cm", "mb")), rownames.force = NA)
#     # chi2 = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#
#
#
#     chi2 = chisq.test(testData, p = refData, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#     print(chi2)
#     chi2Pvalue = chi2$p.value
#     print(chi2Pvalue)
#
#     if(chi2Pvalue > 0.05){
#         significativellyDifferent = TRUE
#         print("OK! not significativelly Different .. statistically significant !?")
#     }else{
#         print("Oups! significativelly different")
#     }
#
#     # testDataAsMatrix = as.matrix.data.frame(RRvect, rownames.force = NA)
#     # refDataAsMatrix = as.matrix.data.frame(comeronData, rownames.force = NA)
#     # chi2_test = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE)
#
#     return(significativellyDifferent) # boolean
# }
#
# ### from untitled filr (oprn by mistake I guess...)
#
# # here, test and ref data are both vectors of recombination rate values to compare
# # for example, testData = RR_using_polynomial and refData = RR_using_Loess0.25
#
# # getwd()
#
#
# pacman::p_load(Brec, na.tools, ggplot2, tidyverse)
#
# plots_path = "plots"
# comeronDataFilesPath = "~/ProBook/_ISEM/Thesis_work/Recombination_BREC_project_thesis_part_1/BREC_paper_writing/Oxford_Bioinformatics_submission/December_comeback/New_validation_and_comparison_tests/my_versions/Comeron_tables/"
# # Comeron chromosomal arms --------------------------------------
# comeron_2L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2L.txt"), header = F, sep = "\t")
# comeron_2R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr2R.txt"), header = F, sep = "\t")
# comeron_3L = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3L.txt"), header = F, sep = "\t")
# comeron_3R = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chr3R.txt"), header = F, sep = "\t")
# comeron_X = read.csv(file = paste0(comeronDataFilesPath, "/originalFileFromAnna/Comeron_100kb_chrX.txt"), header = F, sep = "\t")
#
# # DmelR5 data arms ---------------------------------------
# inputData = read.csv(file = "data/BREC_byArm_MB_marey_map_R5.36_removeOutliers.csv", header = T, sep = "\t")
# # chrList = get_list_of_chromosomes(inputData)
# newChrList = c("X", "2L", "2R", "3L", "3R")
#
# ## build wholeGenome_Comeron ========================================================
# wholeGenome_Comeron = data.frame()
#
# lastMB_X = comeron_X$V1[nrow(comeron_X)]
# adjusted_comeron_2L = comeron_2L
# adjusted_comeron_2L$V1 = adjusted_comeron_2L$V1 + lastMB_X
#
# lastMB_adj2L = adjusted_comeron_2L$V1[nrow(adjusted_comeron_2L)]
# adjusted_comeron_2R = comeron_2R
# adjusted_comeron_2R$V1 = adjusted_comeron_2R$V1 + lastMB_adj2L
#
# lastMB_adj2R = adjusted_comeron_2R$V1[nrow(adjusted_comeron_2R)]
# adjusted_comeron_3L = comeron_3L
# adjusted_comeron_3L$V1 = adjusted_comeron_3L$V1 + lastMB_adj2R
#
# lastMB_adj3L = adjusted_comeron_3L$V1[nrow(adjusted_comeron_3L)]
# adjusted_comeron_3R = comeron_3R
# adjusted_comeron_3R$V1 = adjusted_comeron_3R$V1 + lastMB_adj3L
#
#
# wholeGenome_Comeron = rbind(comeron_X, adjusted_comeron_2L, adjusted_comeron_2R, adjusted_comeron_3L, adjusted_comeron_3R)
#
# chrSeparators = c(lastMB_X, lastMB_adj2L, lastMB_adj2R,lastMB_adj3L)/10^6
#
# meanRR_wholeGenome_Comeron = mean(wholeGenome_Comeron$V2)
# print(paste0("meanRR_wholeGenome_Comeron = ", meanRR_wholeGenome_Comeron))
#
# gg_wholeGenome_Comeron = ggplot(data=wholeGenome_Comeron) + geom_line(aes(x=V1/10^6, y=V2, color="red")) #, breaks = c(0:5:120) )
# gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_hline(aes(yintercept = meanRR_wholeGenome_Comeron), color = "blue", linetype = "dashed")
# gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_vline(xintercept = chrSeparators, color = "black") #, linetype = "twodash"
# # gg_wholeGenome_Comeron = gg_wholeGenome_Comeron + geom_text(aes(y = meanRR_wholeGenome_Comeron, label = "mean"))
# gg_wholeGenome_Comeron
#
# gridExtra::grid.arrange(gg_wholeGenome_Comeron,gg_wholeGenome_Comeron, ncol=1)
#
# ## ==================================================================================
# for (loess_span in c(0.15, 0.25, 0.5, 0.75,1)) {
#
#     loess_span = 0.7
#
#     wholeGenome_Comeron = data.frame()
#     wholeGenome_RRvect= c()
#     wholeGenome_MBvect = c()
#     chrId_vect = c()
#     chrSeparators_RR = c()
#     chrSeparators_c = c()
#     # chrID = "2R"
#     for (chrID in newChrList) {
#
#         if(chrID == "X"){
#             comeronData = comeron_X
#         }else if(chrID == "2L"){
#             comeronData = comeron_2L
#         }else if(chrID == "2R"){
#             comeronData = comeron_2R
#         }else if(chrID == "3L"){
#             comeronData = comeron_3L
#         }else if(chrID == "3R"){
#             comeronData = comeron_3R
#         }
#
#
#         print(c("=========== chr in process ====> ", chrID, "==================="))
#         testChromosome = get_chromosome_from_inputData(inputData, chrID)
#         chrLength = nrow(testChromosome)
#         print(c("chrLength", chrLength))
#
#         MB = testChromosome$mb
#         cM = testChromosome$cm
#
#         # generating needed bins
#         maxMb = max(MB)
#         chr_100kb_bins = seq(from = 1, by = 100000, to = maxMb*1000000) # *1000000 100kb
#
#         # estimating RR for needed bins
#
#         model = loess(cM ~ MB, span = loess_span, degree = 2)#, na.action = na.exclude
#         MBvect = chr_100kb_bins/10^6
#         regFn = predict(model, MBvect)  ## needed only for plotting
#         MBvectplus1 = MBvect+1
#         MBvectminus1 = MBvect-1
#         RR_predectedplus1 = predict(model, MBvectplus1)
#         RR_predectedplus1 = na.replace(RR_predectedplus1, max(RR_predectedplus1, na.rm = TRUE))
#         RR_predectedminus1 = predict(model, MBvectminus1)
#         RR_predectedminus1 = na.replace(RR_predectedminus1, min(RR_predectedminus1, na.rm = TRUE))
#         regDr <- mapply(function(x1, x2, y1, y2) {(y2-y1)/(x2-x1 )}, MBvectminus1, MBvectplus1, RR_predectedminus1, RR_predectedplus1)
#         # {round((y2-y1)/(x2-x1 ), 2)}
#         RRvect = regDr
#         RRvect[RRvect<0] = 0
#
#         # print("Loess model is generated...")
#         # print(c("RRvect = ", RRvect))
#
#         print("all RR done")
#
#
#         # start chi-square testing ----------------------
#         # testDataAsMatrix = as.matrix.data.frame(RRvect, rownames.force = NA)
#         # refDataAsMatrix = as.matrix.data.frame(comeronData, rownames.force = NA)
#         # chi2_test = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE)
#
#         # comeronData213 = comeron_X[c(1:213),]
#
#         # comeronData =  comeronData[c(1:length(RRvect)),]
#
#         newRR_object = data.frame(MBvect, RRvect)
#         # chi2 = chisq.test(RRvect, p=comeronData$V2, rescale.p = TRUE) # to rescale p to sum up to 1
#         # chi2
#
#         # chi2 = chisq.test(comeronData$V2, p=RRvect, rescale.p = TRUE) # to rescale p to sum up to 1
#         # chi2
#         # myggPlot = ggplot(comeronData) + geom_line(aes(x = V1/10^6, y = V2), color = 'red') + labs(title = paste0("chr ",chrID) , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#         # ggcomparison = myggPlot + geom_line(data = newRR_object, aes(x = MBvect, y = RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#         #                              , label = paste0("Loess_span = ", loess_span, "\n"
#         #                              , "P-value = ", round(chi2$p.value, 4), "\n"
#         #                              , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#         #                              , "df = ", chi2$parameter))
#         # ggcomparison
#         #
#         # ggsave(filename = paste0("chr_",chrID, "_chi2Pvalue_", round(chi2$p.value, 4) , "_comparison_BREC_Loess_", loess_span,"_pc_vs_Comeron", ".png")
#         #        , plot = ggcomparison
#         #        , path = plots_path #plot = last_plot()
#         #        , height = 8
#         #        , width = 12
#         #        )
#
#
#
#         print(chrId_vect)
#
#         if(chrID %in% (newChrList[-1])){ # to avoid the fist :  which doesn't require adjustment
#             print("from inside not X")
#             adjusted_current_MBvect = MBvect + lastMB_current_MBvect
#             lastMB_current_MBvect = MBvect[length(wholeGenome_MBvect)]
#
#             # comeron---------
#             adjusted_comeron = comeronData
#             adjusted_comeron$V1 = adjusted_comeron$V1 + lastMB_adj_comeron
#             lastMB_adj_comeron = comeronData$V1[nrow(wholeGenome_Comeron)]
#             chrId_vect = c(chrId_vect, chrID)
#         }else{ # in case of X
#             print("from inside yesss X")
#             lastMB_current_MBvect = MBvect[nrow(testChromosome)]
#             adjusted_current_MBvect = MBvect
#
#             # comeron----------
#             lastMB_adj_comeron = comeronData$V1[nrow(comeronData)]
#             adjusted_comeron = comeronData
#             chrId_vect = c(chrId_vect, chrID)
#         }
#
#         wholeGenome_Comeron = rbind(wholeGenome_Comeron, adjusted_comeron)
#         wholeGenome_MBvect = c(wholeGenome_MBvect, adjusted_current_MBvect)
#
#         wholeGenome_RRvect = c(wholeGenome_RRvect, RRvect)
#         chrSeparators_c = c(chrSeparators_c, lastMB_adj_comeron)
#         chrSeparators_RR = c(chrSeparators_RR, lastMB_current_MBvect)
#     }
#
#
#     chrSeparators1 = c(lastMB_X, lastMB_adj2L, lastMB_adj2R,lastMB_adj3L)/10^6 ## only for ploting
#     meanRR_wholeGenome_Comeron = mean(wholeGenome_Comeron$V2)
#     print(paste0("meanRR_wholeGenome_Comeron = ", meanRR_wholeGenome_Comeron))
#
#     gg_wholeGenome_Comeron = ggplot(data=wholeGenome_Comeron) + geom_line(aes(x=V1/10^6, y=V2, color="red")) #, breaks = c(0:5:120) )
#
#     wholeGenome_newRR_object = data.frame(wholeGenome_MBvect, wholeGenome_RRvect)
#     gg_wholeGenome_newRR_object = ggplot(data=wholeGenome_newRR_object) + geom_line(aes(x=wholeGenome_MBvect, y=wholeGenome_RRvect, color="red"))
#
#     gridExtra::grid.arrange(gg_wholeGenome_newRR_object, gg_wholeGenome_Comeron, ncol=1)
#
#
#
#     ### whole genome ====================================================================
#     wholeGenome_Comeron =  comeronData[c(1:length(wholeGenome_RRvect)),]
#     wholeGenome_newRR_object = data.frame(wholeGenome_MBvect, wholeGenome_RRvect)
#     chi2 = chisq.test(wholeGenome_Comeron$V2, p=wholeGenome_RRvect, rescale.p = TRUE) # to rescale p to sum up to 1
#     chi2
#     wholeGenome_myggPlot = ggplot(wholeGenome_Comeron) + geom_line(aes(x = V1/10^6, y = V2), color = 'red') + labs(title = paste0("whole_genome") , x = "Physical location (Mb)", y = "Recombination rate(cM/Mb)") #, color = "Comeron map")
#     wholeGenome_ggcomparison =  wholeGenome_myggPlot + geom_line(data = wholeGenome_newRR_object, aes(x = wholeGenome_MBvect, y = wholeGenome_RRvect), color = "darkgreen") + annotate("text", x = 2, y = 5
#                                                                                                                                                                                        , label = paste0("Loess_span = ", loess_span, "\n"
#                                                                                                                                                                                                         , "P-value = ", round(chi2$p.value, 4), "\n"
#                                                                                                                                                                                                         , "Chi-sq = ", round(chi2$statistic, 4), "\n"
#                                                                                                                                                                                                         , "df = ", chi2$parameter))
#     wholeGenome_ggcomparison
#
#     ggsave(filename = paste0("whole_genome", "_chi2Pvalue_", round(chi2$p.value, 4) , "_comparison_BREC_Loess_", loess_span,"_pc_vs_Comeron", ".png")
#            , plot =  wholeGenome_ggcomparison
#            , path = plots_path #plot = last_plot()
#            , height = 8
#            , width = 12
#     )
#     # =====================================================================================
# }
#
# # chi2_1inv = chisq.test(comeronData, RRvect) #, rescale.p = TRUE) # to rescale p to sum up to 1
# # chi2_1inv  # ==> chi-sq(a,b) = chi-sq(b,a)
#
# # chi2_1_inv = chisq.test(RR_object_loess0.75$regDr, p = RR_object_polynomial$regDr, rescale.p = TRUE) # to rescale p to sum up to 1
# # chi2_1_inv
#
# # chi2_2 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
# # chi2_3 = chisq.test(RR_object_polynomial$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
# #
# # chi2_4 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.5$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
# # chi2_5 = chisq.test(RR_object_loess0.25$regDr, p = RR_object_loess0.75$regDr, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#
#
# print(c("chi2_1 = ", chi2_1, "chi2Pvalue_1 = ", chi2_1$p.value ))
# print(c("chi2_2 = ", chi2_2, "chi2Pvalue_2 = ", chi2_2$p.value ))
# print(c("chi2_3 = ", chi2_3, "chi2Pvalue_3 = ", chi2_3$p.value ))
# print(c("chi2_4 = ", chi2_4, "chi2Pvalue_4 = ", chi2_4$p.value ))
# print(c("chi2_5 = ", chi2_5, "chi2Pvalue_5 = ", chi2_5$p.value ))
#
# significativellyDifferent = FALSE
#
#
# # testDataAsMatrix = as.matrix.data.frame(subset(testData, select=c("cm", "mb")), rownames.force = NA)
# # refDataAsMatrix = as.matrix.data.frame(subset(refData, select=c("cm", "mb")), rownames.force = NA)
# # chi2 = chisq.test(testDataAsMatrix, p = refDataAsMatrix, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
#
#
#
# chi2 = chisq.test(testData, p = refData, simulate.p.value = TRUE, rescale.p = TRUE) # to rescale p to sum up to 1
# print(chi2)
# chi2Pvalue = chi2$p.value
# print(chi2Pvalue)
#
# if(chi2Pvalue > 0.05){
#     significativellyDifferent = TRUE
#     print("OK! not significativelly Different .. statistically significant !?")
# }else{
#     print("Oups! significativelly different")
# }
#
# # estimate RR with all scenarios
# # RR_object_polynomial = estimate_recombination_rates_third_degree_polynomial(testChromosome)
# # RR_object_loess0.25 = estimate_recombination_rates_loess(testChromosome, span = 0.25)
# # RR_object_loess0.5 = estimate_recombination_rates_loess(testChromosome, span = 0.5)
# # RR_object_loess0.75 = estimate_recombination_rates_loess(testChromosome, span = 0.75)
#
#
