#' #' validation_using_shift_measure
#'
#'     ##  ref cyto boundaries for all Dmel R6 chromosomes
#'
#'     # if(chrID == "2"){# --------- for chr 2 dmel6
#'     #     # refHCB_centro = cyto_R6
#'     #     cl = 17.400000 # old : 21.17693
#'     #     cr = 32.826207 # old : 26.22948
#'     #     # refHCB_telo = Brec_chr100%
#'     #     tl = 0.5
#'     #     tr = 48.426189
#'     # }else if(chrID == "3"){# --------- for chr 3 dmel6
#'     #     # refHCB_centro = cyto_R6
#'     #     cl = 19.906900
#'     #     cr = 41.984505
#'     #     # refHCB_telo = Brec_chr100%
#'     #     tl = 0.7
#'     #     tr = 59.184505
#'     # }else if(chrID == "X"){ # --------- for chr X dmel6
#'     #     # refHCB_centro = cyto_R6
#'     #     cr = 20.928973
#'     #     # refHCB_telo = Brec_chr100%
#'     #     tl = 2.405967
#'     # }
#'
#'     # Build dataframe of ref cyto boundaries for all Dmel R6 chromosomes
#'
#'
#'     refCB = data.frame(matrix(ncol=5, nrow = 0))
#'     colnames(refCB) <- c("chrID", "cl", "cr", "tl", "tr")
#'     refCB = dplyr::add_row(refCB, chrID = "2", cl = 17.400000 ,cr = 32.826207 , tl = 0.5, tr = 48.426189 )
#'     refCB = dplyr::add_row(refCB, chrID = "3",  cl = 19.906900, cr = 41.984505, tl = 0.7, tr = 59.184505)
#'     refCB = dplyr::add_row(refCB, chrID = "X",  cr = 20.928973, tl = 2.405967)
#'
#'     # chrID      cl       cr       tl       tr
#'     # 1     2 17.4000 32.82621 0.500000 48.42619
#'     # 2     3 19.9069 41.98450 0.700000 59.18451
#'     # 3     X      NA 20.92897 2.405967       NA
#'
#'     # ********************************************************************************************************************************
#'     # *** Read data and build whole chromosomes from arms ****************************************************************************
#'     # ********************************************************************************************************************************
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
#'
#'     # ========= Init ===============================================
#'     testCB = data.frame(matrix(ncol=5, nrow = 0))
#'     colnames(testCB) = colnames(refCB)
#'
#'
#'     for (chrID in chrList) {
#'
#'
#'         chrID = "X"
#'         print(c("=========== chr in process ====> ", chrID, "==================="))
#'         refChromosome = get_chromosome_from_inputData(inputData, chrID)
#'         print(c("refChromosomeSize",nrow(refChromosome)))
#'         # cleaning step
#'         refChromosome = clean_chromosome_data(refChromosome, genomeName = "DmelR6", chrID) #=> chr2 : 240 instead of 267
#'         refChromosomeSize = nrow(refChromosome)
#'         print(c("refChromosome after cleaning --> size = ", refChromosomeSize))
#'
#'         # ************ RUN BREC TO GET testCB dataframe*******************************************************************
#'         # ********************************************************************************************************************************
#'         testChromosome = refChromosome
#'
#'         # run BREC to get new HCB
#'         RR_object = estimate_recombination_rates(testChromosome)
#'         print("RR done")
#'         chrType = get_chromosome_type(testChromosome, RR_object$regDr)
#'         R2DataFrame2D = compute_cumulated_R_squared_2directions(testChromosome)
#'         print("R2 done")
#'
#'         print("Extracting CB for this chromosome ...")
#'         heteroChromatinBoundaries = extract_CB(testChromosome, RR_object, R2DataFrame2D, chrID, chrType)
#'         print("extract centroCB done")
#'         telomeres_boundaries = extract_telomeres_boundaries(testChromosome, R2DataFrame2D, chrID, chrType)
#'         print("extract telo CB done")
#'         RR_object = extrapolate_RR_estimates(testChromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType)
#'         print("extrapolation done")
#'
#'         print(telomeres_boundaries)
#'         print(heteroChromatinBoundaries)
#'
#'         if(chrType == 1){  # not X
#'             hcb_cl =  heteroChromatinBoundaries$heteroBoundLeft
#'             hcb_cr =  heteroChromatinBoundaries$heteroBoundRight
#'             hcb_tl =  telomeres_boundaries$extrapolPhysPos_left
#'             hcb_tr =  telomeres_boundaries$extrapolPhysPos_right
#'             testCB = dplyr::add_row(testCB, chrID = chrID, cl = hcb_cl ,cr = hcb_cr , tl = hcb_tl, tr = hcb_tr )
#'         }else{
#'             hcb_cl =  heteroChromatinBoundaries$heteroBoundLeft
#'             hcb_cr =  heteroChromatinBoundaries$heteroBoundRight
#'             testCB = dplyr::add_row(testCB, chrID = chrID,  cr = hcb_cr , tl = hcb_tl )
#'         }
#'
#'     }
#'     print(testCB)
#'
#'     # ********************************************************************************************************************************
#'     # ******** Validation protocol ******************************************************************************************************************
#'     # ********************************************************************************************************************************
#'
#'     #  compute shiftDF = abs(refCB - testCB)
#'     keptRowNames <- names(refCB)[-1]
#'     shiftDF = na.pass(cbind(refCB[1], abs( refCB[keptRowNames] - testCB[match(refCB$chrID, testCB$chrID), keptRowNames])) )  #shiftDF = abs(refCB - testCB)
#'     print(shiftDF)
#'
#'     # plot
#'     # transpose first
#'     t_shiftDF = transpose(shiftDF[,-1])
#'     # get row and colnames in order
#'     colnames(t_shiftDF) <- shiftDF[,1]
#'     t_shiftDF = cbind(chrID = colnames(shiftDF[-1]), t_shiftDF ) #errrr
#'     # rownames(t_shiftDF) <- colnames(shiftDF[-1])
#'
#'
#'     d <- reshape2::melt(t_shiftDF, id="chrID")
#'     colnames(d) <- c("variable", "chrID", "shift")
#'
#'
#'     gg_validation =  ggplot(d, aes(x=shift, y = variable, color=chrID, group=chrID)) +
#'         geom_line(aes(linetype=chrID), size=1) +
#'         geom_point(aes(shape=chrID), size=4 )
#'         # scale_linetype_manual(shift = c(2,3,1)) +
#'         # scale_shape_manual(shift= c(15,15,15))
#'
#'     gg_validation = gg_validation + ggtitle("Variation of shift values for each chromatin boundary on Dmel R6 chromosomes - shiftCB = abs(cytoCB - brecCB)") +
#'         xlab("Shift (Mb)") + ylab("Chromatin boundaries")
#'
#'     # Modifier les titres des légendes
#'     # gg_validation = gg_validation + theme(legend.text = element_text("Chromosome ID"))
#'
#'     # gg_validation =  ggplot(d, aes(x=value, y = chrID, color=variable, group=variable)) +
#'     #     geom_point(aes(shape=variable, size=4)) +
#'     #     stat_smooth(aes(linetype=variable), size=1 ) +
#'     #     scale_linetype_manual(values = c(2,1,1,2)) +
#'     #     scale_shape_manual(values= c(15,17,17,15))
#'
#'
#'     ggsave(filename = paste0("validation/validationResuts4.png"), plot = gg_validation, path = "/home/yasmine/ProBook/_ISEM/Thesis_work/Recombination_BREC_project_thesis_part_1/DailyUpdates_R_dev/R_projects/My_Brec_project_final/Brec/DQC") #plots_path
#'
#'     gg_validation
#'
#'
#'     # -------------------------------------------------------------------------------------
#' #
#' #     good_CB_resolution = FALSE
#' #     treshols = 1 # Mb
#' #     shift = abs(obsCB, exptCB)
#' #
#' #     cat("shift = " , shift)
#' #
#' #     if (shift == 0){
#' #         good_CB_resolution = TRUE
#' #         print(c("OK! good CB resolution (perfect match) ==> shift = ", shift, "treshold = ,", treshold))
#' #     }else{
#' #         if (shift > 0 & shift <= treshold){
#' #             good_CB_resolution = TRUE
#' #             print(c("OK! good CB resolution ==> shift = ", shift, "treshold = ,", treshold))
#' #         }else{
#' #             print(c("Oups! low CB resolution ==> shift = ", shift, "treshold = ,", treshold))
#' #         }
#' #     }
#' #
#' #     return(good_CB_resolution) # boolean
