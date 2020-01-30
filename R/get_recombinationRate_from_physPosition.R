#' #' #' Get recombination rate value from physical position
#' #' #'
#' #' #' get recombination rate value corresponding to the given physical position value on the chronosome (in mb)
#' #' #'
#' #' #' @param chromosome xx
#' #' #' @param physicalPosVect xxx
#' #' #'
#' #' #' @return RRvalue is the numeric value of recombination rate
#' #' #' @export
#' #
#' #
#' # get_recombinationRate_from_physPosition <- function(chromosome, physicalPosVect){  ## NOT TESTED YET !!
#' #
#' #   ## physicalPosVect is the vector of regions expressed in mb
#' #
#' #   # chrSize = nrow(chromosome)
#' #   # MB = chromosome$mb #physicalMap
#' #   # cM = chromosome$cm #geneticMap
#' #
#' #
#' #   #----#----#----#----#----#----#----#----#----#----#----#----
#' #   #---- Fitting Linear Model ----------------------method 1 raw rr before  extrapol
#' #
#' #   # model = glm(cM ~ poly(MB, 3, raw = T))
#' #   # a = model$coefficients[4] # not taking the whole number exp : -1.870727 instead of -1.8707274
#' #   # b = model$coefficients[3]
#' #   # c = model$coefficients[2]
#' #   # d = model$coefficients[1]
#' #   # regDr = (a *3*physicalPosVect^2) + (b *2*physicalPosVect) + c  # regDr formula used here manually
#' #   # regDr[regDr < 0 ] = 0
#' #
#' #     #----#----#----#----#----#----#----#----#----#----#----#----
#' #   # adapt with extrapolation like in Brec_chromosme + HCB
#' #
#' #
#' #   #66666666666666666666666666666666666666666666666666666666666666666666666666666
#' # library(Brec)
#' # library(na.tools)
#' #   # read dmel R5 data
#'   inputData = read.csv(file = "data/BREC_byArm_MB_marey_map_R5.36_removeOutliers.csv", header = T, sep = "\t")
#'   chrList = get_list_of_chromosomes(inputData)
#'
#'   chrID = "X"
#'   chrID = "3R"
#'   # for (chrID in chrList) {
#'   print(c("=========== chr in process ====> ", chrID, "==================="))
#'   chromosome = get_chromosome_from_inputData(inputData, chrID)
#'   # print(c("refChromosomeSize",nrow(testChromosome)))
#'   chrSize = nrow(chromosome)
#'   MB = chromosome$mb #physicalMap
#'   cM = chromosome$cm #geneticMap
#'
#'
#'   # estimate RR with all scenarios
#'   RR_object_loess0.25 = estimate_recombination_rates_loess(chromosome, 0.25)
#'
#'   RR_object = RR_object_loess0.25
#'
#'   minRR_object = get_min_RR_value_based_on_polynomial(chromosome)
#'   print(" get minRR done !")
#'
#'   chrType = get_chromosome_type(chromosome, minRR_object)
#'   # RR_object2 = RR_object
#'
#'   R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
#'   print("R2 done")
#'   print(R2DataFrame2D)
#'
#'
#'   # if(using_slidingWindowApproach_for_HCB) {
#'   print("Extracting CB for this chromosome ...")
#'   heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
#'   print("extract centroCB done")
#'   telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType, minRR_object)
#'   print("extract telo CB done")
#'   RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType, minRR_object)
#'   print("extrapolation done")
#'
#'   #66666666666666666666666666666666666666666666666666666666666666666666666666666
#'
#'   # get loess model as RR "continuous" estimates ---------------
#'   model = loess(cM ~ MB, span = 0.25, degree = 2)#, na.action = na.exclude
#'
#'   physicalPosVect = c(0,5)
#'
#'   MBvect = physicalPosVect
#'   if(length(physicalPosVect)== 1){
#'       if(physicalPosVect < min(MB) || physicalPosVect > max(MB)){
#'           RRVect = NULL
#'           print("RRVect = 'na' because physicalPosVect < min(MB) || physicalPosVect > max(MB)")
#'       }else if(physicalPosVect == min(MB)){
#'           RRVect = min(MB)
#'           print("RRVect = min(MB) because physicalPosVect == min(MB)")
#'       }else if(physicalPosVect == max(MB)){
#'           RRVect = max(MB)
#'           print("RRVect = max(MB) because physicalPosVect == max(MB)")
#'       }else{
#'
#'           # predict RR values for physicalPosVect == 1 pos ------------------------
#'
#'           regFn = predict(model, MBvect)  ## needed only for plotting
#'           MBvectplus1 = MBvect+1
#'           MBvectminus1 = MBvect-1
#'           RR_predectedplus1 = predict(model, MBvectplus1)
#'           RR_predectedplus1 = na.replace(RR_predectedplus1, max(RR_predectedplus1, na.rm = TRUE))
#'           RR_predectedminus1 = predict(model, MBvectminus1)
#'           RR_predectedminus1 = na.replace(RR_predectedminus1, min(RR_predectedminus1, na.rm = TRUE))
#'           regDr <- mapply(function(x1, x2, y1, y2) {round((y2-y1)/(x2-x1 ), 2)}, MBvectminus1, MBvectplus1, RR_predectedminus1, RR_predectedplus1)
#'           print("Loess model is generated...")
#'           print(c("RRVect = ", RRVect))
#'       }
#'   }else{
#'       # predict RR values for physicalPosVect > 1 pos ------------------------
#'       regFn = predict(model, MBvect)  ## needed only for plotting
#'       MBvectplus1 = MBvect+1
#'       MBvectminus1 = MBvect-1
#'       RR_predectedplus1 = predict(model, MBvectplus1)
#'       RR_predectedplus1 = na.replace(RR_predectedplus1, max(RR_predectedplus1, na.rm = TRUE))
#'       RR_predectedminus1 = predict(model, MBvectminus1)
#'       RR_predectedminus1 = na.replace(RR_predectedminus1, min(RR_predectedminus1, na.rm = TRUE))
#'       regDr <- mapply(function(x1, x2, y1, y2) {round((y2-y1)/(x2-x1 ), 2)},  MBvectminus1, MBvectplus1, RR_predectedminus1, RR_predectedplus1)
#'       RRVect = regDr
#'       print("Loess model is generated...")
#'       print(c("RRVect = ", RRVect))
#'   }
#'  if(!is.null(RRVect)){
#'     print(c("mean(RRVect) = ", mean(RRVect)))
#'  }else{
#'      print("RRVect is NULL !!")
#'  }
#'
#'   res =list(data.frame(physicalPosVect, RRVect), mean(RRVect))
#' # #   return(res)
#' # # }
