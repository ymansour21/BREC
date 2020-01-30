#' extract chromatin boundaries
#'
#' approch : start from min(RR) and go in both ways on R2 plots with a sliding window
#'             for which the size is computed automatically based on physical_map
#'             range (écart) of each 2 consecutive points. So, the value of the maximum
#'             range will represent the sliding window size
#'
#' @param chromosome xx
#' @param RR_object xx
#' @param R2DataFrame2D xxx
#' @param chrID xxx
#' @param chrType xxx
#' @param minRR_object xxx
#'
#' @importFrom grDevices boxplot.stats
#' @return heteroChromatinBoundaries a dataframe of both centromeric chromatinboundaries, values are physical locations
#' @export

extract_CB <- function(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object){
  # Remember : left corresponds to dirction 1 and right to direction 2

    if(chrType == 1){   #whole chromosome : works on metacentric chromosomes ========================================================

        minRR = minRR_object$minRRpoly # min(RR_vector)
        indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)

        print(c("indexOfMinRR", indexOfMinRR))
        # get rn and ln the right and left neighbours of the central point which is the phy-pos of the minimum RR : mb(minRR)
        centralPt = chromosome$mb[indexOfMinRR]
        print(centralPt)
        if(!is.na(chromosome$mb[indexOfMinRR+1])){
            rn = chromosome$mb[indexOfMinRR+1]
        }else{
            rn = centralPt
        }
        print(rn)

        if(indexOfMinRR != 1){
            if(!is.na(chromosome$mb[indexOfMinRR-1])){
                ln = chromosome$mb[indexOfMinRR-1]
            }
        }else{
            ln = centralPt
        }
        print(ln)
        ecartRnLn = abs(rn-ln)
        cat("ecartRnLn", ecartRnLn, sep = " = ")
        # ecartRnLn = max(abs(centralPt-rn), abs(centralPt-ln))
        ecartsPhysVector = c()
        for(k in 2:(nrow(chromosome))){
            ecartsPhysVector = c(ecartsPhysVector, chromosome$mb[k] - chromosome$mb[k-1])
        }
        print(ecartsPhysVector)
        b = boxplot.stats(ecartsPhysVector)
        # print(b)
        # boxplot(ecartsPhysVector)
        seuilMaxMoustache = b$stats[5] # in case of boxplot -> b$stats[5, ] because it's a matrix
        cat("seuilMaxMoustache", seuilMaxMoustache, sep = " = ")

        # print("#############################################################")
        # print(paste("number of markers", nrow(chromosome)))
        # print(paste("markers density", nrow(chromosome)/ chromosome$mb[nrow(chromosome)]))
        # print(paste("ecartRnLn", ecartRnLn  ))
        # print(paste("seuilMaxMoustache " ,seuilMaxMoustache, "seuilMaxMoustache * 2 ", seuilMaxMoustache*2))
        # print(paste("ecartRnLn <= seuilMaxMoustache*2", ecartRnLn <= seuilMaxMoustache*2))
        #
        # print("#############################################################")


        if(ecartRnLn <= seuilMaxMoustache*2){
            print("Meta chromosome: extract_CB_with_no_centromeric_gap...")
            heteroChromatinBoundaries = extract_CB_with_no_centromeric_gap(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
        }else{
            print("Meta chromosome:extract_CB_with_centromeric_gap...")
            heteroChromatinBoundaries = extract_CB_with_centromeric_gap(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
        }
    }else if(chrType == 0){  #chromosomal arm : works on telocentric chromosomes =====================================================================
        print("Telo chromosome: extract_CB_with_no_centromeric_gap by default...")
        heteroChromatinBoundaries = extract_CB_with_no_centromeric_gap(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
    }


  return(heteroChromatinBoundaries)
}
