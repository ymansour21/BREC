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
#' @param chrType ....
#'
#' @importFrom grDevices boxplot.stats
#' @return heteroChromatinBoundaries a dataframe of both centromeric chromatinboundaries, values are physical locations
#' @export

extract_CB_using_RR <- function(chromosome, RR_object, R2DataFrame2D, chrID, chrType){
    # Remember : left corresponds to dirction 1 and right to direction 2


    minRR = min(RR_object$regDr)
    indexOfMinRR = match(minRR, RR_object$regDr)
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

    if(!is.na(chromosome$mb[indexOfMinRR-1])){
        ln = chromosome$mb[indexOfMinRR-1]
    }else{
        ln = centralPt
    }
    print(ln)
    ecartRnLn = abs(rn-ln)
    # ecartRnLn = max(abs(centralPt-rn), abs(centralPt-ln))
    ecartsPhysVector = c()
    for(k in 2:(nrow(chromosome))){
        ecartsPhysVector = c(ecartsPhysVector, chromosome$mb[k] - chromosome$mb[k-1])
    }
    print(ecartsPhysVector)
    b = boxplot.stats(ecartsPhysVector)
    seuilMaxMoustache = b$stats[5] # in case of boxplot -> b$stats[5, ] because it's a matrix
    if(ecartRnLn <= seuilMaxMoustache){
        print("extract_CB_with_no_centromeric_gap ...")
        heteroChromatinBoundaries = extract_CB_with_no_centromeric_gap_using_RR(chromosome, RR_object, R2DataFrame2D, chrID, chrType)
    }else{
        print("extract_CB_with_centromeric_gap ...")
        heteroChromatinBoundaries = extract_CB_with_centromeric_gap_using_RR(chromosome, RR_object, R2DataFrame2D, chrID, chrType)
    }

    return(heteroChromatinBoundaries)
}
