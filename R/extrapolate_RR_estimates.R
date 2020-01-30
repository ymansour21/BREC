#' extrapolate_RR_estimates
#'
#' extrapolate recombination rate values in heterochromatin regions along the chromosome
#'
#' two cases to handle : centromere and telomeres
#' goal : set recombination rate values to zero in both cases (since theses genomic regions are known for a null recombination)

#' @param chromosome xxx
#' @param RR_object xxx
#' @param heteroChromatinBoundaries xxx
#' @param telomeres_boundaries xxx
#' @param chrID xxx
#' @param chrType ....
#' @param minRR_object ...
#'
#' @return RR_object
#' @export

extrapolate_RR_estimates <- function(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType, minRR_object){

    if(chrType == 1){   #whole chromosome : works on metacentric chromosomes =====================================================
        # centromere_____________
        RR_object$regDr[heteroChromatinBoundaries$indexHBleft : heteroChromatinBoundaries$indexHBright] = 0

        # telomeres______________
        RR_object$regDr[telomeres_boundaries$index_minR2_right : nrow(RR_object)] = 0
        RR_object$regDr[1 : telomeres_boundaries$index_minR2_left] = 0


    }else{  #chromosomal arm : works on telocentric chromosomes ===================================================================
        minRR = minRR_object$minRRpoly # min(RR_vector)
        indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)

        if(indexOfMinRR == nrow(chromosome)){ # this is a left ahromosomal arm (exp : 2L) => left boundary (red)
            # centromere_____________
            RR_object$regDr[heteroChromatinBoundaries$indexHBArm : nrow(RR_object)] = 0

            # telomere______________
            RR_object$regDr[1 : telomeres_boundaries$index_minR2_arm] = 0

        }else if(indexOfMinRR == 1){ # this is a right chromosomal arm (exp : 2R) => right boundary (purple)
            # centromere_____________
            RR_object$regDr[1 : heteroChromatinBoundaries$indexHBArm] = 0

            # telomere______________
            RR_object$regDr[telomeres_boundaries$index_minR2_arm : nrow(RR_object)] = 0

        } ## in case heterochromatine boundaries are not defined, no extrapolation is needed

    }
    return(RR_object)
}
