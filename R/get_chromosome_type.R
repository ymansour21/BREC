#' get_chromosome_type
#'
#' this function applyes a test based on min(RR) to determine if a chromosome is telocentric (a chromosomal arm) or metacentric (a whole chromosome)
#'
#' @param chromosome xx
#' @param chrID xx
#' @param minRR_object xx
#' @param RR_object xxx
#'
#' @return chr_type xx
#' @export
#'

get_chromosome_type <- function(chromosome, chrID, minRR_object){

    # ======================================================
    ## the used will provide this infomation on the gui --------------------------
    # chr_type = 0 # arm
    # chr_type = 1 # whole chr
    # chr_type = 2 # don't know
    # ======================================================

    # ## in case of don't know, apply gap-based idea here below ---------------------
    # if(chr_type == 2){ # don't know
    #
    #     # first test to define chr type ==> pos_of_min(RR) -------------------------
    #     minRR = minRR_object$minRRpoly # min(RR_vector)
    #     indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)
    #
    #     if((indexOfMinRR == 1) || (indexOfMinRR == nrow(chromosome))){
    #         chr_type = 0 # "chromosomal arm"
    #         print("1st test: minRR is in the begining or the end ! This is a telocentric chromosome (chromosomal arm) ! ")
    #     }else{
    #         chr_type = 1 # "whole chromosome"
    #         print("1st test: minRR is in the middle ! This is a metacentric chromosome (whole chromosome) !")
    #     }
    #     #---------------------------------------------------------------------------
    #     # if(chr_type == 1){
    #     #    # ....
    #     # }else{
    #         # use ditribution of all gaps to define the biggest gap on physicl map
    #         ecartsPhysVector = c()
    #         for(k in 2:(nrow(chromosome))){
    #             ecartsPhysVector = c(ecartsPhysVector, chromosome$mb[k] - chromosome$mb[k-1])
    #         }
    #         # print(ecartsPhysVector)
    #         # summary(ecartsPhysVector)
    #         # sd(ecartsPhysVector)
    #         if(max(ecartsPhysVector) > mean(ecartsPhysVector) + 2*sd(ecartsPhysVector)){
    #             chr_type = 1 # "whole chromosome"
    #             print("2nd test: the gap found is a centromeric gap ==> This is a metacentric chromosome (whole chromosome) with centro gap !")
    #         }else{
    #             chr_type = 0 # "chromosomal arm"
    #             print("2nd test: the gap found is not centromeric ==> This is a telocentric chromosome (chromosomal arm) ! ")
    #         }
    #     # }
    # }

    # print("#############################################################")
    # print(paste("chr size", chromosome$mb[nrow(chromosome)]))
    # print(paste("gap size", max(ecartsPhysVector)))
    # print(paste("gap size = % chr size ", max(ecartsPhysVector) / chromosome$mb[nrow(chromosome)]))
    # print(paste("comparison max > mean+2sd " ,max(ecartsPhysVector) > mean(ecartsPhysVector) + 2*sd(ecartsPhysVector)))
    #
    # print("#############################################################")

    ###---- proposed soluion -- adapted for BREC -----------------------------------------------------------------------------

    # minRRloess --------------------------------------------------------------------
    # minRRloess = min(RR_loess25)
    # indexOfMinRRloess = match(minRRloess, RR_loess25)

    RR_object = estimate_recombination_rates_loess(chromosome, span = 0.15)
    minRRloess = min(RR_object$regDr)
    indexOfMinRRloess = match(minRRloess, RR_object$regDr)
    physPos_minRRloess = chromosome$mb[indexOfMinRRloess]
    print(data.frame(chrID, physPos_minRRloess, minRRloess, indexOfMinRRloess))

    # minRRpoly --------------------------------------------------------------------
    # minRRpoly = min(RR_poly)
    # indexOfMinRRpoly = match(minRRpoly, RR_poly)
    minRRpoly = minRR_object$minRRpoly
    indexOfMinRRpoly = minRR_object$indexOfMinRRpoly
    physPos_minRRpoly = chromosome$mb[indexOfMinRRpoly]
    print(data.frame(chrID, physPos_minRRpoly,  minRRpoly, indexOfMinRRpoly))

    chrSize = chromosome$mb[nrow(chromosome)]
    pcg_df = data.frame(chrID, pcg40 =0.4*chrSize ,pcg60 =0.6*chrSize)
    # print(pcg_df)
    # print(data.frame(chrID, chrSize, physPos_minRRloess, physPos_minRRpoly))

    # chr_type = 0 # arm
    # chr_type = 1 # whole chr

    chr_type = 2 # don't know
    chr_sub_type = "Don't know!"
    if(physPos_minRRloess == chromosome$mb[1] | physPos_minRRloess == chrSize){
        if(physPos_minRRpoly == physPos_minRRloess){
            chr_type = 0  # arm
            chr_sub_type = "Telocentric"
        }
        else{
            # chr_type = 2
            # chr_sub_type = "Don't know"
            print("Warning !! User decision might be required...")
        }
    }else{ # minRRloess is inside chr => whole (confirmed)
        if(physPos_minRRloess >= pcg_df$pcg40 & physPos_minRRloess <= pcg_df$pcg60){ # case 1 : metacentric
            chr_type = 1 # whole chr
            chr_sub_type = "Atelocentric - metacentric"
        # }else if((indexOfMinRRloess >= pcg_df$pcg90 | indexOfMinRRloess <= pcg_df$pcg10)){ # case 3 : acrocentric
        #     chr_sub_type = "acrocentric"
        }else{  # if(physPos_minRRloess >= pcg_df$pcg10 & physPos_minRRloess < pcg_df$pcg45) | (physPos_minRRloess >= pcg_df$pcg55 & physPos_minRRloess < pcg_df$pcg90)  # case  : submetacentric
            # chr_sub_type = "Atelocentric - not metacentric" #submetacentric
            chr_type = 2 # don't know
            chr_sub_type = "Don't know!"
        }

    }
    chrType_object = data.frame(chr_type, chr_sub_type)
    print(chrType_object)
    return(chrType_object)
}
