#' #' define_chromosome_type_based_on_gap_existance
#' #'
#' #' this function applyes a test based on min(RR) to determine if a chromosome is telocentric (a chromosomal arm) or metacentric (a whole chromosome)
#' #'
#' #' @param chromosome xx
#' #' @param minRR_object xxx
#' #'
#' #' @return chr_type xx
#' #' @export
#' #'
#'
#' define_chromosome_type_based_on_gap_existance <- function(chromosome, minRR_object){  ## updated version of get_chr_type() : december 2019 with gap idea
#'
#'     # ======================================================
#'     ## the used will provide this infomation on the gui --------------------------
#'     # chr_type = 1 # arm
#'     # chr_type = 0 # whole chr
#'     chr_type = 2 # don't know
#'     # ======================================================
#' #
#' #     # from fct get_chromosome_type()-----------------------------------------------
#' #      minRR = minRR_object$minRRpoly # min(RR_vector)
#' #         indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)
#' #
#' #         if((indexOfMinRR == 1) || (indexOfMinRR == nrow(chromosome))){
#' #             chr_type = 0 # "chromosomal arm"
#' #             print("This is a telocentric chromosome (chromosomal arm) ! ")
#' #         }else{
#' #             chr_type = 1 # "whole chromosome"
#' #             print("This is a metacentric chromosome (whole chromosome) !")
#' #         }
#'
#'     ## in case of don't know, apply gap-based idea here below ---------------------
#'     if(chr_type == 2){
#'
#'         # use already existing process detecting there is a gap or not
#'
#'         if(gap = T){
#'             chr_type = 1 # "whole chromosome"
#'             print("This is a metacentric chromosome (whole chromosome) !")
#'         }else{
#'             # use ditribution of all gaps to define the biggest gap on physicl map
#'             ecartsPhysVector = c()
#'             for(k in 2:(nrow(chromosome))){
#'                 ecartsPhysVector = c(ecartsPhysVector, chromosome$mb[k] - chromosome$mb[k-1])
#'             }
#'             # print(ecartsPhysVector)
#'             # summary(ecartsPhysVector)
#'             # sd(ecartsPhysVector)
#'             if(max(ecartsPhysVector) > mean(ecartsPhysVector) + sd(ecartsPhysVector)){
#'                 chr_type = 1 # "whole chromosome"
#'                 print("This is a metacentric chromosome (whole chromosome) !")
#'             }else{
#'                 chr_type = 0 # "chromosomal arm"
#'                 print("This is a telocentric chromosome (chromosomal arm) ! ")
#'             }
#'         }
#'     }
#'
#'     return(chr_type)
#' }
