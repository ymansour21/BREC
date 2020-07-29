#' extract_telomeres_boundaries
#'
#' identify telomeres boundaries to use later for extrapolating the RR plot, using min R2 in both directions
#'
#' goal
#'
#' @param chromosome xxx
#' @param R2DataFrame2D  xxx
#' @param chrID xxx
#' @param chrType ....
#' @param minRR_object xxx
#'
#' @return telomeres_boundaries
#' @export

extract_telomeres_boundaries <- function(chromosome, R2DataFrame2D, chrID, chrType, minRR_object){

    if(chrType == 1){   #whole chromosome : works on metacentric chromosomes =================================================

        minR2_right = min(R2DataFrame2D$R2Vect_dir2)
        minR2_left = min(R2DataFrame2D$R2Vect_dir1)
        index_minR2_right = match(minR2_right, R2DataFrame2D$R2Vect_dir2)
        index_minR2_left = match(minR2_left, R2DataFrame2D$R2Vect_dir1)

        #___________# needed only for display!!
        telo_right = chromosome$mb[index_minR2_right]
        telo_left = chromosome$mb[index_minR2_left]
        # print(c("telo_left=", telo_left, "telo_right=", telo_right, "index_minR2_left=", index_minR2_left, "index_minR2_right=", index_minR2_right))
        telomeres_boundaries = data.frame(index_minR2_left, telo_left, index_minR2_right, telo_right)

    }else{  #chromosomal arm : works on telocentric chromosomes ===============================================================

        middleChr_pos = nrow(chromosome)/2
        minRR = minRR_object$minRRpoly # min(RR_vector)
        indexOfMinRR = minRR_object$indexOfMinRRpoly # match(minRR, RR_vector)

        if(indexOfMinRR == nrow(chromosome)){ # this is a left ahromosomal arm (exp : 2L) => left boundary (red)
            minR2_arm = min(R2DataFrame2D$R2Vect_dir1)
            index_minR2_arm = match(minR2_arm, R2DataFrame2D$R2Vect_dir1)
            if(index_minR2_arm > middleChr_pos){  #this is to solve the issue: minR2_arm happens to be on the centro side and not the telo => solved by finding the min in the right half of the arm
                minR2_arm = min(R2DataFrame2D$R2Vect_dir1[1:middleChr_pos])
                index_minR2_arm =  match(minR2_arm, R2DataFrame2D$R2Vect_dir1[1:middleChr_pos])
            }
        }else if(indexOfMinRR == 1){ # this is a right chromosomal arm (exp : 2R) => right boundary (purple)
            minR2_arm = min(R2DataFrame2D$R2Vect_dir2)
            index_minR2_arm = match(minR2_arm, R2DataFrame2D$R2Vect_dir2)
            if(index_minR2_arm < middleChr_pos){ #this is to solve the issue: minR2_arm happens to be on the centro side and not the telo => solved by finding the min in the right half of the arm
                minR2_arm = min(R2DataFrame2D$R2Vect_dir2[middleChr_pos:1])
                index_minR2_arm =  match(minR2_arm, R2DataFrame2D$R2Vect_dir2[middleChr_pos:1])
            }
        }

        #___________# needed only for display!!
        telo_arm = chromosome$mb[index_minR2_arm]
        # print(c("telo_arm=", telo_arm, "index_minR2_arm=", index_minR2_arm, "minR2_arm=", minR2_arm ))
        telomeres_boundaries = data.frame(index_minR2_arm, telo_arm)
    }

    return(telomeres_boundaries)
}
