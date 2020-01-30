#' generate_HCB_output_data_file
#'
#' this function generates and saves the output as a .txt data file for heterochromatin boundaries with centromere and telomeres estimated sizes
#'
#'
#' @param genomeName xxx
#' @param chrID xxx
#' @param chromosome xx
#' @param heteroChromatinBoundaries xx
#' @param telomeres_boundaries xx
#' @param output_path xx
#' @param chrType ....
#'
#' @return RR_fileContent
#' @export


generate_HCB_output_data_file <- function(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType){

    # output2: heterochromatin_boundaries

    if(chrType == 1){  #whole chromosome : works on metacentric chromosomes ===================================================================
        centromereSize = heteroChromatinBoundaries$heteroBoundRight - heteroChromatinBoundaries$heteroBoundLeft
        leftTelomereSize = telomeres_boundaries$telo_left
        rightTelomereSize = chromosome$mb[nrow(chromosome)] - telomeres_boundaries$telo_right

        feature = c( "chromosome",
                     "left_telomere_boundary",
                     "left_centromere_boundary",
                     "right_centromere_boundary",
                     "right_telomere_boundary",
                     "centromere_size",
                     "left_telomere_size",
                     "right_telomere_size")
        value = c( chrID,
                   telomeres_boundaries$extrapolPhysPos_left,
                   heteroChromatinBoundaries$heteroBoundLeft,
                   heteroChromatinBoundaries$heteroBoundRight,
                   telomeres_boundaries$telo_right,
                   centromereSize,
                   leftTelomereSize,
                   rightTelomereSize)

        print(feature)
        print(value)

        HCB_fileContent = data.frame(feature = feature, value = value)

        # HCB_fileContent = data.frame(chromosome = chrID,
        #                              left_telomere_boundary = telomeres_boundaries$telo_left,
        #                              left_centromere_boundary = heteroChromatinBoundaries$heteroBoundLeft,
        #                              right_centromere_boundary = heteroChromatinBoundaries$heteroBoundRight,
        #                              right_telomere_boundary = telomeres_boundaries$telo_right,
        #                              centromere_size = centromereSize,
        #                              left_telomere_size = leftTelomereSize,
        #                              right_telomere_size = rightTelomereSize)
    }else{ #chromosomal arm : works on telocentric chromosomes =================================================================================
        centromereSize = chromosome$mb[nrow(chromosome)] - heteroChromatinBoundaries$heteroBoundArm
        ArmTelomereSize = telomeres_boundaries$telo_arm

        feature = c( "chromosome",
                     "left_telomere_boundary",
                     "right_centromere_boundary",
                     "centromere_size",
                     "left_telomere_size")
        value = c( chrID,
                   telomeres_boundaries$telo_arm,
                   heteroChromatinBoundaries$heteroBoundArm,
                   centromereSize,
                   ArmTelomereSize)

        print(length(feature))
        print(length(value))

        HCB_fileContent = data.frame(feature = feature, value = value)

        # HCB_fileContent = data.frame(chromosome = chrID,
        #                              arm_telomere_boundary =  telomeres_boundaries$telo_arm,
        #                              arm_centromere_boundary = heteroChromatinBoundaries$heteroBoundArm,
        #                              centromere_size = centromereSize,
        #                              arm_telomere_size = ArmTelomereSize)
    }
    # utils::write.table(HCB_fileContent, file = stringr::str_replace_all( paste(output_path, genomeName,"- Chr", chrID, "_chromatinBoundaries_Brec.txt"), stringr::fixed(" "), ""), sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
    return(HCB_fileContent)
 }
