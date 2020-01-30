#' Brec_chromosome_part_2
#'
#' The main function that initiates the workflow of recombination rates and chromatin boundaries estimation along a chromosme
#'
#' This function applies Brec on a chromosome by taking as parameters the name of the input genome and the chromosome number (or ID)
#' in addition of some other optional parameters like
#'
#' @param BREC_chromosome_results_list ...
#' @param chrType_object ..
#'
#' @importFrom utils head
#' @importFrom stringr str_to_upper
#' @import shiny
#' @import htmlwidgets
#' @return BrecResultsList
#' @export

#!usr/bin/env Rscript

# @param physicalMapUnit the measurement unit used for the physical map in the input data file. Default value is megabase pair "Mb". Options: basepair "bp".


Brec_chromosome_part_2 <- function(BREC_chromosome_results_list, chrType_object){


    genomeName = BREC_chromosome_results_list$genomeName
    plots_path = BREC_chromosome_results_list$plots_path
    chromosome = BREC_chromosome_results_list$chromosome
    RR_object = BREC_chromosome_results_list$RR_object
    R2DataFrame2D = BREC_chromosome_results_list$R2DataFrame2D
    chrID = BREC_chromosome_results_list$chrID
    minRR_object = BREC_chromosome_results_list$minRR_object
    using_slidingWindowApproach_for_HCB = BREC_chromosome_results_list$using_slidingWindowApproach_for_HCB

    chrType = chrType_object$chr_type
    # RR_object2 = RR_object

    if(using_slidingWindowApproach_for_HCB) {
        R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
        print("R2 done")
        # print(R2DataFrame2D)
        print("Extracting CB for this chromosome ...")
        heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
        print("extract centroCB done")
        telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType, minRR_object)
        print("extract telo CB done")
        RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType, minRR_object)
        print("extrapolation done")
    }else{
        if(chrType == 1){  #whole chromosome : works on metacentric chromosomes
            heteroChromatinBoundaries = data.frame( heteroBoundLeft = NULL, indexHBleft = NULL, heteroBoundRight = NULL, indexHBright = NULL, swSize = NULL)
            telomeres_boundaries = data.frame(index_minR2_left = NULL, telo_left = NULL, index_minR2_right = NULL, telo_right = NULL)
        }else{ #chromosomal arm : works on telocentric chromosomes
            heteroChromatinBoundaries = data.frame(heteroBoundArm = NULL, indexHBArm = NULL, swSize = NULL)
            telomeres_boundaries = data.frame(index_minR2_arm = NULL, telo_arm = NULL)
        }
        print("No chromatin boundaraies are identified and no recombination rate extrapolation is done !!! ")
    }

    # RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
    # HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)
    #
    # cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")

    # RR_plot = plot_steps_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize)

    RR_plot = plot_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize, telomeres_boundaries, plots_path, chrType_object)

    # save_plot_as_png(RR_plot, plots_path, genomeName, chrID)

    cat(" \n *-*-*-*-*||  The plot is saved as png file in this directory :", plots_path,"  ||*-*-*-*-* \n ")
    # print(RR_plot)

    # save interactive plot as html
    # saveWidget(RR_plot, paste0(plots_path,"RR_plot.html"))

    # saveRDS(RR_plot,paste0(plots_path,"RR_plot.RDS"))

    # display Stats + save them in external file.txt
    # generate_stats(chromosome, RR_object, HCB_fileContent, do_cleaning)

    BrecResultsList = list(RR_plot, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrType)

    print(chrType_object)
    print(heteroChromatinBoundaries)
    print(telomeres_boundaries)
    return(BrecResultsList) # return RR vector only    ## this return is crucial and needed by other functions, only commented to avoid the printing
}

