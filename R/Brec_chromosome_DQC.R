#' Brec for a chromosome with updated DQC
#'
#' The main function that initiates the workflow of recombination rates and chromatin boundaries estimation along a chromosme
#'
#' This function applies Brec on a chromosome by taking as parameters the name of the input genome and the chromosome number (or ID)
#' in addition of some other optional parameters like
#'
#' @param genomeName string of the name of the genome (organism) forwhich the data is provided. This will be used for output and plot file names. Default value is an empty string "".
#' @param inputData ......
#' @param inputChrID numeric or character value which identifies the chromosome to be processed no default value is available.

#'
#' @importFrom utils head
#' @import shiny
#' @import htmlwidgets
#' @return RR_object
#' @export


# #' @import shinyalert
#!usr/bin/env Rscript

# ============== new version of 15/05/2019 including new DQC process, used by Brec_dashboard_DQC.R ============
Brec_chromosome_DQC <- function(genomeName="Aedes aegypti", inputData, inputChrID ){
# @param separator string for the character used for seperating values in the input data file. Default value is the tab separator "\\t". Options : ","  ";"  " ".
# @param physicalMapUnit the measurement unit used for the physical map in the input data file. Default value is megabase pair "Mb". Options: basepair "bp".

    # options(encoding = "UTF-8")
    # # install_needed_pkgs()
    # wd = getwd()
    # plots_path = paste0(wd,"/Brec/plots/")
    output_path = paste0(wd,"/Brec/results/")    #!!!!!!!!!!!!!! get this from dashoboard later !!!!!!!!!!!!!!!!!!!!
    # using_slidingWindowApproach_for_HCB = TRUE
    #
    # # genomeName = get_genome_full_name(inputGenomeName)
    # # inputData = load_input_data(genomeName,  separator, physicalMapUnit) # return listOfChromosomes
    #
    # # chrList = get_list_of_chromosomes(inputData)
    # chrID = stringr::str_to_upper(inputChrID)
    # chromosome = get_chromosome_from_inputData(inputData, chrID)
    # cat("\n**/**/**/**/**", "chrId : " , chrID ," --- length of this chr : ", nrow(chromosome),"**/**/**/**/**")

    # # **** handling data quality issues ****************
    # goodDataQuality = data_quality_test(chromosome)
    # if (goodDataQuality) { # good data quality 1st iteration
    #   cat("\n Your data quality is : Good ! ")
    #   do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
    #   if(do_5_perCent_cleaning){
    #     cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
    #     chromosome = cleanedChromosome_5_perCent
    #   }
    # }else{ # low data quality 1st iteration
    #   print("Your data quality is : Not good enough !!!")
    #   do_cleaning = ask_user_about_data_cleaning()
    #   if(do_cleaning){
    # cleanedChromosome = clean_chromosome_data(chromosome, genomeName, chrID)
    #     chromosome = cleanedChromosome
    #   }
    #   goodDataQuality = data_quality_test(chromosome)
    #   if (goodDataQuality) {  # good data quality 2nd iteration
    #     print("Your data quality became : Good !")
    #   }else{  # low data quality 2nd iteration
    #     print("Your data quality is still : Not good enough !!!")
    #     do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
    #     if(do_5_perCent_cleaning){
    #       cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
    #       chromosome = cleanedChromosome_5_perCent
    #       goodDataQuality = data_quality_test(chromosome)
    #     }
    #   }
    #   if(!goodDataQuality){
    #       using_slidingWindowApproach_for_HCB = FALSE
    #   }
    # }
    # # ***************************************************

    # cat("\n new chr size : " , nrow(chromosome), "\n")
    #**** end of handling data quality issues *************

    RR_object = estimate_recombination_rates(chromosome)
    print("RR done")
    chrType = get_chromosome_type(chromosome, RR_object$regDr)
    # RR_object2 = RR_object

    R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
    print("R2 done")

    # if(using_slidingWindowApproach_for_HCB) {
    print("Extracting CB for this chromosome ...")
    heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType)
    print("extract centroCB done")
    telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType)
    print("extract telo CB done")
    RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType)
    print("extrapolation done")

    # }else{
    #     heteroChromatinBoundaries = data.frame( heteroBoundLeft = 0, heteroBoundRight = 0, swSize = 0)
    #     telomeres_boundaries = data.frame(index_minR2_left = 0, extrapolPhysPos_left = 0, index_minR2_right = 0, extrapolPhysPos_right = 0)
    #     print("No chromatin boundaraies estimation and no extrapolation !!! ")
    # }

    RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
    HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)

    cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")

    # RR_plot = plot_steps_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize)
    RR_plot = plot_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize, plots_path)

    # save_plot_as_png(RR_plot, plots_path, genomeName, chrID)

    cat(" \n *-*-*-*-*||  The plot is saved as png file in this directory :", plots_path,"  ||*-*-*-*-* \n ")
    # print(RR_plot)

    # save interactive plot as html
    # saveWidget(RR_plot, paste0(plots_path,"RR_plot.html"))

    # saveRDS(RR_plot,paste0(plots_path,"RR_plot.RDS"))

    # display Stats + save them in external file.txt
    # generate_stats(chromosome, RR_object, HCB_fileContent, do_cleaning)

    BrecResultsList = list(RR_plot, RR_object, heteroChromatinBoundaries, telomeres_boundaries)


    return(BrecResultsList) # return RR vector only    ## this return is crucial and needed by other functions, only commented to avoid the printing
}

