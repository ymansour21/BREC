#' Brec_chromosome
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
#' @importFrom stringr str_to_upper
#' @import shiny
#' @import htmlwidgets
#' @return BREC_chromosome_results_list
#' @export

#!usr/bin/env Rscript

# @param physicalMapUnit the measurement unit used for the physical map in the input data file. Default value is megabase pair "Mb". Options: basepair "bp".


Brec_chromosome <- function(genomeName="genome name", inputData, inputChrID, spanFromGui){

  options(encoding = "UTF-8")
  # install_needed_pkgs()
  wd = getwd()
  plots_path = paste0(wd,"/Brec/plots/")
  output_path = paste0(wd,"/Brec/results/")

  using_slidingWindowApproach_for_HCB = TRUE

  # genomeName = get_genome_full_name(inputGenomeName)
  # inputData = load_input_data(genomeName,  separator, physicalMapUnit) # return listOfChromosomes

  chrList = get_list_of_chromosomes(inputData)
  chrID = str_to_upper(inputChrID)

  # inputData = transform_inputData_to_chromosomes(inputData, chrList)
  # chrList = get_list_of_chromosomes(inputData)

  chromosome = get_chromosome_from_inputData(inputData, chrID)
  cat("\n**/**/**/**/**", "chrId : " , chrID ," --- length of this chr : ", nrow(chromosome),"**/**/**/**/**")

  # **** handling data quality issues *******************************************************
    goodDataQuality = data_quality_test(chromosome)

    if (goodDataQuality) { # good data quality 1st iteration
      cat("\n Your data quality is : Good ! ")
      do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
      if(do_5_perCent_cleaning){
        cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
        chromosome = cleanedChromosome_5_perCent
      }
    }else{ # low data quality 1st iteration
      print("Your data quality is : Not good enough !!!")
      do_cleaning = ask_user_about_data_cleaning()
      if(do_cleaning){
        cleanedChromosome = clean_chromosome_data(chromosome, genomeName, chrID)
        chromosome = cleanedChromosome
        goodDataQuality = data_quality_test(chromosome)
        if (goodDataQuality) {  # good data quality 2nd iteration
          print("Your data quality became : Good !")
        }else{  # low data quality 2nd iteration
          print("Your data quality is still : Not good enough !!!")
          do_5_perCent_cleaning = ask_user_about_data_cleaning_5_perCent()
          if(do_5_perCent_cleaning){
            cleanedChromosome_5_perCent = clean_5_perCent_chromosome_data(chromosome, genomeName, chrID)
            chromosome = cleanedChromosome_5_perCent
            goodDataQuality = data_quality_test(chromosome)
          }
        }
      }
      if(!goodDataQuality){
          using_slidingWindowApproach_for_HCB = FALSE
      }
    }
  # ******************************************************************************************

  cat("\n new chr size : " , nrow(chromosome), "\n")
  #**** end of handling data quality issues *************
  spanVal = spanFromGui/100 # default = 0.15
  RR_object = estimate_recombination_rates_loess(chromosome, spanVal)
  # RR_object = estimate_recombination_rates_third_degree_polynomial(chromosome) #== used for plotting for paper
  print(RR_object)
  print("RR done")

  minRR_object = get_min_RR_value_based_on_polynomial(chromosome)
  print(" get minRR done !")

  chrType_object = get_chromosome_type(chromosome,chrID, minRR_object) #, RR_object)
  # chrType = chrType_object$chr_type
  # chrSubType = chrType_object$chr_sub_type

  BREC_chromosome_results_list <- list(genomeName, plots_path, chromosome, RR_object, R2DataFrame2D, chrID, chrType_object, minRR_object, using_slidingWindowApproach_for_HCB)
  names(BREC_chromosome_results_list) <- c("genomeName", "plots_path", "chromosome", "RR_object", "R2DataFrame2D", "chrID", "chrType_object", "minRR_object", "using_slidingWindowApproach_for_HCB")
  return(BREC_chromosome_results_list)

  # after user gui interaction, calling Brec_chromosome_part_2(BREC_chromosome_results_list)
# ======================================================================================================================================================
 #  # RR_object2 = RR_object
 #
 #  R2DataFrame2D = compute_cumulated_R_squared_2directions(chromosome)
 #  print("R2 done")
 #  print(R2DataFrame2D)
 #
 #  # if(using_slidingWindowApproach_for_HCB) {
 #      print("Extracting CB for this chromosome ...")
 #      heteroChromatinBoundaries = extract_CB(chromosome, RR_object, R2DataFrame2D, chrID, chrType, minRR_object)
 #      print("extract centroCB done")
 #      telomeres_boundaries = extract_telomeres_boundaries(chromosome, R2DataFrame2D, chrID, chrType, minRR_object)
 #      print("extract telo CB done")
 #      RR_object = extrapolate_RR_estimates(chromosome, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrID, chrType, minRR_object)
 #      print("extrapolation done")
 #
 #  # }else{
 #  #     heteroChromatinBoundaries = data.frame( heteroBoundLeft = 0, heteroBoundRight = 0, swSize = 0)
 #  #     telomeres_boundaries = data.frame(index_minR2_left = 0, extrapolPhysPos_left = 0, index_minR2_right = 0, extrapolPhysPos_right = 0)
 #  #     print("No chromatin boundaraies estimation and no extrapolation !!! ")
 #  # }
 #
 #  # RR_fileContent  = generate_RR_output_data_file(genomeName, chrID, chromosome, RR_object, output_path)
 #  # HCB_fileContent = generate_HCB_output_data_file(genomeName, chrID, chromosome, heteroChromatinBoundaries, telomeres_boundaries, output_path, chrType)
 #  #
 #  # cat(" \n *-*-*-*-*||  2 outputs are saved as txt files in this directory :", output_path,"  ||*-*-*-*-* \n ")
 #
 #  # RR_plot = plot_steps_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize)
 #  RR_plot = plot_all(chromosome, RR_object, genomeName, toString(chrID), R2DataFrame2D, heteroChromatinBoundaries, heteroChromatinBoundaries$swSize, telomeres_boundaries, plots_path, chrType)
 #
 #  # save_plot_as_png(RR_plot, plots_path, genomeName, chrID)
 #
 #  cat(" \n *-*-*-*-*||  The plot is saved as png file in this directory :", plots_path,"  ||*-*-*-*-* \n ")
 #  # print(RR_plot)
 #
 #  # save interactive plot as html
 #  # saveWidget(RR_plot, paste0(plots_path,"RR_plot.html"))
 #
 #  # saveRDS(RR_plot,paste0(plots_path,"RR_plot.RDS"))
 #
 #  # display Stats + save them in external file.txt
 # # generate_stats(chromosome, RR_object, HCB_fileContent, do_cleaning)
 #
 #  BrecResultsList = list(RR_plot, RR_object, heteroChromatinBoundaries, telomeres_boundaries, chrType)
 #
 # print(chrType_object)
 # print(heteroChromatinBoundaries)
 # print(telomeres_boundaries)
 #  return(BrecResultsList) # return RR vector only    ## this return is crucial and needed by other functions, only commented to avoid the printing
}

