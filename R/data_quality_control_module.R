#' data_quality_control_module
#'
#' test data quality based on markers number/density and distribution
#'
#'
#' @param testChromosome xx
#'
#' @return goodDataQuality a boolean value
#' @export



data_quality_control_module <- function(genomeName="genome name", chromosome, inputChrID) {

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
    return(chromosome)
}
