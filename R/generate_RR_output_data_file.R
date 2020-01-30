#' generate_RR_output_data_file
#'
#' this function generates and saves the output as a .txt data file for recombination rate estimates along a chromosome
#'
#'
#' @param genomeName xxx
#' @param chrID xxx
#' @param chromosome xx
#' @param RR_object xx
#' @param output_path xx
#'
#' @return RR_fileContent
#' @export


generate_RR_output_data_file <- function(genomeName, chrID, chromosome, RR_object, output_path){

    # output1: recombination_rate_estimates_Brec
    RR_fileContent = data.frame(chromosome = chrID, physical_location = chromosome$mb, genetic_location = chromosome$cm, recombination_rate_estimates_Brec = RR_object$regDr)
    # utils::write.table(RR_fileContent, file = stringr::str_replace_all( paste(output_path, genomeName,"- Chr", chrID, "_RRestimates_Brec.txt"), stringr::fixed(" "), ""), sep = "\t",quote = FALSE, row.names = FALSE, col.names = TRUE)

    # DT::datatable(data = RR_fileContent, caption = "Here is is output data also saved in /results as .txt files")

    return(RR_fileContent)
}



