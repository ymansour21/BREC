#' load_input_data
#'
#' read input dataset from a csv/txt file into a dataframe
#'
#' @seealso read.table
#'
#' @param genomeName string of the name of the genome (organism) forwhich the data is provided. This will be used for output and plot file names. Default value is an empty string "".
#' @param separator string for the character used for seperating values in the input data file. Default value is the tab separator "\\t". Options : ","  ";"  " ".
#' @param physicalMapUnit the measurement unit used for the physical map in the input data file. Default value is megabase pair "Mb". Options: basepair "bp".
#'
#' @export

load_input_data <- function(genomeName, separator, physicalMapUnit){

    # useDefaultDataset = ask_user_about_which_dataset_to_use()

    if(useDefaultDataset){
        rawDataFile = paste0(getwd(),"/data/Dmel_R6_formatted_v2.csv")
    }else{
        rawDataFile = file.choose()
    }
    if(is.na(rawDataFile)){
        useDefaultDataset = ask_user_about_which_dataset_to_use()
        if(useDefaultDataset){
            rawDataFile = paste0(getwd(),"/data/Dmel_R6_formatted_v2.csv")
        }else{
            rawDataFile = file.choose()
        }
        if(is.na(rawDataFile)){
            inputData = NULL
            stop("You did not select any dataset to use !!!")
        }
    }else{
        inputData <- utils::read.table(rawDataFile, header = TRUE, sep = separator)
        colnames(inputData)
        if(physicalMapUnit != "Mb"){
            inputData$mb = round((inputData$mb / 10^6), 2)  # convert to Mb
        }
        chrList = get_list_of_chromosomes(inputData)
        # if (dataByChrArms){ # test if 2 arms are compatible, else, break and display an erroe msg
        #     inputData = transform_inputData_to_chromosomes(inputData, chrList)
        # }
    }

    return(inputData)
}
