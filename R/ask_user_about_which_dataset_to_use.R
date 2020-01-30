#' Ask the user about which dataset to use
#'
#' prompt a question to ask weather to select the user's own dataset or directly use the default Drosophila melanogaster R6 pre-loaded dataset.
#'
#' display a yes/no question and take a string answer as input
#'
#' @return a boolean value TRUE for yes / FALSE for no
#' @export

ask_user_about_which_dataset_to_use <- function(){

    useDefaultDataset = TRUE
    userChoice = readline(prompt = "Would you like to use your own dataset ? y/n \n")
    if (userChoice %in% c("y", "Y", "yes", "YES")){
        useDefaultDataset = FALSE
    }else if(userChoice %in% c("n", "N", "no", "NO")){
        print("The default dataset which is going to be used is of the genome: Drosophila melanogaster R6")
    }else{
        ask_user_about_which_dataset_to_use()
    }
    return(useDefaultDataset)
}
