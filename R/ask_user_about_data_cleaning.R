#' ask_user_about_data_cleaning
#'
#' prompt a question to ask the user about performing data cleaning or not
#'
#' display a yes/no question and take a string answer as input.
#'
#' @return a boolean value TRUE for yes / FALSE for no
#' @export

ask_user_about_data_cleaning <- function(){

  do_cleaning = FALSE
  userChoice = readline(prompt = "Would you like to remove outliers from your chromosome ? y/n \n")
  if (userChoice %in% c("y", "Y", "yes", "YES")){
    do_cleaning = TRUE
  }else if(userChoice %in% c("n", "N", "no", "NO")){
    print("No cleaning is applied.")
  }else{
    ask_user_about_data_cleaning()
  }
  return(do_cleaning)
}
