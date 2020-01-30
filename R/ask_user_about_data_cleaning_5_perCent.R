#' ask_user_about_data_cleaning_5_perCent
#'
#' prompt a question to ask the user about performing data cleaning_5\% or not
#'
#' display a yes/no question and take a string answer as input
#'
#' @return a boolean value TRUE for yes / FALSE for no
#' @export

ask_user_about_data_cleaning_5_perCent <- function(){

  do_cleaning_5_perCent = FALSE
  userChoice = readline(prompt = "Would you still like to remove outliers representing 5% of your chromosome ? y/n \n")
  if (userChoice %in% c("y", "Y", "yes", "YES")){
      do_cleaning_5_perCent = TRUE
  }else if(userChoice %in% c("n", "N", "no", "NO")){
    print("No cleaning is applied.")
  }else{
    ask_user_about_data_cleaning_5_perCent()
  }
  return(do_cleaning_5_perCent)
}
