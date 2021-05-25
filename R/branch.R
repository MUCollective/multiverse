#' Define multiple analysis paths for a step in the multiverse
#' 
#' The `branch()` function allows the user to define multiple analysis options for a particular step
#' in the analysis.
#' 
#' @details For every step in the analysis, there may be more than one analysis option. We use `branch()`
#' to declare these different analysis options. Each branch is characterised by a parameter. The first
#' argument passed into the branch is the parameter. 
#' 
#' All the other arguments passed into branch are the different analysis options corresponding to 
#' that parameter (that particular step in the analysis process). Naturally, at least two or more 
#' options should be declared. Thus, the branch function will provide a warning
#' if the total number arguments passed is less than three.
#' 
#' The `branch()` function does not support nested branches. Thus, we cannot declare a branch within a branch.
#' 
#' @param parameter A string to identify the branch. Each branch is characterised using a parameter which takes
#' different options.
#'  
#' @param ... Different options for completing a particular step in the analysis. Each option is
#' declared as <option_name> ~ <option_calculation>. See examples for more details.
#' 
#' @param .options Declare a continuous value as the option of a parameter using a sequence (see examples for details), 
#' and the expanded sequence will be included as options for that parameter in the multiverse. 
#'  
#' @examples 
#' \donttest{
#' library(dplyr)
#' 
#' # declaring multiple options for a data processing step (calculating a new variable) 
#' data(durante)
#' data.durante <- durante
#' 
#' data.durante  %>%
#'     mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
#'     mutate( NextMenstrualOnset = StartDateNext)
#' 
#' 
#' # create a multiverse object
#' M <- multiverse()
#' 
#' # if the variable `NextMenstrualOnset` can be calculated in more than one way
#' # we can use branch to declare all the different analysis options. This will 
#' # replace the current analysis within the current syntax.
#' # Since this is a multiverse analysis, this is only supported within a multiverse object. Hence:
#' inside(M, {
#'   df <- data.durante  %>%
#'     mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
#'     mutate( NextMenstrualOnset = branch(menstrual_calculation, 
#'                                    "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
#'                                    "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
#'                                    "mc_option3" ~ StartDateNext)
#'     )
#' })
#' 
#' # continuous option values for a parameter
#' inside(M, {
#'   branch(foo, "option1" ~ 1, .options = 1:10)
#' })
#'
#' M2 = multiverse()
#' # alternatively, we could specify how we want the vector to be expanded
#' # for continuous parameters
#' inside(M2, {
#'   branch(foo, "option1" ~ 1, .options = seq(0, 1, by = 0.1))
#' })
#' }
#' 
#' @name branch
NULL
