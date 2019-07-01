#' Execute parts of, or the entire multiverse
#' 
#' @description These are functions which allow the user to execute the multiverse. The user can choose to either execute the default analysis
#' using the execute_default() or parts or whole of the multiverse using the execute_multiverse(). These functions allow the user 
#' to interactively inspect the code.
#' 
#' @param multiverse The multiverse object
#' 
#' @param N Override the default analysis and instead perform the N-th analysis from the multiverse table
#' 
#' @param .vec A vector specifying the range of analysis paths from the multiverse to be executed. Defaults to \code{\link[base]{NA}}
#' which indicates the complete multiverse is to be executed.
#' 
#' @details Each single analysis within the multiverse lives in a separate environment. We provide convenient functions to access 
#' the results for the  default analysis, as well as parts or whole of the multiverse. Each analysis can also be accessed from the
#' multiverse table, under the results column. 
#' 
#' @examples
#' \dontrun{
#' #' M <- new("multiverse")
#' inside(M, {
#'   data <- rnorm(100, 50, 20)
#'   
#'   x.mean <- mean(data, trim = branch(
#'     trim_values, 
#'     "trim_none" ~ 0,
#'     "trim_1pc" ~ 0.05,
#'     "trim_5pc" ~ 0.025,
#'     "trim_10pc" ~ 0.05
#'   ))
#' })
#' 
#' # Prints the default analysis. Here, the default
#' # analysis is the one conducted with `trim_none`
#' parse_multiverse(M) %>%
#' execute_default() %>%
#'   print()
#' }
#' 
#' # Will print the results from all the multiverses
#' parse_multiverse(M) %>%
#' execute_multiverse() %>%
#'   print_multiverse()
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate 
#' 
#' @export
execute_default <- function(multiverse, N = NA) {
  UseMethod("execute_default")
}

execute_default.multiverse <- function(.m, N = NA) {
  multiverse = attr(.m, "multiverse")
  
  .param_assgn = multiverse[['default_parameter_assignment']]
  stopifnot(is.numeric(.param_assgn))
  
  env = multiverse[['multiverse_table']][['results']][[.param_assgn ]]
  
  eval(.c, env)
}

execute_default.Multiverse <- function(multiverse, N = NA) {
  .param_assgn = multiverse[['default_parameter_assignment']]
  stopifnot(is.numeric(.param_assgn))
  
  .c = multiverse %>%  get_code()
  
  env = multiverse[['multiverse_table']][['results']][[.param_assgn ]]
  
  eval(.c, env)
}
