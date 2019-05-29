#' Parse the multiverse syntax to identify branches
#' 
#' In a multiverse, the user can define different values that a parameter can take using the `branch` call.
#' The `parse_multiverse` identifies the `branch` calls defined in the analysis syntax and parses them into a list of 
#' parameters and the corresponding values that each parameter can take.
#' 
#' @param M The multiverse object with some code passed to it
#' 
#' @return The `parse_multiverse` function returns a list of lists. the list of parameters and the list of conditions. 
#' The list of parameters is a named list which defines all the values that each defined parameter can take. 
#' The list of conditions defines, if any of the parameter values are conditional on a specific value of another 
#' parameter, the condition.
#' 
#' @import rlang
#' @importFrom purrr map
#' @export
#' 
# wrapper function for get_parameter_conditions
# takes as input a multiverse and parses it
# returns the output of get_parameter_conditions
parse_multiverse <- function(M) {
  parameter_conditions_list <- get_parameter_conditions( attr(M, "code") )
  attr(M, "parameters") <- parameter_conditions_list
}

# takes as input an expression
# returns as output a paramater condition list whose structure
# resembles list(parameter = list(), condition = list())
get_parameter_conditions <- function(.expr) {
  switch_expr(.expr,
    # Base cases
    constant = , # falls through; the next element is evaluated
    symbol = list(parameters = list(), conditions = list()),
    
    # Recursive cases
    call = {
      child_parameter_conditions <- map(.expr, get_parameter_conditions) %>%
        reduce(combine_parameter_conditions)
      
      if (is_call(.expr, "branch")) {
        get_branch_parameter_conditions(.expr) %>%
          combine_parameter_conditions(child_parameter_conditions)
      } else {
        child_parameter_conditions
      }
    }
  )
}

# takes as input two lists list(parameter = list(), condition = list())
# returns as output a list(parameter = list(), condition = list()), 
# which is a concatenation of the two lists provided as input
combine_parameter_conditions <- function(l1, l2) {
  list(
    parameters = c(l1$parameters, l2$parameters),
    conditions = c(l1$conditions, l2$conditions)
  )
}

# takes as input a `branch` call which contains a parameter name 
# and parameter processing options. Parameter option names is optional
# returns as output a list(parameter = list(), condition = list())
get_branch_parameter_conditions <- function(.branch_call) {
  if (! is_symbol(.branch_call[[2]])) {
    stop("parameter names should be symbols")
  }
  parameter_name <- as.character(.branch_call[[2]])
  parameter_options <- map(.branch_call[-1:-2], ~ .x[[2]])
  
  parameter_options_list <- list(parameter_options)
  names(parameter_options_list) <- parameter_name
  
  list(parameters = parameter_options_list, conditions = list())
}

