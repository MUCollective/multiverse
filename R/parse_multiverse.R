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
#' @importFrom purrr map_dbl
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr tibble
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' 
#' @export
parse_multiverse <- function(multiverse) {
  stopifnot( is.r6_multiverse(multiverse) )
  
  parameter_conditions_list = get_parameter_conditions( multiverse[['code']] )
  multiverse[['parameters']] = parameter_conditions_list$parameters
  multiverse[['conditions']] = parameter_conditions_list$conditions
  
  if( length( multiverse[['parameters']] ) >= 1) {
    multiverse[['default_parameter_assignment']] = 1
    multiverse[['multiverse_table']] = get_multiverse_table(multiverse, parameter_conditions_list$parameters)
      #parameter_conditions_list$parameters %>% map(~ .x[[1]])
  } else {
    multiverse[['default_parameter_assignment']] = NULL
    multiverse[['multiverse_table']] = get_multiverse_table_no_param(multiverse)
  }
}

get_multiverse_table_no_param <- function(multiverse) {
  tibble::tibble(
    .parameter_assignment = list(NA),
    .code = list( multiverse[['code']] ),
    .results = list( env() )
  )
}

# creates a parameter table from the parameter list
# first creates a data.frame of all permutations of parameter values
# then enforces the constraints defined in the conditions list
get_multiverse_table <- function(multiverse, parameters.list) {
  df <- parameters.list %>%
    expand.grid(KEEP.OUT.ATTRS = FALSE) #unnest( cols = everything())
  
  param.assgn = lapply(seq_len(nrow(df)), function(i) lapply(df, "[[", i)) 
  
  df %>%
    mutate(
      .parameter_assignment = param.assgn,
      .code = map(.parameter_assignment, ~ get_code(multiverse, .x)),
      .results = map(.parameter_assignment, function(.x) env())
    ) %>%
    as_tibble()
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
  stopifnot(identical(names(l1), c("parameters", "conditions")))
  stopifnot(identical(names(l2), c("parameters", "conditions")))
  
  # merge the parameter lists: when a parameter appears in both lists, 
  # take the union of the options provided. the use of two loops and intersect / setdiff
  # up front is to prevent a potentially more expensive linear search inside the loop
  parameters = l1$parameters
  shared_parameters = intersect(names(l1$parameters), names(l2$parameters))
  for (n in shared_parameters) {
    parameters[[n]] = union(l1$parameters[[n]], l2$parameters[[n]])
  }
  l2_only_parameters = setdiff(names(l2$parameters), shared_parameters)
  for (n in l2_only_parameters) {
    parameters[[n]] = l2$parameters[[n]]
  }
  
  list(
    parameters = parameters,
    conditions = union(l1$conditions, l2$conditions)
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
  parameter_options <- map(.branch_call[-1:-2], ~ get_option_name(.x) )
  
  parameter_options_list <- list(parameter_options)
  names(parameter_options_list) <- parameter_name
  
  list(parameters = parameter_options_list, conditions = list())
}

get_option_name <- function(x) {
  if (is.call(x) && x[[1]] == "~") {
    if (is.call(x[[2]])) {
      return( expr_text(x[[2]]) )
    }
    return( x[[2]] )
  } else {
    if (is.call(x)) {
      return(expr_text(x))
    }
    return(x)
  }
}
  

