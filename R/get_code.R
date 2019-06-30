#' Code corresponding to a single analysis
#' 
#' Given a particular set of options for each parameter, extracts the code for performing a 
#' single analysis from the code used to declare the multiverse.
#' 
#' @details For a particular parameter assignment (i.e. one set of options that each defined parameter 
#' in the multiverse takes), this function rewrites the code passed into the multiverse to output the 
#' corresponding code for that set of parameter values --- the analysis for a single universe.
#' 
#' This is primarily going to be called by other functions, and perhaps not going to be as useful to 
#' the user for anything other than inspecting the rewritten code.
#' 
#' @param multiverse The multiverse object with some code passed to it
#' 
#' @param .assgn A list containing the assignments for each defined parameter in the multiverse
#' 
#' @import rlang
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom magrittr extract2
#' 
#' @export
# wrapper function for get_parameter_code
get_code <- function(multiverse, .assgn = list()) {
  if( length( multiverse[['current_parameter_assignment']] ) != 0 && length(.assgn) == 0 ) {
    message("Assigning options to parameter from `current_parameter_assignment`")
    .assgn = multiverse[['current_parameter_assignment']]
  }
  get_parameter_code( multiverse[['code']], .assgn)
}

# takes as input: parameter assignment, and an expression (or code) which contains branches
# returns as output an expression (or code) without branches
get_parameter_code <- function(.expr, .assgn) {
  switch_expr(.expr,
    # Base cases
    constant = , # falls through; the next element is evaluated
    symbol = .expr,
    
    # Recursive cases
    call = {
      if (is_call(.expr, "branch")) {
        compute_branch(.expr, .assgn) %>%
          get_parameter_code(.assgn)
      } else {
        as.call(map(.expr, ~ get_parameter_code(.x, .assgn)))
      }
    }
  )
}

# takes as input:  parameter assignment, and the expression or code containing a branch
# returns as output an expression (or code) without the branch
compute_branch <- function(.expr, .assgn) {
  assigned_parameter_option_name <- .assgn[[.expr[[2]]]]
  parameter_values <- .expr[-1:-2]
  
  assigned_parameter_option_value <- map2(parameter_values, 
                                          assigned_parameter_option_name, function(.x, .y) .y %in% as.character(.x) ) %>% 
    flatten_lgl() %>%
    which(arr.ind = TRUE)
  
  parameter_values %>%
    extract2(assigned_parameter_option_value) %>%
    extract2(3)
}