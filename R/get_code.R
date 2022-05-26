#' Code corresponding to a single analysis
#'
#' Given a particular set of options for each parameter, extracts the code for performing a
#' single analysis from the code used to declare the multiverse. This function is called automatically 
#' and not exported.
#'
#' @details For a particular parameter assignment (i.e. one set of options that each defined parameter
#' in the multiverse takes), this function rewrites the code passed into the multiverse to output the
#' corresponding code for that set of parameter values --- the analysis for a single universe.
#'
#' This is primarily going to be called by other functions, and perhaps not going to be as useful to
#' the user for anything other than inspecting the rewritten code.
#'
#' @param .code Code that is passed to the multiverse. This is not stripped of calls such as \code{branch_assert()}.
#'
#' @param .assgn A list containing the assignments for each defined parameter in the multiverse
#'
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @importFrom rlang is_missing 
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#'
# wrapper function for get_parameter_code
get_code <- function(.code, .assgn = NULL) {
  # print(.code)
  lapply(.code, get_parameter_code, .assgn)
}

# takes as input: parameter assignment, and an expression (or code) which contains branches
# returns as output an expression (or code) without branches
get_parameter_code <- function(.expr, .assgn) {
  if (!is_missing(.expr)) {
    .expr = rm_branch_assert(.expr)
    
    if (is.call(.expr) ) {
      # Recursive cases
      if (.expr[[1]] == quote(branch)) {
        get_parameter_code(compute_branch(.expr, .assgn), .assgn)
      } else {
        as.call(lapply(.expr, get_parameter_code, .assgn))
      }
    } else {
      # Base case: constants and symbols
      .expr
    }
  } else {
    .expr
  }
}

### this function is to allow people to declare `branch_assert` which does not work rn
rm_branch_assert <- function(.expr) {
  # if the expression is not of length 3, then there isn't a conditional call
  if(length(.expr) == 3) {
    # checks if the rhs of the expression is a branch_assert call
    # rewrites the expression by removing it
    if (is.call(.expr[[3]]) && .expr[[3]][[1]] == quote(branch_assert)) {
      .expr = rm_branch_assert(.expr[[2]])
    }
    
    # checks if the lhs of the expression is a branch_assert call
    # rewrites the expression by removing it
    else if (is.call(.expr[[2]]) && .expr[[2]][[1]] == quote(branch_assert)) {
      .expr = rm_branch_assert(.expr[[3]])
    }
    
    # checks if expression is a %when% conditional
    # if it is, only return the lhs
    else if (is.call(.expr) && .expr[[1]] == quote(`%when%`)) {
      .expr = .expr[[2]]
    }

    .expr
  } else {
    .expr
  }
}

# takes as input:  parameter assignment, and the expression or code containing a branch
# returns as output an expression (or code) without the branch
compute_branch <- function(.expr, .assgn) {
  assigned_parameter_option_name = .assgn[[.expr[[2]]]]
  option_names = lapply(.expr[-1:-2], get_option_name)

  param_assignment <- unlist(lapply(option_names, function(x) x == assigned_parameter_option_name))
  
  # print(.expr[-1:-2])
  # print(param_assignment)
  # print(extract2(.expr[-1:-2], which(param_assignment, arr.ind = TRUE)))
  get_option_value(extract2(.expr[-1:-2], which(param_assignment, arr.ind = TRUE)))
}

get_option_value <- function(x) {
  if (is.call(x) && x[[1]] == "~") {
    return( f_rhs(x) )
  } else {
    return(x)
  }
}





