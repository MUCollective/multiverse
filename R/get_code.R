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
#' @param .code Code that is passed to the multiverse stripped of calls such as `branch_assert`.
#' This is the output of `remove_branch_assert` function.
#'
#' @param .assgn A list containing the assignments for each defined parameter in the multiverse
#'
#' @importFrom rlang is_call
#' @importFrom rlang expr_text
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom stringi stri_detect_regex
#'
#' @export
# wrapper function for get_parameter_code
get_code <- function(multiverse, .code, .assgn = NULL) {
  stopifnot( is.r6_multiverse(multiverse))

  if (is.numeric(.assgn)) {
    .assgn = multiverse[['multiverse_table']][['parameter_assignment']][[.assgn]]
  }

  if( length( multiverse[['default_parameter_assignment']] ) != 0 && length(.assgn) == 0 ) {
    #message("Assigning options to parameter from `default_parameter_assignment`")
    .assgn = default_parameter_assignment(multiverse)
  }

  get_parameter_code(.code, .assgn)
}

# takes as input: parameter assignment, and an expression (or code) which contains branches
# returns as output an expression (or code) without branches
get_parameter_code <- function(.expr, .assgn) {
  if (is.call(.expr)) {
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
}

# takes as input:  parameter assignment, and the expression or code containing a branch
# returns as output an expression (or code) without the branch
compute_branch <- function(.expr, .assgn) {
  assigned_parameter_option_name = .assgn[[.expr[[2]]]]
  option_names = lapply(.expr[-1:-2], get_option_name)

  param_assignment <- flatten_lgl(lapply(option_names, function(x) x == assigned_parameter_option_name))

  get_option_value(extract2(.expr[-1:-2], which(param_assignment, arr.ind = TRUE)))
}

get_option_value <- function(x) {
  if (is_call(x, "~")) {
    return( f_rhs(x) )
  } else {
    return(x)
  }
}


get_branch_assert <- function(.expr) {
  if (is_call(safe_f_rhs(.expr)$result, "branch_assert")) {
    .expr = f_lhs(.expr)
    get_branch_assert(.expr)
  } else if (is_call(safe_f_lhs(.expr)$result, "branch_assert")) {
    .expr = f_rhs(.expr)
    get_branch_assert(.expr)
  } else {
    .expr
  }
}


