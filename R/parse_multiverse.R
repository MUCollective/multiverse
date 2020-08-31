# Names that should be suppressed from global variable check by codetools
# Names used broadly should be put in _global_variables.R
globalVariables(c(".universe", ".parameter_assignment"))

#' Parse the multiverse syntax to identify branches
#'
#' In a multiverse, the user can define different values that a parameter can take using the `branch` call.
#' The `parse_multiverse` identifies the `branch` calls defined in the analysis syntax and parses them into a list of
#' parameters and the corresponding values that each parameter can take. This function is called automatically 
#' and not exported.
#'
#' @param .multiverse The multiverse object which will contain the analysis
#' 
#' @param .expr The expression that is being parsed
#' 
#' @param .code All the code that has been passed to the multiverse
#' 
#' @param .label The label of the code block or inside call which was used to pass the 
#' code being parsed into the multiverse
#'
#' @return The `parse_multiverse` function returns a list of lists. the list of parameters and the list of conditions.
#' The list of parameters is a named list which defines all the values that each defined parameter can take.
#' The list of conditions defines, if any of the parameter values are conditional on a specific value of another
#' parameter, the condition.
#'
#' @importFrom purrr map
#' @importFrom purrr map_dbl
#' @importFrom purrr reduce
#' @importFrom purrr safely
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr tibble
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom purrr compact
#' @importFrom purrr map_chr
#' @importFrom rlang parse_expr
#' @importFrom rlang expr_deparse
#' @importFrom rlang is_call
#' @importFrom rlang expr_text
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#' @importFrom utils modifyList
#' @importFrom utils globalVariables
#' @importFrom methods is
#' 

parse_multiverse <- function(.multiverse, .expr, .code, .label) {
  m_obj <- attr(.multiverse, "multiverse")
  
  # print(.expr)
  
  # if the newly added code is not the last element, implies
  # the user is editing pre-declared parameters. We need to recompute
  # everything after that point
  if (.label %in% names(head(.code, -1))) {
    .code <- .code[-((which(names(.code) == .label)+1):length(.code))]
  }
  
  parameter_conditions_list <- get_parameter_conditions_list( unname(.code) )
  parameters = parameter_conditions_list$parameters
  conditions = parameter_conditions_list$conditions
  
  m_obj$parameters <- parameters
  m_obj$conditions <- conditions
  
  parameter_set <- c(m_obj$parameter_set, setdiff(names(parameters), m_obj$parameter_set))
  
  if (length( m_obj$multiverse_diction$keys() ) == 0) .parent_key = NULL
  else {
    if (.label %in% m_obj$multiverse_diction$keys()) {
      p_idx <- which(m_obj$multiverse_diction$keys() == .label) - 1
      if (p_idx == 0) .parent_key = NULL
      else .parent_key = m_obj$multiverse_diction$keys()[[which(m_obj$multiverse_diction$keys() == .label) - 1]]
    } else {
      .parent_key = unlist(tail(m_obj$multiverse_diction$keys(), 1))
    }
  }
  
  .expr <- list(.expr)
  names(.expr) <- .label
  
  q <- parse_multiverse_expr(.multiverse, .expr, parameters, .parent_key)
  
  invisible( m_obj$multiverse_diction$set(.label, q) )
}

parse_multiverse_expr <- function(multiverse, .expr, .param_options, .parent_block) {
  stopifnot(is(multiverse, "multiverse"))
  
  .m_obj <- attr(multiverse, "multiverse")
  all_conditions <- .m_obj$conditions
  .super_env <- attr(multiverse, "multiverse_super_env")
  
  df <- data.frame( lapply(expand.grid(.param_options, KEEP.OUT.ATTRS = FALSE), unlist), stringsAsFactors = FALSE )
  
  n <- ifelse(nrow(df), nrow(df), 1)
  
  # gets the environments from the previous code block in the multiverse
  # these will be the parents for the new environments created from the execution
  # of a new code block.
  if (is.null(.parent_block)) {
    
    parent.envs <- lapply(seq_len(n), function(x) .super_env)
    parents <- lapply(seq_len(n), function(x) 0)
  } else {
    parent.envs <- lapply(.m_obj$multiverse_diction$get(.parent_block), `[[`, "env")
    parents <- lapply(seq_len(length(.m_obj$multiverse_diction$get(.parent_block))), function(x) x)
  }
  
  lapply(seq_len(n), function(i) {
    .p <- lapply(df, "[[", i)
    
    # new envs per parent env: nrow(df)/length(parent.envs)
    # number of parent env: length(parent.envs)
    # parent: parent.envs[[ceiling(i/(nrow(df)/length(parent.envs)))]]
    list(
      env = new.env(parent = parent.envs[[ceiling(i/(n/length(parent.envs)))]]), 
      parent = parents[[ceiling(i/(n/length(parent.envs)))]],
      parameter_assignment = .p, 
      code = get_code(.expr, .p)
    ) 
  })
}


get_parameter_conditions_list <- function(.c) {
  l <- lapply( .c, get_parameter_conditions )
  
  .p = unlist(lapply(l, function(x) x$parameters), recursive = FALSE)
  
  # check if names are duplicated
  # if yes, then make sure all the option names of the parameter
  # are used. If no, throw an error that it should cover all the
  # options for a parameter.
  if (isTRUE(any(duplicated(names(.p))))) {
    duplicate_names <- duplicated(names(.p), fromLast = TRUE) | duplicated(names(.p))
    if(isFALSE( all(duplicated(.p[duplicate_names], fromLast = TRUE) | duplicated(.p[duplicate_names])) )) {
      stop("reused parameters should have the same number of options and the same names for each option as the original declaration")
    }
    .p <- .p[!duplicated(names(.p))]
  }
  .c = unlist(lapply(l, function(x) x$conditions), recursive = FALSE)
  
  list(
    parameters = .p,
    conditions = .c
  )
}

# takes as input an expression
# returns as output a paramater condition list whose structure
# resembles list(parameter = list(), condition = list())
get_parameter_conditions <- function(.expr) {
  if (is.call(.expr)) {
    child_parameter_conditions <- lapply(.expr, get_parameter_conditions) %>%
      reduce(combine_parameter_conditions)
    
    if (is_call(.expr, "branch")) {
      get_branch_parameter_conditions(.expr) %>%
        combine_parameter_conditions(child_parameter_conditions)
    } else if (is_call(.expr, "branch_assert")) {
      get_branch_assert_condition(.expr) %>%
        combine_parameter_conditions(child_parameter_conditions)
    } else {
      child_parameter_conditions
    }
  } else {
    # Base case: constants and symbols
    list(parameters = list(), conditions = list())
  }
}

# takes as input a `branch` call which contains a parameter name
# and parameter processing options. Parameter option names is optional
# returns as output a list(parameter = list(), condition = list())
get_branch_parameter_conditions <- function(.branch_call) {
  if (! is_symbol(.branch_call[[2]])) {
    stop("parameter names should be symbols")
  }
  parameter_name <- .branch_call[[2]]
  parameter_options <- lapply(.branch_call[-1:-2], get_option_name )
  parameter_conditions <- lapply(.branch_call[-1:-2], function(x) get_condition(x, parameter_name) )
  
  if (length(unique(lapply(parameter_options, typeof))) != 1) {
    stop("all option names should be of the same type")
  }
  
  parameter_options_list <- list(parameter_options)
  names(parameter_options_list) <- as.character(parameter_name)
  
  list( parameters = parameter_options_list, conditions = parameter_conditions )
}

get_branch_assert_condition <- function(.x) {
  list(parameters = list(), conditions = list( expr((!!f_rhs(.x))) ))
}

get_condition <- function(.x, name) {
  .antecedent = get_option_name(.x)
  if (is_call(.x, "~")) {
    .consequent = c(
      get_implies_consequent(f_lhs(.x)),
      get_implies_consequent(f_rhs(.x))
    )[[1]]
  } else {
    .consequent = get_implies_consequent(.x)
  }
  
  if ( !is.null(.consequent)) {
    expr(( !!name != !!.antecedent | !!.consequent ))
  }
}

get_implies_consequent <- function(.x) {
  if( is_call(.x, "%when%") ) {
    f_rhs(.x)
  } else if (is_call(.x, "(") | is_call(.x, "{")) {
    get_implies_consequent(f_rhs(.x))
  }
}

# takes as input two lists list(parameter = list(), condition = list())
# returns as output a list(parameter = list(), condition = list()),
# which is a concatenation of the two lists provided as input
combine_parameter_conditions <- function(l1, l2) {
  stopifnot(identical(names(l1), c("parameters", "conditions")))
  stopifnot(identical(names(l2), c("parameters", "conditions")))
  
  # check that duplicate parameters have the same option names
  
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
    conditions = compact(union(l1$conditions, l2$conditions))
  )
}

get_option_name <- function(x) {
  # when option names are specified
  if (is.call(x) && x[[1]] == "~") {
    if (is.call( f_lhs(x) ) && f_lhs(x)[[1]] == "%when%" ) {
      .expr = f_lhs(f_lhs(x))
      return( create_name_from_expr(.expr) )
    } else if (is.call( f_lhs(x)) ) {
      .expr = f_lhs(x)
      return( create_name_from_expr(.expr) )
    }
    return( f_lhs(x) )
  }
  # when option names are implicitly identified from the expression
  else {
    if (is.call( x ) && x[[1]] == "%when%" ) {
      .expr = f_lhs(x)
      return( create_name_from_expr(.expr) )
    }
    create_name_from_expr(x, TRUE)
  }
}

combine_parameter_conditions_list <- function(l) {
  .list = list(parameters = list(), conditions = list())
  
  for (i in range(1:length(l))) {
    .list$parameters <- modifyList(.list$parameters, l[[i]]$parameters)
    .list$conditions <- modifyList(.list$conditions, l[[i]]$conditions)
  }
  
  .list
}




