#' Parse the multiverse syntax to identify branches
#'
#' In a multiverse, the user can define different values that a parameter can take using the `branch` call.
#' The `parse_multiverse` identifies the `branch` calls defined in the analysis syntax and parses them into a list of
#' parameters and the corresponding values that each parameter can take.
#'
#' @param multiverse The multiverse object with some code passed to it
#'
#' @param .super_env The parent environment of the multiverse object i.e. the environment
#' in which the multiverse object was called. This is automatically recorded.
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
#' @importFrom tibble as_tibble
#' @importFrom purrr compact
#' @importFrom purrr map_chr
#' @importFrom rlang parse_expr
#' @importFrom rlang expr_deparse
#' @importFrom rlang is_call
#' @importFrom rlang expr_text
#' @importFrom rlang f_rhs
#' @importFrom rlang f_lhs
#'
#' @export
parse_multiverse <- function(multiverse, .super_env) {
  stopifnot( is.r6_multiverse(multiverse) )

  l <- lapply( multiverse[['code']], get_parameter_conditions )
  parameter_conditions_list <- combine_parameter_conditions_list(l)
  
  # keys <- unique(unlist(lapply(l, names)))
  # parameter_conditions_list <- setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)
  
  
  multiverse[['parameters']] = parameter_conditions_list$parameters
  multiverse[['conditions']] = parameter_conditions_list$conditions

  if( length( multiverse[['parameters']] ) >= 1) {
    multiverse[['default_parameter_assignment']] = 1
    multiverse[['multiverse_table']] = get_multiverse_table(multiverse, parameter_conditions_list, .super_env)
  }
  else {
    multiverse[['default_parameter_assignment']] = NULL
    multiverse[['multiverse_table']] = get_multiverse_table_no_param(multiverse, .super_env)
  }
}

get_multiverse_table_no_param <- function(multiverse, .super_env) {
  tibble::tibble(
    .parameter_assignment = list( list() ),
    .code = list( multiverse[['code']] ),
    .results = list( new.env(parent = .super_env) )
  )
}

# creates a parameter table from the parameter list
# first creates a data.frame of all permutations of parameter values
# then enforces the constraints defined in the conditions list
get_multiverse_table <- function(multiverse, parameters_conditions.list, .super_env) {
  df <- data.frame( lapply(expand.grid(parameters_conditions.list$parameters, KEEP.OUT.ATTRS = FALSE), unlist), stringsAsFactors = FALSE )

  param.assgn =  lapply(seq_len(nrow(df)), function(i) lapply(df, "[[", i))

  if (length(parameters_conditions.list$condition) > 0) {
    all_conditions <- parse_expr(paste0(lapply(parameters_conditions.list$conditions, expr_deparse), collapse = "&"))
  } else {
    all_conditions <- expr(TRUE)
  }
  
  .code = multiverse[['code']]

  df <- select(mutate(df, .universe = seq(1:nrow(df))), .universe, everything())
  
  filter(as_tibble(mutate(df,
      .parameter_assignment = param.assgn,
      .code = lapply(.parameter_assignment, function(x) get_code(multiverse, .code, x)),
      .results = lapply(.parameter_assignment, function(x) new.env(parent = .super_env))
  )), eval(all_conditions))
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
    .list$parameters <- modifyList(l[[i]]$parameters, .list$parameters)
    .list$conditions <- modifyList(l[[i]]$conditions, .list$conditions)
  }
  
  .list
}




