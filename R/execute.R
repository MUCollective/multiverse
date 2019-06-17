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
#' @importFrom rlang global_env
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate 
#' 
#' @export
execute_default <- function(multiverse, N = NA, list = list()) {
  multiverse.parsed = parse_multiverse(multiverse)
  
  if (! is.na(N) && N >= 1) {
    if ( N > nrow(multiverse.parsed@multiverse_table) ) {
      N = nrow(multiverse.parsed@multiverse_table)
    }
    
    multiverse@current_parameter_assignment = multiverse.parsed@multiverse_table[["parameter_assignment"]][[N]]
  }
  
  multiverse.parsed %>%
      get_code(multiverse@current_parameter_assignment) %>%
      eval(envir = rlang::global_env())
}

#' @export
execute_multiverse <- function(multiverse, .vec = NA) {
  
  multiverse.parsed = parse_multiverse(multiverse)
  
  if (is.na(.vec)) {
    .vec = 1:nrow(multiverse.parsed@multiverse_table)
  } else if(max(.vec) > nrow(multiverse.parsed@multiverse_table)) {
    .vec = 1:nrow(multiverse.parsed@multiverse_table)
  }
  
  multiverse.parsed@multiverse_table = multiverse.parsed@multiverse_table %>%
    mutate(
      code = map(parameter_assignment, ~ get_code(multiverse, .x)),
      results = map(parameter_assignment, function(.x) env())
    ) %>%
    execute_each(.vec)
  
  multiverse.parsed
}

execute_each <- function(.m_tbl, .vec) {
  for (i in .vec) {
    eval( .m_tbl$code[[i]], env = .m_tbl$results[[i]] )
  }
  
  .m_tbl
}

#' @export
print_multiverse <- function(multiverse, .var, .vec = NULL) {
  .var = as_name(enquo(.var))
  
  if (is.null(.vec)) {
    .vec = 1:nrow(multiverse@multiverse_table)
  } else if(max(.vec) > nrow(multiverse@multiverse_table)) {
    .vec = 1:nrow(multiverse@multiverse_table)
  } else if (length(.vec) == 1) {
    .vec = 1:max(.vec)
  }
  
  for (i in .vec) {
      multiverse@multiverse_table$results[[i]][[.var]] %>%
        print()
  }
}