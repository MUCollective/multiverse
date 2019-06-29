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
execute_default <- function(multiverse, N = NA, list = list()) {
  .m_name = find_chain_lhs() # this would have to be recursive for objects of type call
  parent = pryr::where(deparse(.m_name))
  
  .assgn = multiverse@current_parameter_assignment
  
  a_tbl = multiverse@multiverse_table %>%
    select( names(multiverse@parameters) )
  
  if ( !is.numeric(.assgn) ) {
    .n = length(multiverse@parameters)
    .idx = apply(a_tbl, 1, function(.x) sum(.x == .assgn) == .n) %>%
      match(TRUE, .)
  } else {
    .idx =.assgn
  }
  
  .code_expr = multiverse@multiverse_table[['code']][[.idx]]
  
  eval( .code_expr, eval(call("slot", .m_name, 'multiverse_table'), parent)[['results']][[.idx]] )
}

execute_multiverse <- function(multiverse, .vec = NA, parent = NULL) {
  .m_name = find_chain_lhs()
  parent = pryr::where(deparse(.m_name))
  
  if (is.null(parent)) parent = globalenv() #parent.frame()
  env = new.env( parent = parent )
  env$m_tbl = multiverse@multiverse_table
  
  if (is.na(.vec)) {
    .vec = 1:nrow( env$m_tbl )
  } else if(max(.vec) > nrow( env$m_tbl )) {
    .vec = 1:nrow( env$m_tbl )
  }
  
  env$m_tbl = env$m_tbl %>%
    execute_each(.vec)
  
  eval( call( "<-", call("@", .m_name, "multiverse_table"), env$m_tbl ) , parent )
}

execute_each <- function(m_tbl, .vec) {
  for (i in .vec) {
    eval( m_tbl$code[[i]], env = m_tbl$results[[i]] )
  }
  
  m_tbl
}

find_chain_lhs <- function() {
  i <- 1
  while(!("chain_parts" %in% ls(envir=parent.frame(i))) && i < sys.nframe()) {
    i <- i+1
  }
  
  # the environment where magrittr
  # stores the chain as lhs and rhs
  .env = parent.frame(i)
  .env$lhs
}

