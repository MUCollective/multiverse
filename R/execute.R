#' Execute parts of, or the entire multiverse
#' 
#' @description These are functions which allow the user to execute parts or whole of the multiverse. 
#' The user can choose to either execute the default analysis using the [execute_default], or a part or 
#' whole of the multiverse using the [execute_multiverse].
#' 
#' @param multiverse The multiverse object
#' 
#' @param N Override the default analysis and instead perform the N-th analysis from the multiverse table
#' 
#' @param .vec A vector specifying the range of analysis paths from the multiverse to be executed. Defaults to \code{\link[base]{NA}}
#' which indicates the complete multiverse is to be executed.
#' 
#' @details Each single analysis within the multiverse lives in a separate environment. We provide convenient functions to execute 
#' the results for the  default analysis, as well as parts or whole of the multiverse. The default analysis is executed everytime 
#' code is added to the multiverse, hence we do not expect users to use the [execute_default] function frequently.
#' 
#' @examples
#' \dontrun{
#' #' M <- multiverse()
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
#' M %>%
#'   execute_multiverse()
#' }
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
  execute_default.Multiverse(multiverse, N)
}

execute_default.Multiverse <- function(multiverse, N = NA) {
  .param_assgn = multiverse[['default_parameter_assignment']]
  if ( is.list(multiverse[['parameters']]) & length(multiverse[['parameters']]) == 0 ) {
      .c = multiverse[['code']]
      env = multiverse[['multiverse_table']][['.results']][[1]]
      eval(.c, env)
  } else {
    stopifnot(is.numeric(.param_assgn) || is.null(.param_assgn))
    .c = multiverse %>%  get_code()
    env = multiverse[['multiverse_table']][['.results']][[.param_assgn ]]
    eval(.c, env)
  }
}

#' @export
execute_multiverse <- function(.m, N = NA) {
  stopifnot( is.multiverse(.m) )
  multiverse = attr(.m, "multiverse")
  
  execute_each_in_multiverse(multiverse, N)
}

execute_each_in_multiverse <- function(multiverse, N) {
  m_tbl = multiverse[['multiverse_table']]
  
  for (i in 1:nrow(m_tbl)) {
    eval( m_tbl[['.code']][[i]], env = m_tbl[['.results']][[i]] )
  }
}
