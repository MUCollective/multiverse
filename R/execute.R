#' Execute parts of, or the entire multiverse
#'
#' @description These are functions which allow the user to execute parts or whole of the multiverse.
#' The user can choose to either execute the default analysis using the [execute_default], or a part or
#' whole of the multiverse using the [execute_multiverse].
#'
#' @param multiverse The multiverse object
#'
#' @details Each single analysis within the multiverse lives in a separate environment. We provide convenient functions to access
#' the results for the  default analysis, as well as parts or whole of the multiverse. Each analysis can also be accessed from the
#' multiverse table, under the results column.
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
#' # Computes the analysis for all
#' # universes in the multiverse`
#' M %>%
#'   execute_multiverse()
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @name execute
#' @export
execute_multiverse <- function(multiverse) {
  stopifnot( is.multiverse(multiverse) )
  .m_obj = attr(multiverse, "multiverse")

  execute_all_in_multiverse(.m_obj)
}

execute_all_in_multiverse <- function(m_obj) {
  m_tbl = m_obj[['multiverse_table']]

  for (i in 1:nrow(m_tbl)) {
    invisible( lapply(m_tbl$.code[[i]], eval, envir = m_tbl[['.results']][[i]]) )
  }
}

#' @rdname execute
#' @export
execute_default <- function(multiverse) {
  UseMethod("execute_default")
}

execute_default.multiverse <- function(multiverse) {
  m_obj = attr(multiverse, "multiverse")
  execute_default.Multiverse(m_obj)
}

execute_default.Multiverse <- function(multiverse) {
  .param_assgn = multiverse[['default_parameter_assignment']]

  if ( is.list(multiverse[['parameters']]) & length(multiverse[['parameters']]) == 0 ) {
    env = multiverse[['multiverse_table']][['.results']][[1]]
    .c = multiverse[['multiverse_table']][['.code']][[1]]
  } else {
    stopifnot(is.numeric(.param_assgn) || !is.null(.param_assgn))
    .c = multiverse[['multiverse_table']][['.code']][[.param_assgn]]
    env = multiverse[['multiverse_table']][['.results']][[.param_assgn ]]
  }
  
  invisible( lapply(.c, eval, envir = env) )
}


