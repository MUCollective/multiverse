#' Execute parts of, or the entire multiverse
#'
#' @description These are functions which allow the user to execute parts or whole of the multiverse.
#' The user can choose to either execute the default analysis using the \link[execute_default], or a part or
#' whole of the multiverse using the \link[execute_multiverse].
#'
#' @param multiverse The multiverse object
#' 
#' @param parallel (Logical) Indicates whether to run the multiverse in parallel. Defaults to FALSE.
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
#' @importFrom dplyr mutate
#' @importFrom parallel detectCores
#' @importFrom parallel mclapply
#' 
#' @name execute
#' @export
execute_multiverse <- function(multiverse, parallel = FALSE) {
  stopifnot( is.multiverse(multiverse) )
  .m_obj = attr(multiverse, "multiverse")

  execute_all_in_multiverse(.m_obj, parallel)
}

execute_all_in_multiverse <- function(m_obj, .in_parallel) {
  m_tbl = m_obj[['multiverse_table']]
  .universe = seq(1:nrow(m_tbl))
  
  if (.in_parallel) {
    results <- parallel::mclapply(.universe, execute_universe, multiverse = m_obj, mc.cores = 2)
  } else {
    results <- lapply(.universe, execute_universe, multiverse = m_obj)
  }
  
  # results
  #for (i in 1:nrow(m_tbl)) {
  #  execute_universe(m_obj, i)
  #  invisible( lapply(m_tbl$.code[[i]], eval, envir = m_tbl[['.results']][[i]]) )
  #}
}

#' @rdname execute
#' @export
execute_default <- function(multiverse) {
  UseMethod("execute_default")
}

execute_default.multiverse <- function(multiverse) {
  m_obj = attr(multiverse, "multiverse")
  execute_universe(m_obj)
}

execute_universe <- function(multiverse, .universe = NULL) {
  if(is.null(.universe)) {
    .param_assgn = multiverse[['default_parameter_assignment']]
  } else {
    .param_assgn = .universe
  }
  
  if ( is.list(multiverse[['parameters']]) & length(multiverse[['parameters']]) == 0 ) {
    .c = multiverse[['multiverse_table']][['.code']][[1]]
    env = multiverse[['multiverse_table']][['.results']][[1]]
  } else {
    stopifnot(is.numeric(.param_assgn) || !is.null(.param_assgn))
    .c = multiverse[['multiverse_table']][['.code']][[.param_assgn]]
    env = multiverse[['multiverse_table']][['.results']][[.param_assgn ]]
  }
  
  invisible( lapply(.c, eval, envir = env) )
}


