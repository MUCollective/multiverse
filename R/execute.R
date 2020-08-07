#' Execute parts of, or the entire multiverse
#'
#' @description These are functions which allow the user to execute parts or whole of the multiverse.
#' The user can choose to either execute the default analysis using the \code{\link{execute_default}}, or a part or
#' whole of the multiverse using the \code{\link{execute_multiverse}}.
#'
#' @param multiverse The multiverse object
#' 
#' @param cores Indicates the number of cores to use. This will execute the entire multiverse in parallel. 
#' Defaults to NULL (running in a single core)
#' 
#' @param .universe Indicate which universe to execute, if the user wants to execute a specific combination
#' of the parameters using `execute_universe`. Defaults to NULL, which will execute the first (default) analysis.
#'
#' @details Each single analysis within the multiverse lives in a separate environment. 
#' We provide convenient functions to access the results for the  default analysis, as well as 
#' parts or whole of the multiverse. Each analysis can also be accessed from the multiverse table,
#' under the results column.
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
execute_multiverse <- function(multiverse, cores = 1L) {
  m_tbl = attr(multiverse, "multiverse")[['multiverse_table']]
  .code_list = m_tbl[['.code']]
  .env_list = m_tbl[['.results']]
  .universe = seq(1:nrow(m_tbl))
  
  results <- parallel::mcmapply(execute_code_from_universe, .code_list, .env_list, mc.cores = cores)
}


#' @rdname execute
#' @export
execute_default <- function(multiverse) {
  UseMethod("execute_default", multiverse)
}

#' @rdname execute
#' @export
execute_default.default <- function(multiverse) {
  stop("execute_default can only be called on objects of class multiverse")
}

#' @rdname execute
#' @export
execute_default.multiverse <- function(multiverse) {
  execute_universe(multiverse)
}

#' @rdname execute
#' @export
execute_universe <- function(multiverse, .universe = NULL) {
  m_obj = attr(multiverse, "multiverse")
  
  if(is.null(.universe)) {
    .param_assgn = m_obj[['default_parameter_assignment']]
  } else {
    .param_assgn = .universe
  }
  
  if ( is.list(m_obj[['parameters']]) & length(m_obj[['parameters']]) == 0 ) {
    .c = m_obj[['multiverse_table']][['.code']][[1]]
    env = m_obj[['multiverse_table']][['.results']][[1]]
  } else {
    stopifnot(is.numeric(.param_assgn) || !is.null(.param_assgn))
    .c = m_obj[['multiverse_table']][['.code']][[.param_assgn]]
    env = m_obj[['multiverse_table']][['.results']][[.param_assgn ]]
  }
  
  execute_code_from_universe(.c, env)
}

execute_code_from_universe <- function(.c, .env) {
  invisible( lapply(.c, eval, envir = .env) )
}

execute_multiverse2 <- function(multiverse, cores = getOption("mc.cores", 1L)) {
  stopifnot( is.multiverse(multiverse) )
  .m_obj = attr(multiverse, "multiverse")
  .universe = seq(1:nrow(expand(multiverse)))
  
  results <- mclapply(.universe, execute_universe, multiverse = .m_obj, mc.cores = cores)
}


