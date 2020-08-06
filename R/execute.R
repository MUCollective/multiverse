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
#' @importFrom parallel mcmapply
#' 
#' @name execute
#' @export
execute_multiverse <- function(multiverse, .universe = 1) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  n <- m_diction$as_list() %>% length()
  .level = attr(multiverse, "multiverse")$unchanged_until
  
  .to_exec = tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  invisible(lapply(tail(seq_len(n), n = (n - .level)), exec_all, .m_diction = m_diction) )
}

#' @rdname execute
#' @export
execute_universe <- function(multiverse, .universe = 1) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  .level = attr(multiverse, "multiverse")$unchanged_until
  
  .order = get_exec_order(m_diction, .universe, length(m_diction$keys()))
  .to_exec = tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  
  invisible(lapply(.to_exec, exec_in_order, .m_diction = m_diction, .universes = .order) )
}

execute_code_from_universe <- function(.c, .env = globalenv()) {
  e <- tryCatch( invisible( lapply(.c, eval, envir = .env) ), error = function(e) e )
  if (is(e, "error")) list(e) else .env
}

# for a universe, get the indices which need to be executed
get_exec_order <- function(.m_diction, .uni, .level) {
  if (.level >= 1){
    .p <- .m_diction$get(.m_diction$keys()[[.level]])[[.uni]]$parent
    c(get_exec_order(.m_diction, .p, .level - 1), .uni)
  }
}

exec_in_order <- function(.m_diction, .universes, .i) {
  x <- .m_diction$get(as.character(.i))[[ .universes[[.i]] ]]
  
  execute_code_from_universe(x$code, x$env)
}

exec_all <- function(.m_diction, .i) {
  x <- .m_diction$get(as.character(.i))
  
  .code_list = lapply(x, `[[`, "code")
  .env_list = lapply(x, `[[`, "env")
  
  mapply(execute_code_from_universe, .code_list, .env_list)
}



