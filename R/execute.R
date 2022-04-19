#' Execute parts of, or the entire multiverse
#'
#' @description These are functions which allow the user to execute parts or whole of the multiverse.
#' The user can choose to either execute the default analysis using the \code{\link{execute_universe}}, or a part or
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
#' \donttest{
#' library(dplyr)
#' 
#' M <- multiverse()
#' 
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
#' @importFrom berryFunctions tryStack
#' 
#' @name execute
#' @export
execute_multiverse <- function(multiverse, cores = getOption("mc.cores", 1L)) {
  m_obj <- attr(multiverse, "multiverse")
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  
  .level = min(m_obj$unchanged_until, m_obj$exec_all_until)
  .to_exec = tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  
  .m_list <- m_diction$as_list()[.to_exec] # list of unexecuted (vertical) steps in the multiverse
  .res <- lapply(.m_list, exec_all, cores = cores) # we execute each step in sequence from top to bottom
  
  m_obj$exec_all_until <- length(m_diction$as_list())
}

# executes all the options resulting from decisions declared within a single code 
# block in parallel. We do not need to communicate with other universes at this step
# making it suitable for parallelisation.
exec_all <- function(x, cores) {
  .code_list = lapply(x, `[[`, "code")
  .env_list = lapply(x, `[[`, "env")
  
  .res <- mcmapply(execute_code_from_universe, .code_list, .env_list, mc.cores = cores)
  
  lapply(seq_along(.res), function(i, x) {
    if (is(x[[i]], "try-error"))  {
      warning("error in universe ", i, "\n")
       cat(x[[i]])
    }
  }, x = .res)
}


#' @rdname execute
#' @export
execute_universe <- function(multiverse, .universe = 1) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  .level = attr(multiverse, "multiverse")$unchanged_until
  
  .order = get_exec_order(m_diction, .universe, length(m_diction$keys()))
  .to_exec = tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  
  .m_list <- m_diction$as_list()[.to_exec]
  
  .res <- mapply(exec_in_order, .m_list, .to_exec, MoreArgs = list(.universes = .order))
}

execute_code_from_universe <- function(.c, .env = globalenv()) {
  # lapply(.c, eval, envir = .env)
  tryStack(lapply(.c, eval, envir = .env), silent = TRUE)
}

# for a universe, get the indices which need to be executed
get_exec_order <- function(.m_diction, .uni, .level) {
  if (.level >= 1){
    .p <- .m_diction$get(.m_diction$keys()[[.level]])[[.uni]]$parent
    c(get_exec_order(.m_diction, .p, .level - 1), .uni)
  }
}

exec_in_order <- function(.universe_list, .universes, .i) {
  x <- .universe_list[[ .universes[[.i]] ]]
  
  .exec_res <- execute_code_from_universe(x$code, x$env)
  if (is(.exec_res, "try-error"))  warning("error in default universe", "\n", .exec_res)
  else if (is(.exec_res, "warning"))  warning("warning in default universe", "\n", .exec_res)
}




