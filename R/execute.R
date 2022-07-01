#' Execute parts of, or the entire multiverse
#'
#' @description These are functions which allow the user to execute parts or whole of the multiverse.
#' The user can choose to either execute the default analysis using the \code{execute_universe()}, or the
#' whole of the multiverse using the \code{execute_multiverse()}.
#'
#' @param multiverse The multiverse object
#' 
#' @param parallel Indicates whether to execute the multiverse analysis in parallel. If TRUE, multiverse makes use of
#' \code{future::future} as backend to support parallel processing. Requires configuration of \code{future::plan}. Defaults to FALSE.
#'
#' @param progress Indicates whether to include a progress bar for each step of the execution. Defaults to FALSE.
#' 
#' @param .universe Indicate which universe to execute, if the user wants to execute a specific combination
#' of the parameters using \code{execute_universe()}. Defaults to NULL, which will execute the first (default) analysis.
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
#' @importFrom future.apply future_lapply
#' @importFrom berryFunctions tryStack
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' 
#' @name execute
#' @export
execute_multiverse <- function(multiverse, parallel = FALSE, progress = FALSE) {
  m_obj <- attr(multiverse, "multiverse")
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  
  .level = min(m_obj$unchanged_until, m_obj$exec_all_until)
  .to_exec = tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  
  .m_list <- m_diction$as_list()[.to_exec] # list of unevaluated (vertical) steps in the multiverse
  l = unlist(unname(lapply(.m_list, length)))
  cumulative_l = cumsum(c(0, l))[1:length(l)]
  steps = sum(l)
  
  if (progress) {
    pb = txtProgressBar(style = 3)
  } else {
    pb = NULL
  }
  
  # we execute each step in sequence from top to bottom
  .res <- mapply(exec_all, .m_list, cumulative_l, MoreArgs = list(progressbar = pb, steps = steps, in_parallel = parallel))
  
  
  # update multiverse diction with new elements
  m_diction$update(ordered_dict(.res))
  
  m_obj$exec_all_until <- length(m_diction$as_list())
}

# executes all the options resulting from decisions declared within a single code 
# block in parallel. We do not need to communicate with other universes at this step
# making it suitable for parallelisation.
exec_all <- function(list_block_exprs, current, progressbar, steps, in_parallel) {
  .code_list = lapply(list_block_exprs, `[[`, "code")
  .env_list = lapply(list_block_exprs, `[[`, "env")
  
  # .res contains the updated list which includes environments that 
  # contain the result of the evaluated expression
  # as well as the error stack
  if (in_parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      warning("")
      app = lapply
    } else {
      app = future_lapply
    }
  } else {
    app = lapply
  }
  
  .res <- app(seq_along(.code_list), execute_each, .code_list, .env_list, progressbar, current, steps)
  
  .error_messages = lapply(.res, function(x) x$ts)
  .res_envs = lapply(.res, function(x) x$env)
  
  # updates the old environments in the dictionary with the new environments
  mapply(function(old_env, new_env) {list2env(as.list(new_env), envir = old_env)}, .env_list, .res_envs)
  
  lapply(seq_along(.res), function(i, x) {
    if (is(x[[i]], "try-error"))  {
      warning("error in universe ", i)
      cat(x[[i]], "\n\n")
    }
  }, x = .error_messages)
  
  .error_messages = lapply(.error_messages, function(x) { ifelse(is(x, "try-error"), x, NA) })
  
  # save the errors to `list_block_exprs` (current level of m_list)
  # return the updated `list_block_exprs`
  list(
    mapply(function(x, y) {x$error = y; return(list(x))}, list_block_exprs, .error_messages)
  )
}

execute_each <- function(i, code, env_list, pb, curr, n) {
  .c = code[[i]]
  .e = env_list[[i]]
  # .env = new.env()
  .error_stack = tryStack(lapply(.c, eval, envir = .e), silent = TRUE)
  
  if (!is.null(pb)) {
    setTxtProgressBar(pb, (curr + i)/n)
  }
  
  list(env = .e, ts = .error_stack)
}


#' @rdname execute
#' @export
execute_universe <- function(multiverse, .universe = 1) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  # .level = attr(multiverse, "multiverse")$unchanged_until
  # .level = attr(multiverse, "multiverse")$exec_all_until
  # if (is.na(.level)) .level = 0
  # we probably don't want to use cached execution when executing a single universe
  
  .order = get_exec_order(m_diction, .universe, length(m_diction$keys()))
  .to_exec = seq_len(m_diction$size()) #tail(seq_len(m_diction$size()), n = m_diction$size() - .level)
  
  .m_list <- m_diction$as_list()[.to_exec]
  
  .res <- mapply(exec_in_order, .m_list, .to_exec, MoreArgs = list(.universes = .order))
}

execute_code_from_universe <- function(.c, .env = globalenv()) {
  tryStack(lapply(.c, eval, envir = .env), silent = TRUE)
}

# for a universe, get the indices which need to be executed
get_exec_order <- function(.m_diction, .uni, .level) {
  if (.level > 1){
    .p <- .m_diction$get(.m_diction$keys()[[.level]])[[.uni]]$parent
    c(get_exec_order(.m_diction, .p, .level - 1), .uni)
  } else {
    .uni
  }
}

exec_in_order <- function(.universe_list, .universes, .i) {
  x <- .universe_list[[ .universes[[.i]] ]]
  
  .exec_res <- execute_code_from_universe(x$code, x$env)
  if (is(.exec_res, "try-error"))  warning("error in default universe", "\n", .exec_res)
  else if (is(.exec_res, "warning"))  warning("warning in default universe", "\n", .exec_res)
}




