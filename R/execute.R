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
#' @importFrom furrr future_map
#' @importFrom berryFunctions tryStack
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' 
#' @name execute
#' @export
execute_multiverse = function(multiverse, parallel = FALSE, progress = FALSE) {
  if (getOption("tree", 1)) {
    execute_tree(multiverse, parallel, progress)
  } else {
    execute_linear(multiverse, parallel, progress)
  }
}

execute_tree <- function(multiverse, parallel, progress) {
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


# execute linear
# As an alternative to the tree-based hierarchical structure, we provide a simple linear execution framework
# which avoids redundant computation but makes it significantly easier to perform execution in parallel
execute_linear <- function(multiverse, parallel, progress) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  m_tbl = expand(multiverse)
  .code_list = m_tbl[['.code']]
  .env_list = m_tbl[['.results']]
  
  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      warning("Package furrr is required to perform parallel execution.")
      app = lapply
    } else {
      app = future_map
    }
  } else {
    app = lapply
  }
  
  .res <- app(seq_along(.code_list), execute_linear_universe, .code_list, .env_list)
  
  handle_results_and_envs(.res, .env_list)
  .error_messages = handle_errors(.res)
  
  .m_list = tail(attr(multiverse, "multiverse")$multiverse_diction$as_list(), 1)[[1]]
 
  # save the errors to `list_block_exprs` (current level of m_list)
  # return the updated `list_block_exprs`
  .diction_res = list(
    mapply(function(x, y) {x$error = y; return(list(x))}, .m_list, .error_messages)
  )
  names(.diction_res) = length(m_diction$as_list())
  
  m_diction$update(ordered_dict(.diction_res))
  attr(multiverse, "multiverse")$exec_all_until <- length(m_diction$as_list())
}

# returns a named list of length 2 comprising of
# a environment and a error_stack object
execute_linear_universe <- function(i, code, env_list) {
  .c = code[[i]]
  .e = env_list[[i]]
  .error_stack = tryStack( invisible(lapply(.c, eval, envir = .e)), silent = TRUE)
  list(env = .e, ts = .error_stack)
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
    if (!requireNamespace("furrr", quietly = TRUE)) {
      warning("")
      app = lapply
    } else {
      app = future_map
    }
  } else {
    app = lapply
  }
  
  .res <- app(seq_along(.code_list), execute_each, .code_list, .env_list, progressbar, current, steps)
  
  handle_results_and_envs(.res, .env_list)
  .error_messages = handle_errors(.res)
  
  # save the errors to `list_block_exprs` (current level of m_list)
  # return the updated `list_block_exprs`
  list(
    mapply(function(x, y) {x$error = y; return(list(x))}, list_block_exprs, .error_messages)
  )
}

# input: .res, .env_list
# output: NULL
handle_results_and_envs = function(.res, .env_list) {
  # creates a list of result environments
  .res_envs = lapply(.res, function(x) x$env)
  
  # updates the old environments in the dictionary with the new environments
  mapply(function(old_env, new_env) {list2env(as.list(new_env), envir = old_env)}, .env_list, .res_envs)
}

handle_errors = function(.res, .indices = NULL) {
  # creates a list of error messages
  .error_messages = lapply(.res, function(x) x$ts)
  
  if (is.null(.indices)) .indices = seq_along(.res)
  
  # prints the error messages
  lapply(seq_along(.res), function(i, x) {
    if (is(x[[i]], "try-error"))  {
      warning("error in universe ", i)
      cat("\n", x[[i]], "\n\n")
    }
  }, x = .error_messages)
  
  # returns the error messages (for multiverse table)
  lapply(.error_messages, function(x) { ifelse(is(x, "try-error"), x, NA) })
}

execute_each <- function(i, code, env_list, pb, curr, n) {
  .c = code[[i]]
  .e = env_list[[i]]
  .error_stack = tryStack(lapply(.c, eval, envir = .e), silent = TRUE)
  
  if (!is.null(pb)) {
    setTxtProgressBar(pb, (curr + i)/n)
  }
  
  list(env = .e, ts = .error_stack)
}

#' @rdname execute
#' @export
execute_universe = function(multiverse, .universe = 1, parallel = FALSE, progress = FALSE) {
  m_diction = attr(multiverse, "multiverse")$multiverse_diction

  m_tbl = expand(multiverse)
  .code_list = m_tbl[[".code"]]
  .env_list = m_tbl[[".results"]]
  
  if ((length(.universe) > 1) & parallel) {
    .res = future_map(.universe, execute_linear_universe, .code_list, .env_list, .progress = progress)
  } else {
    .res = lapply(.universe, execute_linear_universe, .code_list, .env_list)
  }
  
  .res_envs = lapply(.res, function(x) x$env)
  # .errors = lapply(.res, function(x) x$ts)
  .errors = lapply(seq_along(.env_list), 
                   function(x, y, i) { 
                     if(x %in% i) y[[1]]$ts else NULL  
                   }, .res, .universe)

  mapply(function(old_env, new_env) {list2env(as.list(new_env), envir = old_env)}, .env_list[.universe], .res_envs)
  
  # prints the error messages
  lapply(seq_along(.errors), function(i, x) {
    if (is(x[[i]], "try-error"))  {
      warning("error in universe ", i)
      cat("\n", x[[i]], "\n\n")
    }
  }, x = .errors)
  
  # returns the error messages (for multiverse table)
  .error_messages = lapply(.errors, function(x) { ifelse(is(x, "try-error"), x, NA) })

  .m_list = tail(attr(multiverse, "multiverse")$multiverse_diction$as_list(), 1)

  # # save the errors to `list_block_exprs` (current level of m_list)
  # # return the updated `list_block_exprs`
  .diction_res = list(
    mapply(function(x, y) {x$error = y; return(list(x))}, .m_list[[1]], .error_messages)
  )
  names(.diction_res) = names(.m_list)

  m_diction$update(ordered_dict(.diction_res))
  attr(multiverse, "multiverse")$exec_all_until <- 0
}

