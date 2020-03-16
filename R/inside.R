#' Pass code into the multiverse
#'
#' Add code to the multiverse using the function using a function call, or an assignment operator, which is
#' a wrapper around the function
#'
#' @details To perform a multiverse analysis, we will need to write code to be executed within the multiverse.
#' The `inside()` functions allows us to do this. Use `inside()` to pass any code to the specified multiverse,
#' which is captured as an expression. To define multiple analysis options in the code passed to the multiverse,
#' use the `branch()` function. See [branch] for more
#' details on how to declare multiple analysis options.
#'
#' The `inside` function only stores the code, and does not execute any code at this step. To execute, we
#' provide separate functions. See [execute] for executing the code.
#'
#' Instead of using the `inside()` function, an alternate implementation of the multiverse is using
#' the assignment operator, `<-`. See examples below.
#'
#' @param multiverse A multiverse object. A multiverse object is an S3 object which can be defined using `multiverse()`
#'
#' @param .expr R syntax. All the operations that the user wants to perform within the multiverse can be passed.
#' Since it accepts a single argument, chunks of code can be passed using `{}`. See example for details.
#'
#' @param name,value If the shorthand assignment `$<-` is used to assign values to variables in the multiverse
#' name and value of arguments should be specified. Name indicates the variable name; value indicates the value to be assigned to name.
#'
#' @return a multiverse object
#'
#' @examples
#' \dontrun{
#' M.1 <- multiverse()
#'
#' # using `inside` to declare multiverse code
#' inside(M.1, {
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
#' M.2 <- multiverse()
#'
#' # using the assignment operator to declare multiverse code
#' M$data <- rnorm(100, 50, 20)
#' M$x.mean <- mean(data, trim = branch(
#'     trim_values,
#'     "trim_none" ~ 0,
#'     "trim_1pc" ~ 0.05,
#'     "trim_5pc" ~ 0.025,
#'     "trim_10pc" ~ 0.05
#'   ))
#'
#' # declaring multiple options for a data processing step (calculating a new variable)
#' data(durante)
#' df <- durante
#'
#' inside(M.1, {
#'   df <- df  %>%
#'     mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
#'     mutate( NextMenstrualOnset = branch(menstrual_calculation,
#'                                    "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
#'                                    "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
#'                                    "mc_option3" ~ StartDateNext
#'   ))
#' })
#'
#' }
#'
#' @import rlang
#' @importFrom magrittr %>%
#' @importFrom magrittr inset2
#'
#' @name inside
#' @export
inside <- function(multiverse, .expr, .name = NULL) {
  .expr = enexpr(.expr)
  
  if(!is_call(.expr, "{")) {
    .expr = expr({ !!.expr })
  }
  .expr = eval_seq_in_code(.expr)

  add_and_parse_code(attr(multiverse, "multiverse"), .super_env = attr(multiverse, "multiverse_super_env"), .expr, .name)
}

#' @rdname inside
#' @export
`$<-.multiverse` <- function(multiverse, name, value) {
  # must use call here instead of putting { .. } inside the expr()
  # because otherwise covr::package_coverage() will insert line number stubs
  # *into* the expression and cause tests to break
  if (!is.call(value)) stop(
    "Only objects of type language can be passed into the multiverse. Did you forget to add `~`?"
  )
  
  .expr = call("{", expr( !!sym(name) <- !!rlang::f_rhs(value) ))
  .expr = eval_seq_in_code(.expr)

  add_and_parse_code(attr(multiverse, "multiverse"), .super_env = attr(multiverse, "multiverse_super_env"), .expr)

  multiverse
}

compare_code <- function(x, y) {
  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)
  
  n <- max(length(x), length(y))
  length(x) <- n                      
  length(y) <- n
  
  mapply( function(.x, .y) identical(deparse(.x), deparse(.y)), x, y )
}


add_and_parse_code <- function(m_obj, .super_env, .code, .name = NULL, execute = TRUE) {
  # .loc = match( FALSE, compare_code(m_obj$code, .code) )
  .loc = length(m_obj$code)
  
  if (is_null(m_obj$code)) {
    # .c = .code
    if (is.null(.name)) {
      .c = list(.code) 
    } else {
      .c = list()
      .c[[.name]] = .code
    }
  } else {
    # .c = concatenate_expr(m_obj$code, .code)
    # .c = append(m_obj$code[1:.loc], .code)
    if (is.null(.name)) {
      .c = append(m_obj$code[1:.loc], .code)
    } else {
      .c = m_obj$code
      .c[[.name]] = .code
    }
  }
  
  m_obj$code <- .c
  parse_multiverse(m_obj, .super_env)

  # the execute parameter is useful for parsing tests where we don't want to
  # actually execute anything. probably more for internal use
  if (execute) execute_default(m_obj)
}

concatenate_expr <- function(ref, to_add){
  if (is_call(to_add, "{")) {
    ref = concatenate_expr( ref, as.list(to_add)[-1] )
  } else {
    if(length(to_add) == 1){
      ref = inset2(ref, length(ref) + 1, to_add[[1]])
    } else {
      ref = inset2(ref, length(ref) + 1, to_add[[1]])
      to_add = to_add[-1]
      ref = concatenate_expr(ref, to_add)
    }
  }

  ref
}

eval_seq_in_code <- function(.expr) {
    if (is.call(.expr)) {
      if (is_call(.expr, "branch")) {
        .new_expr = .expr
        if(".options" %in% names(.expr)) {
          .eval_seq = eval(.expr[['.options']])
          .idx = match(c(".options"), names(.expr))
          .new_expr =  magrittr::inset(unname(.expr), c(.idx:((.idx-1) + length(.eval_seq))), .eval_seq)
        }
        return(.new_expr)
      } else {
        as.call(map(.expr, ~ eval_seq_in_code(.x)))
      }
    } else {
      .expr
    }
}




