#' Pass code into the multiverse
#'
#' Add code to the multiverse using the function using a function call, or an assignment operator, which is
#' a wrapper around the function
#' 
#' The inside function can only access variables which can be accessed at the same environment 
#' where the multiverse object was declared in.
#'
#' @details To perform a multiverse analysis, we will need to write code to be executed within the multiverse.
#' The `inside()` functions allows us to do this. Use `inside()` to pass any code to the specified multiverse,
#' which is captured as an expression. To define multiple analysis options in the code passed to the multiverse,
#' use the `branch()` function. See \code{\link{branch}} for more
#' details on how to declare multiple analysis options.
#'
#' The `inside` function only stores the code, and does not execute any code at this step. To execute, we
#' provide separate functions. See \code{\link{execute}} for executing the code.
#'
#' Instead of using the `inside()` function, an alternate implementation of the multiverse is using
#' the assignment operator, `<-`. See examples below.
#' 
#' **Note:** the `inside()` function can only access variables which can be accessed at the same level as the multiverse
#' object. Since `inside()` is merely an interface to add analysis to the multiverse object, even if it is being called 
#' by another function, it is actually manipulating the multiverse object, which will have a different parent environment
#' from where `inside()` is called, and hence not have access to variables which might be accessible in the environment 
#' within the function from where `inside()` is called.
#'
#' @param multiverse A multiverse object. A multiverse object is an S3 object which can be defined using `multiverse()`
#'
#' @param .expr R syntax. All the operations that the user wants to perform within the multiverse can be passed.
#' Since it accepts a single argument, chunks of code can be passed using `{}`. See example for details.
#' 
#' @param .label It is extracted automatically from the code block of type `multiverse`
#' when run in an RMarkdown document. This should be used only within an RMarkdown document. 
#' Defaults to NULL.
#'
#' @return a multiverse object
#'
#' @examples
#' \donttest{
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
#' inside(M.2, {
#'     data <- rnorm(100, 50, 20)
#' }) 
#' 
#' inside(M.2, {
#'     mean <- mean(data, trim = branch(
#'     trim_values,
#'     "trim_none" ~ 0,
#'     "trim_1pc" ~ 0.05,
#'     "trim_5pc" ~ 0.025,
#'     "trim_10pc" ~ 0.05
#'   ))
#' })
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
inside <- function(multiverse, .expr, .label = NULL) {
  .code = enexpr(.expr)
  
  add_and_parse_code(multiverse, .code, .label)
  
  ## execute everything from where things have changed
  
  # direct calls to inside() by the user result in execution of the
  # default universe in the global environment.
  execute_universe(multiverse)
}


# @param name,value If the shorthand assignment `$<-` is used to assign values to variables 
# in the multiverse name and value of arguments should be specified. Name indicates the variable name; 
# value indicates the value to be assigned to name.

# #' @rdname inside
# #' @export
# `$<-.multiverse` <- function(multiverse, name, value) {
# must use call here instead of putting { .. } inside the expr()
# because otherwise covr::package_coverage() will insert line number stubs
# *into* the expression and cause tests to break
#   if (!is.call(value)) stop(
#     "Only objects of type language can be passed into the multiverse. Did you forget to add `~`?"
#  )

#   .expr = call("{", expr( !!sym(name) <- !!rlang::f_rhs(value) ))
#   .expr = expand_branch_options(.expr)

#   add_and_parse_code(multiverse, .expr)

#   multiverse
# }

compare_code <- function(x, y) {
  if (!is.list(x)) x <- list(x)
  if (!is.list(y)) y <- list(y)
  
  n <- max(length(x), length(y))
  length(x) <- n                      
  length(y) <- n
  
  mapply( function(.x, .y) identical(deparse(.x), deparse(.y)), x, y )
}

add_and_parse_code <- function(multiverse, .expr, .name = NULL) {
  m_obj <- attr(multiverse, "multiverse")
  .super_env <- attr(multiverse, "multiverse_super_env")
  
  # check if .name is NULL
  # if it is NULL auto-generate
  if (is.null(.name)) {
    .name = as.character(length(m_obj$code) + 1)
  }
  
  # ensure that .expr is a single self-contained { ... } block
  if(!is_call(.expr, "{")) {
    .expr = as.call(list(quote(`{`), .expr))
  }
  
  # expand .options arguments in branch calls
  .expr = expand_branch_options(.expr)
  .loc = length(m_obj$code)
  
  # what has been unchanged so far in the tree
  # everything post will be edited in the subsequent steps
  
  # there is no code declared in the multiverse
  if (is_null(m_obj$code)) {
    # code block is not named, it is just appended to a unnamed list -> inside() declaration
    if (is.null(.name)) .c = list(.expr) 
    
    # code block is named, it is just appended to a named list
    else {
      .c = list()
      .c[[.name]] = .expr
    }
    
    .expr <- list(.expr)
  } else {
    # there is code already declared in the multiverse
    if (is.null(.name)) .c = append(m_obj$code[1:.loc], .expr)
    else {
      # replaces existing code
      .c = m_obj$code
      .c[[.name]] = .expr
      
      #.expr needs to be changed so that we recompute everything that occurs subsequently
      .expr = .c[which(names(.c) == .name)]
      .name = names(.c)[which(names(.c) == .name)]
    }
  }
  
  mapply(parse_multiverse, .expr, .name, MoreArgs = list(.multiverse = multiverse, .code = .c))
  
  m_obj$code <- .c
  m_obj$unchanged_until <- length(.c) - length(.name)
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


# expand use of .options argument in branch calls
expand_branch_options <- function(.expr) {
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
      as.call(map(.expr, ~ expand_branch_options(.x)))
    }
  } else {
    .expr
  }
}




