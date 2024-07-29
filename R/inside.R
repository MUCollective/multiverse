#' Pass code into the multiverse
#'
#' Add code to the multiverse using the function using a function call, or an assignment operator, which is
#' a wrapper around the function
#' 
#' The inside function can only access variables which can be accessed at the same environment 
#' where the multiverse object was declared in.
#'
#' @details To perform a multiverse analysis, we will need to write code to be executed within the multiverse.
#' The \code{inside()} functions allows us to do this. Use \code{inside()} to pass any code to the specified multiverse,
#' which is captured as an expression. To define multiple analysis options in the code passed to the multiverse,
#' use the \code{branch()} function. 
#' 
#' @seealso \code{branch()} for more details on how to declare multiple analysis options.
#'
#' The \code{inside} function only stores the code, and does not execute any code at this step. To execute, we
#' provide separate functions. See \code{execute()} for executing the code.
#'
#' Instead of using the \code{inside()} function, an alternate implementation of the multiverse is using
#' the assignment operator, `<-` (please refer to examples below).
#' 
#' \emph{Note}: the \code{inside()} function can only access variables which can be accessed at the same level as the multiverse
#' object. Since \code{inside()} is merely an interface to add analysis to the multiverse object, even if it is being called 
#' by another function, it is actually manipulating the multiverse object, which will have a different parent environment
#' from where \code{inside()} is called, and hence not have access to variables which might be accessible in the environment 
#' within the function from where \code{inside()} is called.
#'
#' @param multiverse A multiverse object. A multiverse object is an S3 object which can be defined using \code{multiverse()}
#'
#' @param .expr R syntax. All the operations that the user wants to perform within the multiverse can be passed.
#' Since it accepts a single argument, chunks of code can be passed using `\{\}`. See example for details.
#' 
#' @param .label It is extracted automatically from the code block of type \code{multiverse}
#' when run in an RMarkdown document. This should be used only within an RMarkdown document. 
#' Defaults to NULL.
#' 
#' @param .execute_default Should the default multiverse be executed as part of this call?
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
#' @importFrom rlang is_call
#' @importFrom rlang enexpr
#' @importFrom rlang is_null
#' @importFrom magrittr inset
#' @importFrom purrr map
#'
#' @name inside
#' @export
inside <- function(multiverse, .expr, .label = NULL, .execute_default = TRUE) {
  .code = enexpr(.expr)
  
  add_and_parse_code(multiverse, .code, .label)
  
  ## execute everything from where things have changed
  
  # direct calls to inside() by the user result in execution of the
  # default universe in the global environment.
  if (.execute_default) {
    invisible( execute_universe(multiverse) )
  }
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
    if (is.null(.name)) {
      .c = list(.expr)
    } # code block is named, it is just appended to a named list
    else {
      .c = list()
      .c[[.name]] = .expr
    }
    .expr <- .c
  } else {
    # there is code already declared in the multiverse
    if (is.null(.name)) .c = append(m_obj$code[1:.loc], .expr)
    else {
      # replaces existing code
      .c = m_obj$code
      .c[[.name]] = .expr
      
      #.expr needs to be changed so that we recompute everything that occurs subsequently
      .expr = .c[which(names(.c) == .name):length(.c)]
      .name = names(.c)[which(names(.c) == .name)]
    }
  }
  
  mapply(parse_multiverse, .expr, names(.expr), MoreArgs = list(.multiverse = multiverse, .code = .c))
  
  m_obj$code <- .c
  
  # case 1: if code is being added to the end, 
  # unchanged_until should be the length of the list
  #
  # case 2: if a previous element, n, in the code list
  #  is being edited, unchanged until should be n - 1
  
  if (tail(names(.c), n = 1) == .name) {
    # case 1
    m_obj$unchanged_until = length(.c) - 1
  } else {
    # case 2:
    # `which` returns the index of the code block which has changed
    # we substract one to indicate what has been unchanged
    .p_idx = which( names(.c) == .name ) - 1
    if (.p_idx == 0) m_obj$unchanged_until = NA
    else m_obj$unchanged_until = .p_idx
  }
  # m_obj$unchanged_until <- length(.c) - length(.name)
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
      as.call(map(as.list(.expr), ~ expand_branch_options(.x)))
    }
  } else {
    .expr
  }
}




