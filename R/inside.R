#' Pass code into the multiverse
#' 
#' Add code to the multiverse using the function using a function call, or an assignment operator, which is
#' a wrapper around the function
#' 
#' @details To perform a multiverse analysis, we will need to write code to be executed within the multiverse. 
#' The `inside()` functions allows us to do this. Use `inside()` to pass any code to the specified multiverse,
#' which is captured as an expression. To define multiple analysis options in the code passed to the multiverse, 
#' use the \code{\link[multidy]{branch}} function. See \code{\link[multidy]{branch}} for more 
#' details on how to declare multiple analysis options. 
#' 
#' The `inside` function only stores the code, and does not execute any code at this step. To execute, we 
#' provide separate functions. See \code{\link[multidy]{execute}} for executing the code.
#' 
#' Instead of using the `inside()` function, an alternate implementation of the multiverse is using 
#' the assignment operator, `<-`. See examples below.
#' 
#' @param multiverse A multiverse object. A multiverse object is an S3 object which can be defined using `new("multiverse")`
#' 
#' @param .expr R syntax. All the operations that the user wants to perform within the multiverse can be passed. 
#' Since it accepts a single argument, chunks of code can be passed using `{}`. See example for details.
#' 
#' @return a multiverse object
#' 
#' #' @examples 
#' \dontrun{
#' M.1 <- new("multiverse")
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
#' M.2 <- new("multiverse)
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
#' data(fertility)
#' data.fertility <- fertility
#' 
#' inside(M.1, {   
#'   df <- data.fertility  %>% 
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
#' @importFrom pryr where
#' 
#' @export
# perhas not the best way of writing modify-in-place functions
inside <- function(multiverse, .code, parent = NULL) {
  .code = enexpr(.code)
  .m_name = quo_name(enquo(multiverse))
  
  # get the frame of the environment where the inside function is called
  # this is where the multiverse object will be located
  # but, inside2 might be also called by the `$<-` operator, thus we might pass it directly
  if (is.null(parent))  parent <- parent.frame()
  
  if (is_null(multiverse@code)) {
    .c = .code
  } else {
    .c = multiverse@code %>% 
      inset2(., length(.) + 1, .code[[2]])
  }
  
  multiverse@code <- .c
  multiverse = multiverse %>%
    parse_multiverse()
  
  assign(.m_name, multiverse, envir = parent)
}

inside2 <- function(multiverse, .code, parent = NULL) {
  # get the frame of the environment where the inside function is called
  # this is where the multiverse object will be located
  # but, inside2 might be also called by the `$<-` operator, thus we might pass it directly
  if (is.null(parent))  parent <- parent.frame()
  
  .m_name = enexpr(multiverse)
  .code = enexpr(.code)
  
  if (is_null(multiverse@code)) {
    .c = .code
  } else {
    .c = multiverse@code %>% 
      inset2(., length(.) + 1, .code[[2]])
  }
  
  eval( call( "<-", call("@", .m_name, "code"), expr(expr(!!.c)) ) , parent )
  parse_multiverse( eval(.m_name, parent), .m_name, parent )
}

`$<-.multiverse` <- function(multiverse, name, value) {
  .m_name = match.call()
  
  print(.m_name)
  #inside( .multiverse_obj, { !!sym(name) <- !!f_rhs(value) } )
}

