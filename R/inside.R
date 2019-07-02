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
#' 
#' @export
inside <- function(.m, .code) {
  .code = enexpr(.code)
  multiverse = attr(.m, "multiverse")
  
  add_and_parse_code(multiverse, .code)  
}

`$<-.multiverse` <- function(.m, name, value) {
  .code = expr({ !!sym(name) <- !!rlang::f_rhs(value) })
  multiverse = attr(.m, "multiverse")
  
  add_and_parse_code(multiverse, .code)
  
  .m
}


add_and_parse_code <- function(multiverse, .code) {
  if (is_null(multiverse$code)) {
    .c = .code
  } else {
    .c = multiverse$code %>% 
      inset2(., length(.) + 1, .code[[2]])
  }
  
  multiverse$code <- .c
  parse_multiverse(multiverse)
  execute_default(multiverse)
}




