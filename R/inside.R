#' Pass code into the multiverse
#' 
#' The inside function allows you to define objects and code to manipulate the objects inside the multiverse.
#' However, the function only stores the code, and does not execute any code at this step. See `eval_multi` for executing the code.
#' 
#' @param multiverse A multiverse object. A multiverse object is an S3 object which can be defined using `new_multiverse`
#' 
#' @param .expr R syntax. All the operations that the user wants to perform within the multiverse can be passed. 
#' Since it accepts a single argument, chunks of code can be passed using `{}`. See example for details.
#' 
#' @import rlang
#' @importFrom magrittr inset2
#' 
#' @export
inside <- function (multiverse, .expr) {
  stopifnot(is_multiverse(multiverse))
  .expr <- enexpr(.expr)
  
  if (is_null(attr(multiverse, "code"))) {
    attr(multiverse, "code") <- .expr
  } else {
    attr(multiverse, "code") <- attr(multiverse, "code") %>% 
      inset2(., length(.) + 1, .expr[[2]])
  }
}