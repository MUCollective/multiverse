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

#' @export
`%is%` <- function(.var, value) {
  .var = enexpr(.var)
  value = enexpr(value)
  if( !identical(value[[1]], expr(`{`)) ) stop("expressions passed to the multiverse should be encapsulated within `{`")
  
  
  multiverse = eval_tidy( .var[[2]] )
  #stopifnot(M is not a multiverse object)
  #stopifnot(`$` is not used to define a variable. Expressions cannot be assigned directly to the multiverse.)
  
  var = .var[[3]]
  value[[2]] <- value[[2]] %>%
    append(exprs(`<-`, !! var), 0) %>%
    as.call()
  
  inside(multiverse, !!value)
}

#' @export
`$<-.multiverse` <- function(multiverse, .assgnd, value) {
  stop('variables cannot be set within the multiverse using the assignment `<-` operator. Use %is% instead.')
}

