#' Create a new multiverse object
#' 
#' multiverse() constructs a new multiverse object. The multiverse object is an S3 object wtih three attributes:
#' \itemize{
#'   \item code: The `code` attribute stores the user's code for conducting a multiverse analysis. The user can
#'   define multiple analysis pathways at each step in the analysis using the `branch` call. It supports tidyverse syntax.
#'   \item parameters: The `parameters` attribute contain the list which itself comprises of two lists: 
#'   the list of parameters and the list of conditions. The list of parameters is a named list which defines all the values 
#'   that each defined parameter can take. The list of conditions defines, if any of the parameter values are conditional
#'    on a specific value of another parameter, the condition. This is created from the `code` attribute.
#'   \item current_parameter_assignment: The `current_parameter_assignment` attribute is a list which contains a single value
#'   assignment for each parameter defined in the `code`.
#' }
#' 
#' @examples 
#' \dontrun{
#' M <- new_multiverse()
#' }
#' 
#' @return An empty multiverse object
#' 
#' @importFrom rlang env
#' @export

multiverse <- function () {
  x <- env()
  attr(x, "code") <- NULL
  attr(x, "parameters") <- list(parameters = list(), conditions = list())
  attr(x, "current_parameter_assignment") <- list()
  
  class(x) <- "multiverse"
  x
}

#' Test if the object is a multiverse
#'
#' This function returns `TRUE` for objects of class multiverse,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#' @return `TRUE` if the object inherits from the `multiverse` class.
#' @export
is_multiverse <- function(x) {
  inherits(x, "multiverse")
}

#' @export
is.multiverse <- function(x) {
  #signal_soft_deprecated("`is.multiverse()` is soft deprecated, use `is_multiverse()`.")
  inherits(x, "multiverse")
}

#' @export
`$.multiverse` <- function(multiverse, variable.name) {
  attr(M, variable.name)
}

