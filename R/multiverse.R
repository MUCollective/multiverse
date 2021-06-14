setClassUnion("listORnumeric", c("list", "numeric"))


Multiverse <- R6Class("Multiverse",
                      public = list(
                        code = NULL,
                        parameters = NULL,
                        conditions = NULL,
                        multiverse_diction = NULL,
                        parameter_set = NULL,
                        unchanged_until = NULL,
                        exec_all_until = NULL,
                        initialize= function() {
                          self$code = NULL
                          self$parameters = list()
                          self$conditions = list()
                          self$multiverse_diction = ordered_dict()
                          self$parameter_set = c()
                          self$unchanged_until = NA
                          self$exec_all_until = 0
                        }
                      )
)

#' Create a new multiverse object
#'
#' Constructs a new multiverse object which enables conducting a multiverse analysis
#'
#' @details To perform a multiverse analysis, the user needs to first constructs a new multiverse object. The user can declare
#' multiple analysis pathways within this multiverse object, and execute these analyses. The multiverse object is
#' contains the following slots:
#' \itemize{
#'   \item code: This slot stores the user's code for conducting a multiverse analysis. The user can
#'   define multiple analysis pathways at each step in the analysis using the `branch` call. It supports tidyverse syntax.
#'
#'   \item parameters: This slot contains a named list of lists. It contains each parameter defined in the
#'   code using the `branch()` function, and the options defined for each parameter (as a list).
#'
#'   \item conditions (NOT IMPLEMENTED): This slot contain a list of conditions: if any of the
#'   parameter values are conditional on a specific value of another parameter, these can be defined in the code
#'   using `branch_assert()` or `branch_exclude()`.
#'
#'   \item current_parameter_assignment: This slot is a list which contains a single option
#'   assigned for each parameter defined in the `code`.
#'
#'   \item multiverse_table: This slot contains a table (in implementation, a tibble which is a
#'   rectangular data structure) where each column of the table will be a unique parameter.
#'   The table will contains every possible combination of options for each parameter — the number of rows
#'   corresponds to the number of different analysis paths. The table also contains, for each row, a list of
#'   option assignments for each parameter (`parameter_assignment` column), code for executing that particular
#'   analysis (of type `expression`), and environments where each code will be executed.
#' }
#' 
#' @param ... currently unused
#'
#' @examples
#' M <- multiverse()
#'
#' @return An empty multiverse object
#'
#' @name multiverse
#' @importFrom rlang env
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom collections ordered_dict
#'
#' @export
multiverse <- function(...) {
  x <- env()
  attr(x, "multiverse_super_env") <- caller_env()
  attr(x, "multiverse") <- Multiverse$new()
  structure(x,
            ...,
            class = c("multiverse")
  )
}

#' Test if the object is a multiverse
#'
#' This function returns `TRUE` for objects of class multiverse,
#' and `FALSE` for all other objects.
#'
#' @rdname multiverse
#' @param x An object
#' @return `TRUE` if the object inherits from the `multiverse` class.
#' @export
is_multiverse <- function(x) {
  .m = attr(x, "multiverse")
  inherits(x, "multiverse") && inherits(.m, "Multiverse")
}

#' @rdname multiverse
#' @return `TRUE` if the object inherits from the `multiverse` class.
#' @export
is.multiverse <- function(x) {
  #signal_soft_deprecated("`is.multiverse()` is soft deprecated, use `is_multiverse()`.")
  .m = attr(x, "multiverse")
  inherits(x, "multiverse") && inherits(.m, "Multiverse")
}

is.r6_multiverse <- function(x) {
  inherits(x, "Multiverse")
}
