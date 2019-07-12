#' Accessing contents of the multiverse object
#' 
#' @description A multiverse object contains several \strong{Object variables}. These can be accessed using convenient functions.
#' Variables from the analysis that is being performed within the multiverse can be accessed using the `$`.
#' Object variables such as the `code`, the `multiverse table`, the `parameters` and the `conditions` can be accessed using respective functions

#' @name accessors
#' @param multiverse Object of class multiverse
#' @param name a variable name
#'
#' @export 
`$.multiverse` <- function(multiverse, name) {
  .idx = 1
  m_obj = attr(multiverse, "multiverse")
  m_obj[['multiverse_table']][['.results']][[.idx]][[name]]
}

#' @rdname accessors
#' @export
multiverse_table <- function(multiverse) {
  UseMethod("multiverse_table")
}

#' @rdname accessors
#' @export
multiverse_table.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `multiverse_table`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
multiverse_table.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['multiverse_table']]
}

#' @rdname accessors
#' @export
code <- function(multiverse) {
  UseMethod("code")
}

#' @rdname accessors
#' @export
code.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `code`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
code.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['code']]
}

#' @rdname accessors
#' @export
parameters <- function(multiverse) {
  UseMethod("parameters")
}

#' @rdname accessors
#' @export
parameters.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `parameters`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
parameters.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['parameters']]
}

#' @rdname accessors
#' @export
conditions <- function(multiverse) {
  UseMethod("conditions")
}

#' @rdname accessors
#' @export
conditions.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `conditions`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
conditions <- function(multiverse) {
  attr(multiverse, "multiverse")[['conditions']]
}
