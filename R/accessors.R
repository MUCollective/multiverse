#' Accessing contents of the multiverse object
#'
#' @description A multiverse object contains several \strong{Object variables}. These can be accessed using convenient functions.
#' Variables from the analysis that is being performed within the multiverse can be accessed using the `$`.
#' Object variables such as the `code`, the `expanded parameter options table`, the `parameters` and the `conditions` can be accessed using respective functions

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

#' @export
`$<-.multiverse` <- function(multiverse, name, value) {
  stop(
    "cannot assign to objects in the multiverse. 
    The `$` can only be used to extract objects from the default analysis of the multiverse"
  )
}

#' @rdname accessors
#' @export
expand <- function(multiverse) {
  UseMethod("expand")
}

#' @rdname accessors
#' @export
expand.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `expand`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
expand.multiverse <- function(multiverse) {
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
conditions.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['conditions']]
}

#' @rdname accessors
#' @param idx index of the universe in the multiverse (corresponds to the row in the table)
#' @export
from_universe_i <- function(multiverse, idx, name) {
  name = enquo(name)
  stopifnot( is.multiverse(multiverse) )
  m_tbl = attr(multiverse, "multiverse")[['multiverse_table']]

  m_tbl$.results[[idx]][[quo_text(name)]]
}


