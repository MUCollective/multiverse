#' @export 
`$.multiverse` <- function(m_obj, value) {
  .idx = 1
  multiverse = attr(m_obj, "multiverse")
  
  multiverse[['multiverse_table']][['.results']][[.idx]][[value]]
}

#' @export
multiverse_table <- function(multiverse) {
  UseMethod("multiverse_table")
}

multiverse_table.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['multiverse_table']]
}

#' @export
code <- function(multiverse) {
  UseMethod("code")
}

code.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['code']]
}

#' @export
parameters <- function(multiverse) {
  UseMethod("parameters")
}

parameters.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['parameters']]
}

#' @export
conditions <- function(multiverse) {
  UseMethod("conditions")
}

conditions <- function(multiverse) {
  attr(multiverse, "multiverse")[['conditions']]
}