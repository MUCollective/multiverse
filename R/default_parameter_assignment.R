#' @export
default_parameter_assignment <- function(multiverse) {
  UseMethod("default_parameter_assignment")
}

default_parameter_assignment.multiverse <- function(multiverse) {
  m_obj = attr(multiverse, "multiverse")
  .idx = m_obj[['default_parameter_assignment']]
  
  m_obj[['multiverse_table']] %>%
    extract2( 'parameter_assignment' ) %>%
    extract2(.idx)
}

default_parameter_assignment.Multiverse <- function(multiverse) {
  .idx = multiverse[['default_parameter_assignment']]
  
  multiverse[['multiverse_table']]  %>%
    extract2( 'parameter_assignment' ) %>%
    extract2(.idx)
}

#' @export
`default_parameter_assignment<-` <- function(multiverse, value) {
  UseMethod("default_parameter_assignment<-")
}

`default_parameter_assignment<-.multiverse` <- function(multiverse, value) {
  stopifnot(is.list(value) || is.numeric(value))
  m_obj = attr(multiverse, "multiverse")
  
  if ( is.list(value) ) {
    .params = m_obj$parameters
    
    if ( is.list(.value) ) {
      stopifnot (length(.value) == length(.params))
      .n = length(.params)
      
      .idx = multiverse$multiverse_table %>%
        select( names(.params) )  %>%
        apply(., 1, function(.x) sum(.x == .value) == .n) %>%
        match(TRUE, .)
    } else {
      .idx = .value
    }
  }
  
  .m[['default_parameter_assignment']] = .idx
  
  multiverse
}