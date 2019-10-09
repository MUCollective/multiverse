#' Retrieve or change the default analysis of the multiverse
#'
#' @description Functions which allow the user to retrieve or update the default assignments for
#' each parameter defined in the multiverse. Retrieving the default analysis allows the users
#' to see what the default analysis would look like (what values each parameter would take). Updating
#' the default analysis allows the users to specify a different set of parameter assignments which will
#' be executed.
#'
#' @param multiverse The multiverse object
#'
#' @param value a named list or integer. The named list should consist of all the \emph{parameters}, and an \emph{option name} assigned to each.
#' The integer indicates the corresponding position in the multiverse table which will be used as the default assignment.
#'
#' @examples
#' \dontrun{
#' M <- multiverse()
#' inside(M, {
#'   df <- data.frame( x = sample(1:100, 50, replace = TRUE)
#'
#'   df <- df %>%
#'    mutate( y = branch(
#'     value_y,
#'     "zero" ~ 0,
#'     "three" ~ 3,
#'     "x + 1" ~ x + 1,
#'     "x-squared" ~ x^2
#'   ))
#' })
#'
#' default_parameter_assignment(M)
#' # the code above will return a list: value_y = zero
#'
#' default_parameter_assignment(M) <- 3
#' default_parameter_assignment(M) <- list(value_y = "x + 1")
#' # both of these will change the default parameter assignment to "x + 1"
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr extract2
#' @importFrom dplyr select
#'
#' @name default_parameter_assignment
#' @export
default_parameter_assignment <- function(multiverse) {
  UseMethod("default_parameter_assignment")
}

#' @rdname default_parameter_assignment
#' @export
default_parameter_assignment.multiverse <- function(multiverse) {
  m_obj = attr(multiverse, "multiverse")
  .idx = m_obj[['default_parameter_assignment']]

  m_obj[['multiverse_table']] %>%
    extract2( '.parameter_assignment' ) %>%
    extract2(.idx)
}

#' @rdname default_parameter_assignment
#' @export
default_parameter_assignment.Multiverse <- function(multiverse) {
  .idx = multiverse[['default_parameter_assignment']]

  multiverse[['multiverse_table']]  %>%
    extract2( '.parameter_assignment' ) %>%
    extract2(.idx)
}

#' @rdname default_parameter_assignment
#' @export
`default_parameter_assignment<-` <- function(multiverse, value) {
  UseMethod("default_parameter_assignment<-")
}

#' @rdname default_parameter_assignment
#' @export
`default_parameter_assignment<-.multiverse` <- function(multiverse, value) {
  stopifnot(is.list(value) || is.numeric(value))
  m_obj = attr(multiverse, "multiverse")

  if ( is.list(value) ) {
    .params = m_obj$parameters

    stopifnot (length(value) == length(.params))
    .n = length(.params)

    .idx = m_obj$multiverse_table %>%
      select( names(.params) )  %>%
      apply(., 1, function(.x) {
        sum(unlist(.x) == unlist(value)) == .n
      }) %>%
      match(TRUE, .)
  } else {
    .idx = value
  }

  m_obj[['default_parameter_assignment']] = .idx

  multiverse
}
