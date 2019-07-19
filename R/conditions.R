#' Define conditions for analysis paths in the multiverse
#'
#' Users can specify conditions for which a particular analysis path may or may not be valid
#' using either the `%when%` operator or the `branch_assert` function.
#'
#' @param Logical predicates defined in terms of the parameters and their options in the multiverse.
#'
#' @details A user can specify multiple different analysis options at each step of the analysis
#' process using [branch]. However, it is possible that the values of certain parameters might be
#' conditional on the values of certain other parameters.
#'
#' The conditional or the "implies" relationship between two statements, \emph{A} and \emph{A} has the meaning,
#' "if \emph{A} is true, then \emph{B} is also true." We evaluate this relationship using classical logic:
#' \deqn{  A \implies B is an abbreviation for \neg A \lor B }
#'
#' @examples
#' \dontrun{
#' M <- multiverse()
#' inside(M, {
#'     df <- data.frame (x = 1:10 ) %>%
#'         mutate( y = branch( values_y, TRUE, FALSE )) %>%
#'         mutate( z = branch(values_z,
#'             "constant" ~ 5,
#'             "linear" ~ x + 1,
#'             "sum" ~ (x + y) %when% (values_y == TRUE)
#'         ))
#' })
#' }
#'
#' @name conditions

