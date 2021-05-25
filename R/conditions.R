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
#' \donttest{
#' M <- multiverse()
#' 
#' # There are different ways to specifying conditions
#' # One way is to use the %when% operator
#' # the %when% operator can be specified after the option name
#' inside(M, {
#'     df <- data.frame (x = 1:10 ) %>%
#'         mutate( y = branch( values_y, TRUE, FALSE )) %>%
#'         mutate( z = branch(values_z,
#'             "constant" ~ 5,
#'             "linear" ~ x + 1,
#'             "sum" %when% (values_y == TRUE) ~ (x + y)
#'         ))
#' })
#'
#' # or it can be specified after the expression for computing the option value
#' inside(M, {
#'     df <- data.frame (x = 1:10 ) %>%
#'         mutate( y = branch( values_y, TRUE, FALSE )) %>%
#'         mutate( z = branch(values_z,
#'             "constant" ~ 5,
#'             "linear" ~ x + 1,
#'             "sum" ~ (x + y) %when% (values_y == TRUE)
#'         ))
#' })
#'
#' # an advantage of the '%when' operator is that it can also be used it the
#' # option names are not specified for branches.
#' # when option names are not specified for branches, option names are assigned to
#' # the branches. For character, logical or numeric expressions, option names are of the
#' # same type (i.e. character, logical or numeric expressions respectively)
#' # For expressions of type symbol or call, options names are characters strings
#' # containing the expression.
#' # see the next two examples:
#' inside(M, {
#'  df <- data.frame (x = 1:10 ) %>%
#'    mutate( y = branch( values_y, TRUE, FALSE )) %>%
#'    mutate( z = branch(values_z,
#'           5,
#'           x + 1,
#'           (x + y) %when% (values_y == TRUE)
#'    ))
#' })
#'
#' inside(M, {
#'  df <- data.frame (x = 1:10 ) %>%
#'    filter( branch( values_x,
#'        TRUE,
#'        x > 2 | x < 6
#'    ) %>%
#'    mutate( z = branch(values_z,
#'           5,
#'           x + 1,
#'           (x^2) %when% (values_x == 'x > 2 | x < 6')
#'    ))
#' })
#'
#' # or it can be specified after the expression for computing the option value
#' inside(M, {
#'     df <- data.frame (x = 1:10 ) %>%
#'         mutate( y = branch( values_y, TRUE, FALSE )) %>%
#'         mutate( z = branch(values_z,
#'             "constant" ~ 5,
#'             "linear" ~ x + 1,
#'             "sum" ~ x + y
#'         )) %>%
#'         branch_assert( values_z != "sum" | values_y == TRUE )
#' })
#' }
#'
#' @name conditions

