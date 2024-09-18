globalVariables(c("extracted", "extracted_id"))
#' Extract variables and objects from the multiverse
#' 
#' This is a wrapper function for extracting one or more variables and objects declared from within the multiverse
#' in a tidy format. 
#' 
#' @details In a typical analysis, the user will declare variables and objects inside the multiverse object. 
#' However, it might be difficult to access the variables and objects, hence we provide convenient wrappers
#' in the form of \code{extract_variables()}.
#' 
#' If the user wants to extract one or more literals (strings, integers, doubles or logicals of length 1)
#' then each variables is separated out into its own column. If the user wants to extract one or more vectors (or lists)
#' then each such variable will be extracted in its own list column. If the user wants to extract one or more dataframe 
#' then they a column of type data frame (or tibble) would be created (which is a special instance of a list column).
#' 
#' @param x either a multiverse object or a dataframe (created using \code{expand()}) from a multiverse object.
#' See usage.
#' 
#' @param ... one or more variable (or object) names to be extracted from the multiverse object. The names can be quoted or unquoted.
#' 
#' @param .results (Optional) if the .results column which stores the environments for each unique analysis has been changed, 
#' specify the new name of the column. Defaults to \code{.results}
#' 
#' @examples
#' \donttest{
#' library(dplyr)
#' 
#' M <- multiverse()
#' 
#' inside(M, {
#'   data <- rnorm(100, 50, 20)
#'
#'   x.mean <- mean(data, trim = branch(
#'     trim_values,
#'     "trim_none" ~ 0,
#'     "trim_1pc" ~ 0.05,
#'     "trim_5pc" ~ 0.025,
#'     "trim_10pc" ~ 0.05
#'   ))
#'   
#'   y <- sd(data)
#' })
#'
#' # Extracts the relevant variable from the multiverse
#' M %>%
#'   extract_variables(x.mean)
#' 
#' # if you want to filter the multiverse before extracting variables from it
#' # you ca first create the table and manipulate it before extracting variables
#' expand(M) %>%
#'   extract_variables(x.mean)
#'
#' # you can extract more than one variable from the multiverse simultaneously
#' # these variables will be new columns in the dataset
#' expand(M) %>%
#'   extract_variables(x.mean, y)
#' }
#' 
#' @importFrom tidyr unnest_longer
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr mutate
#' @importFrom rlang enquo
#' @importFrom rlang enquos
#' @importFrom rlang as_name
#'
#' @export
extract_variables <- function(x, ..., .results = .results) {
  UseMethod("extract_variables")
}

#' @rdname extract_variables
#' @export
extract_variables.multiverse <- function(x, ..., .results = .results) {
  expand(x) |> 
    extract_variables(..., .results = .results)
}

#' @rdname extract_variables
#' @export
extract_variables.data.frame <- function(x, ..., .results = .results) {
  .results <- enquo(.results)
  
  mutate(
    x, extracted = lapply(!!.results, extract_from_env, ...)
  ) |> 
  unnest_longer(extracted) |> 
  pivot_wider(names_from = extracted_id, values_from = extracted)
}

extract_from_env <- function(.env, ...) {
  .to_pluck <- lapply(enquos(...), as_name)
  
  y <- lapply(.to_pluck, function(x) {
    e <- tryCatch(get(as.character(x), .env), error = function(e) e)
    if (is(e, "error")) NA else e
  })
  names(y) <- .to_pluck
  y
}