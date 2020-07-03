#' Extract variables and objects from the multiverse
#' 
#' This is a wrapper function for extracting one or more variables and objects declared from within the multiverse
#' in a tidy format. 
#' 
#' @details In a typical analysis, the user will declare variables and objects inside the multiverse object. 
#' However, it might be difficult to access the variables and objects, hence we provide convenient wrappers
#' in the form of `extract_variables()`.
#' 
#' If the user wants to extract one or more literals (strings, integers, doubles or logicals of length 1)
#' then each variables is separated out into its own column. If the user wants to extract one or more vectors (or lists)
#' then each such variable will be extracted in its own list column. If the user wants to extract one or more dataframe 
#' then they a column of type data frame (or tibble) would be created (which is a special instance of a list column).
#' 
#' @param .data an data frame object created when declaring the multiverse, and accessed using \code{\link{expand}}
#' See usage.
#' 
#' @param ... one or more variable (or object) names to be extracted from the multiverse object. The names can be quoted or unquoted.
#' 
#' @param .results (Optional) if the .results column which stores the environments for each unique analysis has been changed, 
#' specify the new name of the column. Defaults to `.results`
#' 
#' @importFrom tidyselect vars_pull
#' @importFrom tidyr unnest_wider
#'  
#' @export
extract_variables <- function(.data, ..., .results = .results) {
  results_name = vars_pull(names(.data), {{ .results }})
  
  mutate(.data, extracted = lapply(.results, extract_from_env, ...)
  ) %>%
    unnest_wider("extracted")
}

extract_from_env <- function(.env, ...) {
  pluck <- lapply(enquos(...), as_name)
  y <- lapply(pluck, function(x) .env[[ x ]] )
  names(y) <- pluck
  y
}