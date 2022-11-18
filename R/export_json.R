globalVariables(c(".max", ".min", "cdf.x", "cdf.y", "limits", "universe"))

#' Exporting results from a multiverse analysis to JSON
#'
#' @description Exports the results of the multiverse analysis to JSON in a format which is compatible with the multiverse visualisation tool
#' 
#' 
#' @name export_2_json
#' @param x a tidy tibble or data frame which contains summary statistics or distributional information 
#' of each regression coefficient parameter 
#' @param term column in the data frame, x, which contains the name of the coefficients
#' @param mean column in the data frame, x, which contains the mean estimate for each coefficient
#' @param sd column in the data frame, x, which contains the standard error estimate for each coefficient
#' @param dist column in the data frame, x, which contains vectorised distributions---an object of class 
#' `distribution` for each coefficient
#' @param filename filename on disk (as a character string)
#' 
#' @return a data frame or a JSON file
#' 
#' @examples
#' \donttest{
#' library(dplyr)
#' library(tidyr)
#' 
#' M = multiverse()
#'
#' inside(M, {
#'   df = tibble(
#'     x = rnorm(100),
#'     y = x * 0.5 + rnorm(100, 0, 2)
#'  )
#'  
#'  # alternatives to remove outlier
#'  df.filtered = df %>%
#'    filter(
#'      branch(outlier_exclusion,
#'         "2SD" ~ abs(y - mean(y)) > 2*sd(y),
#'         "3SD" ~ abs(y - mean(y)) > 3*sd(y)
#'      )
#'  )
#'   
#'   fit = lm(y ~ x, data = df)
#'   res = broom::tidy(fit)
#' })
#' 
#' execute_multiverse(M)
#' 
#' multiverse::expand(M) %>%
#'   extract_variables(res) %>%
#'   unnest(res) %>%
#'   export_2_json(term, estimate, std.error) 
#' } 
#' 
#'
#' @importFrom jsonlite write_json
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom tidyr unnest_wider
#' @importFrom stats quantile
#' @importFrom distributional cdf
#' @importFrom distributional dist_normal
#' @importFrom purrr map2
#' @importFrom tidyr nest
#' @importFrom tidyselect starts_with
#'
#'
#' @rdname export_json
#' @export
export_2_json = function (x, term, mean, sd, dist, filename) {
  term = enquo(term)
  
  if (missing(dist) & !missing(mean) & !missing(sd)){
    # we have estimate and std.error
    .mu = enquo(mean)
    .sd = enquo(sd)
    dist = quo(dist)
    
    # change to distributional vectors
    .res_df = mutate(x, dist = dist_normal(!!.mu, !!.sd))
   
  } else if (!missing(dist) & missing(mean) & missing(sd)) {
    # we have a distributional object
    dist = enquo(dist)
    
    .res_df = x
    
  } else {
    stop("No complete and/or distinct argument set provided")
  }
  
  export_dist_2_json(.res_df, !!term, !!dist, filename)
}

#' @rdname export_json
#' @export
export_dist_2_json = function(x, term, dist, filename) {
  dist = enquo(dist)
  term = enquo(term)
  
  x = rename(select(rename(x, universe = .universe), -starts_with(".")), .universe = universe)
  
  .res_df = mutate(group_by(x, !!term), limits = map(!!dist, get_limits))
  .res_df = unnest_wider(.res_df, limits)
  
  .res_df = 
    select(
      mutate(.res_df,
        cdf.x = map2(.min, .max, ~ seq(.x, .y, length.out = 101)),
        cdf.y = map2(!!dist, cdf.x, ~ unlist(cdf(.x, .y)))
      ), 
      - !!dist, -.min, -.max
    )
  
  .res_df = nest(.res_df,  results = c(term:cdf.y))
  
  if (!missing(filename)) {
    write_json(
      .res_df, 
      filename, 
      pretty = TRUE
    )
  } else {
    return(.res_df)
  }
}


get_limits = function(dist) {
  .lower_limit = quantile(dist, 0)
  .upper_limit = quantile(dist, 1)
  
  if (! is.finite(.lower_limit)) {
    .lower_limit = quantile(dist, 0.001)
  }
  if (! is.finite(.upper_limit)) {
    .upper_limit = quantile(dist, 0.999)
  }
  
  list(.min = .lower_limit, .max = .upper_limit)
}

#' @rdname export_json
#' @export
export_code_json <- function(multiverse, filename) {
  .expr = attr(M, "multiverse")[["code"]]
  .c = lapply(unname(.expr), function(x) as.list(extract_code_skeleton(x)))
  
  .c = lapply(.c, style_multiverse_code)
  
  .parameters = unlist(lapply(unname(.expr), extract_parameters), recursive = FALSE)
  
  if (!missing(filename)) {
    write_json(
      list(code = unlist(.c), parameters = .parameters),
      filename,
      pretty = TRUE
    )
  } else {
    return(list(code = unlist(.c), parameters = .parameters))
  }
}

extract_code_skeleton = function(.expr) {
  if (!is_missing(.expr)) {
    .expr = rm_branch_assert(.expr)
    
    if (is.call(.expr) ) {
      # Recursive cases
      if (.expr[[1]] == quote(branch)) {
        .expr[1:2]
      } else {
        as.call(lapply(.expr, extract_code_skeleton))
      }
    } else {
      # Base case: constants and symbols
      .expr
    }
  } else {
    .expr
  }
}

extract_parameters = function(.expr) {
  .expr = unname(.expr)
  
  if (!is_missing(.expr)) {
    if (is.call(.expr) ) {
      # Recursive cases
      if (.expr[[1]] == quote(branch)) {
        # .option = list(as.list(lapply(tail(.expr, n = length(.expr) - 2), function(x) deparse(x))))
        .option = lapply(tail(.expr, n = length(.expr) - 2), function(x) style_text(paste0(deparse(x), collapse = "")))
        list_option = list(unlist(.option))
        names(list_option) = quo_text(.expr[[2]])
        
        return(list_option)
      } else {
        ls = unlist(lapply(.expr, extract_parameters), recursive = FALSE)
        if (!is.null(ls)) ls
      }
    }
  }
}