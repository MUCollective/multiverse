globalVariables(c(".max", ".min", "cdf.x", "cdf.y", "limits", "universe", "values"))

#' Exporting results from a multiverse analysis to JSON
#'
#' @description Exports the results of the multiverse analysis to JSON in a format which is compatible with the multiverse visualisation tool
#' 
#' 
#' @name export_json
#' @param x a tidy tibble or data frame which contains summary statistics or distributional information 
#' of each regression coefficient parameter 
#' @param term column in the data frame, x, which contains the name of the coefficients
#' @param mean column in the data frame, x, which contains the mean estimate for each coefficient
#' @param sd column in the data frame, x, which contains the standard error estimate for each coefficient
#' @param dist column in the data frame, x, which contains vectorised distributions---an object of class 
#' `distribution` for each coefficient
#' @param filename filename on disk (as a character string)
#' 
#' @return a JSON file or (if a filepath is not specified) a dataframe for the results file and a list for the code file
#' 
#' @details
#' ## results JSON file schema
#' It consists of a list of objects (where each object corresponds to one analysis in the multiverse). 
#' Within this object, the results attribute contains a(nother) list of objects corresponding to each outcome variable. 
#' For e.g., here we have four coefficients (see the results of the regression model), and thus the results attribute will contain four objects. 
#' Each object has the following attributes:
#' - `term`: name of the outcome variable
#' - `estimate`: mean / median point estimate i.e., \eqn{\mathbb{E}(\mu)} for any parameter \eqn{\mu}.
#' - `std.error`: standard error for the point estimate i.e., \eqn{\sqrt{\text{var}(\mu)}}
#' - `cdf.x`: a list of quantiles
#' - `cdf.y`: a list of cumulative probability density estimates corresponding to the quantiles
#' 
#' In addition, it also contains the following attributes, but these are not currently used by Milliways:
#' - `statistic`
#' - `p.value`
#' - `conf.low`
#' - `conf.high`
#' 
#' 
#' ## code JSON file schema
#' It consists of two attributes: `code` and `parameters`. 
#' `code` is a list of strings consisting of the R and multiverse syntax used to implement the analysis. For readability, we
#' use [styler] to break up the declared code.
#' `parameters` is an object listing the parameter names and the corresponding options for each of the parameters declared in the analysis.
#' 
#' ## data JSON file schema
#' It consists of a list of objects, each with two attributes: `field` and `values`. 
#' `field` is the name of a column corresponding to a variable in the dataset. 
#' `values` are a list of values for that variable in the dataset.
#' 
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
#'   export_results_json(term, estimate, std.error) 
#' } 
#' 
#'
#' @importFrom jsonlite write_json
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_text
#' @importFrom rlang is_missing
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @importFrom dplyr summarise_all
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr nest
#' @importFrom readr guess_parser
#' @importFrom stats quantile
#' @importFrom distributional cdf
#' @importFrom distributional dist_normal
#' @importFrom purrr map2
#' @importFrom tidyselect starts_with
#'
#' @rdname export_json
#' @export
export_results_json = function (x, term, mean, sd, dist, filename) {
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
  
  export_results_dist_json(.res_df, !!term, !!dist, filename)
}

#' @rdname export_json
#' @export
export_results_dist_json = function(x, term, dist, filename) {
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
      -.min, -.max
    )
  
  .res_df = nest(.res_df,  results = c(term:cdf.y))
  
  if (!missing(filename)) {
    write_json(
      select(.res_df, -!!dist), 
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
export_code_json <- function(x, filename) {
  .expr = attr(x, "multiverse")[["code"]]
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


#' @rdname export_json
#' @export
export_data_json <- function(x, filename) {
  result = summarise_all(x, ~ list(as.character(.)))  |> 
    pivot_longer(cols = everything(), names_to = "field", values_to = "values")  |> 
    mutate(field_type = map_chr(values, guess_parser))
  
  if (!missing(filename)) {
    write_json(
      result,
      filename,
      pretty = TRUE
    )
  } else {
    return(result)
  }
}