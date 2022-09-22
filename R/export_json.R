#' Exporting results from a multiverse analysis to JSON
#'
#' @description export to json
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
#' @importFrom jsonlite write_json
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest_wider
#' @importFrom stats quantile
#' @importFrom distributional cdf

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
  
  .res_df = group_by(x, !!term) %>%
    mutate(limits = map(!!dist, get_limits)) %>%
    unnest_wider(limits) %>%
    mutate(
      cdf.x = map2(.min, .max, ~ seq(.x, .y, length.out = 101)),
      cdf.y = map2(!!dist, cdf.x, ~ unlist(cdf(.x, .y)))
    ) %>%
    select(- !!dist, -.min, -.max)
  
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
  
  c(.min = .lower_limit, .max = .upper_limit)
}