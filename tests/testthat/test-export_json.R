library(dplyr)
library(tidyr)
library(distributional)

test_that("export_2_json works", {
  mu = rnorm(1)
  sd = rexp(1)
  limits = get_limits(dist_normal(mu, sd))
  
  df = tibble(
    .universe = 1,
    term = "Intercept",
    mean = mu,
    se = sd
  )
  
  ref.df = df %>%
    mutate( 
      cdf.x = list(seq(limits[[1]], limits[[2]], length.out = 101)),
      cdf.y = cdf(dist_normal(mu, sd), unlist(cdf.x)) 
    )
  
  test.df = df %>%
    export_2_json(term, mean, se) %>%
    unnest(results)
  
  expect_equal(as.list(ref.df), as.list(test.df))
})

test_that("export_dist_2_json works", {
  mu = rnorm(1)
  sd = rexp(1)
  limits = get_limits(dist_normal(mu, sd))
  
  df = tibble(
    .universe = 1,
    term = "Intercept",
    dist = dist_normal(mu, sd)
  )
  
  ref.df = df %>%
    select(-dist) %>%
    mutate( 
      cdf.x = list(seq(limits[[1]], limits[[2]], length.out = 101)),
      cdf.y = cdf(dist_normal(mu, sd), unlist(cdf.x)) 
    )
  
  test.df = df %>%
    export_dist_2_json(term, dist) %>%
    unnest(results)
  
  expect_equal(as.list(ref.df), as.list(test.df))
})

test_that("get_limits works", {
  d1 = dist_exponential(5)
  expect_equal(get_limits(d1), list(.min = quantile(d1, 0), .max = quantile(d1, 0.999)))
  
  d2 = dist_normal(3, 5)
  expect_equal(get_limits(d2), list(.min = quantile(d2, 0.001), .max = quantile(d2, 0.999)))
  
  d3 = dist_beta(2, 2)
  expect_equal(get_limits(d3), list(.min = quantile(d3, 0), .max = quantile(d3, 1)))
  
  d4 = dist_student_t(3, 0, 5)
  expect_equal(get_limits(d4), list(.min = quantile(d4, 0.001), .max = quantile(d4, 0.999)))
})
