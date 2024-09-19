library(distributional)

test_that("export_results_json creates correct CDF for estimate + std.error and distributions", {
  N = 50
  
  dist1 = dist_normal()
  dist2 = dist_normal(2, 5)
  
  df = tibble(
    .universe = as.integer(1),
    term = c('x', 'y'),
    estimate = c(0, 2),
    std.error = c(1, 5),
    distribution = c(dist1, dist2)
  )
  
  
  seq1 = seq(quantile(dist1, 0.001), quantile(dist1, 0.999), length.out = 101)
  seq2 = seq(quantile(dist2, 0.001), quantile(dist2, 0.999), length.out = 101)
  
  df.ref = tibble(
    .universe = as.integer(1),
    term = c("x", "y"),
    estimate = c(0, 2),
    std.error = c(1, 5),
    cdf.x = list(seq1, seq2),
    cdf.y = c( cdf(dist1, seq1), cdf(dist2, seq2))
  )
  
  df1 = df %>%
    export_results_json(term, dist = distribution) %>%
    unnest(results) %>%
    select(-distribution)
  
  df2 = df %>%
    select(-distribution) %>%
    export_results_json(term, estimate, std.error) %>%
    unnest(results) %>%
    select(-dist)
  
  expect_equal(df.ref, df1)
  expect_equal(df.ref, df2)
})

test_that("export_results_dist_json creates correct CDF for distributions", {
  N = 50
  
  dist1 = dist_normal()
  dist2 = dist_normal(2, 5)
  
  df = tibble(
    .universe = as.integer(1),
    term = c('x', 'y'),
    estimate = c(0, 2),
    std.error = c(1, 5),
    distribution = c(dist1, dist2)
  )
  
  seq1 = seq(quantile(dist1, 0.001), quantile(dist1, 0.999), length.out = 101)
  seq2 = seq(quantile(dist2, 0.001), quantile(dist2, 0.999), length.out = 101)
  
  df.ref = tibble(
    .universe = as.integer(1),
    term = c("x", "y"),
    estimate = c(0, 2),
    std.error = c(1, 5),
    cdf.x = list(seq1, seq2),
    cdf.y = c( cdf(dist1, seq1), cdf(dist2, seq2))
  )
  
  df1 = df %>%
    export_results_dist_json(term, distribution) %>%
    unnest(results) %>%
    select(-distribution)
  
  expect_equal(df.ref, df1)
})


test_that("get_limits esimtates correct finite limits non-normal distributions", {
  expect_equal(get_limits(dist_gamma(1, 1))$.min, 0) # lower limit of gamma dist is 0
  expect_equal(get_limits(dist_gamma(2, 5))$.min, 0) # lower limit of gamma dist is 0
  expect_equal(get_limits(dist_beta(2, 2)), list(.min = 0, .max = 1)) # limits of beta dist are [0, 1]
  expect_equal(get_limits(dist_exponential(2))[[1]], 0) # lower limit of exponential dist is 0
})

test_that("export_results_dist_json creates correct CDF for non-normal distributions", {
  N = 50
  
  dist1 = dist_gamma(2, 2)
  dist2 = dist_beta(2, 2)
  dist3 = dist_exponential(4)
  
  df = tibble(
    .universe = as.integer(1),
    term = c('x', 'y', 'z'),
    distribution = c(dist1, dist2, dist3)
  )
  
  seq1 = seq(0, quantile(dist1, 0.999), length.out = 101)
  seq2 = seq(0, 1, length.out = 101)
  seq3 = seq(0, quantile(dist3, 0.999), length.out = 101)
  
  df.ref = tibble(
    .universe = as.integer(1),
    term = c("x", "y", "z"),
    cdf.x = list(seq1, seq2, seq3),
    cdf.y = c( cdf(dist1, seq1), cdf(dist2, seq2), cdf(dist3, seq3))
  )
  
  df1 = df %>%
    export_results_dist_json(term, distribution) %>%
    unnest(results) %>%
    select(-distribution)
  
  expect_equal(df.ref, df1)
})


test_that("export_results_json creates correct CDF for estimate + std.error and distributions from output of multiverse `expand`", {
  N = 50
  M = multiverse()
  
  dist1 = dist_normal()
  dist2 = dist_normal(2, 5)
  
  inside(M, {
    df = tibble(
      term = 'x',
      distribution = branch(distributions, dist1, dist2)
    )
  })
  
  execute_multiverse(M)
  
  seq1 = seq(quantile(dist1, 0.001), quantile(dist1, 0.999), length.out = 101)
  seq2 = seq(quantile(dist2, 0.001), quantile(dist2, 0.999), length.out = 101)
  
  df.ref = tibble(
    .universe = as.integer(c(1, 2)),
    distributions = c("dist1", "dist2"),
    term = "x",
    cdf.x = list(seq1, seq2),
    cdf.y = c( cdf(dist1, seq1), cdf(dist2, seq2))
  )
  
  df1 = expand(M) %>%
    extract_variables(df) %>%
    unnest(df) %>%
    export_results_json(term, dist = distribution) %>%
    unnest(results) %>%
    select(-distribution)
  
  expect_equal(df.ref, df1)
})