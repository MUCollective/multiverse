
context("execute")

values_x = exprs("a", 0, 5, 10, 14)

test_that("auto-executes the default analysis in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  y <- M$x
  expect_equal(y, "a")
})

test_that("`execute_multiverse` executes the all the analyses in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  
  execute_multiverse(M)
  expect_equal(extract_variable_from_universe(M, 1, x), values_x[[1]])
  expect_equal(extract_variable_from_universe(M, 2, x), values_x[[2]])
  expect_equal(extract_variable_from_universe(M, 3, x), values_x[[3]])
  expect_equal(extract_variable_from_universe(M, 4, x), values_x[[4]])
  expect_equal(extract_variable_from_universe(M, 5, x), values_x[[5]])
})
