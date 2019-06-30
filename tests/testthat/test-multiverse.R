# Tests for multiverse object
context("multiverse")

library(methods)

M = new("multiverse")

test_that("multiverse object is assigned proper class", {
  expect_true(isClass(M, "Multiverse"))
})

test_that("identify multiverse object using 'inherits' function", {
  expect_equal(is_multiverse(M), TRUE)
  expect_equal(is.multiverse(M), TRUE)
})

test_that("new multiverse object is initialised properly", {
  expect_null(M@code)
  expect_warning( expect_mapequal( M[['current_parameter_assignment']], list()) )
  expect_warning( expect_mapequal( M[['parameters']], list()) )
  expect_warning( expect_mapequal( M[['conditions']], list()) )
})
