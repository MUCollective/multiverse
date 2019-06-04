# Tests for multiverse object

context("multiverse")

M <- multiverse()

test_that("multiverse object is assigned proper class", {
  expect_equal(class(M), "multiverse")
})

test_that("identify multiverse object using 'inherits' function", {
  expect_equal(is_multiverse(M), "multiverse")
  expect_equal(is.multiverse(M), "multiverse")
})

test_that("new multiverse object is initialised properly", {
  expect_null(attr(M, "code"))
  expect_mapequal(attr(M, "current_parameter_assignment"), list())
  expect_mapequal(attr(M, "parameters"), list(parameters = list(), conditions = list()))
})
