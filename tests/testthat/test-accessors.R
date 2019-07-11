context("accessors")

test_that("basic assignment / retrieval works with `$`", {
  M <- multiverse()
  M$x <- ~ 5
  
  expect_equal(M$x, 5)
})


test_that("basic assignment / retrieval works with `code()`", {
  M <- multiverse()
  M$x <- ~ 5
  
  expect_equal(code(M), expr({ x <- 5 }) )
})

test_that("basic retrieval works with `parameters()`", {
  M <- multiverse()
  M$x <- ~ branch( x_values,
                   0,
                   3,
                   5
                )
  
  expect_equal(parameters(M), list(x_values = list(0, 3, 5)) )
})

test_that("basic retrieval works with `conditions()`", {
  ### placeholder ______________________________________
  ### placeholder ______________________________________
  ### placeholder ______________________________________
  M <- multiverse()
  expect_equal( conditions(M), list() )
})
