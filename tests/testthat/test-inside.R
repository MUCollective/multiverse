# Tests to verify if expressions can be passed to the multiverse
# and if passed expressions are stored properly
context("inside")

library(rlang)

test_that("inside works on new multiverse object", {
  an_expr = expr({x = data.frame(x = 1:10)})
  
  M = new("multiverse")
  
  M = inside(M, {
    x = data.frame(x = 1:10)
  })
  
  expect_equal(f_rhs( M[["code"]] ), f_rhs(an_expr))
})

test_that("multiple lines of code can be passed to inside", {
  an_expr = expr({
    x = data.frame(x = 1:10)
    y = data.frame(y = 11:20)
  })
  
  M = new("multiverse")
  M = inside(M, {
    x = data.frame(x = 1:10)
  })
  
  M = inside(M, {
    y = data.frame(y = 11:20)
  })
  
  expect_equal(f_rhs( M[["code"]] ), f_rhs(an_expr))
})

test_that("throws error when object is not of type `multiverse`", {
  M.1 = list(a = 1)
  M.2 = data.frame(a = 1)
  
  expect_error( inside(M.1, {x = data.frame(x = 1:10)}) )
  expect_error( inside(M.2, {x = data.frame(x = 1:10)}) )
})

