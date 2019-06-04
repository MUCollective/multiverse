context("inside")

M = multiverse()

test_that("inside works on new multiverse object", {
  an_expr = expr({x <- data.frame(x = 1:10)})
  
  inside(M, {
    x = data.frame(x = 1:10)
  })
  
  expect_equal(attr(M, "code"), an_expr)
})

test_that("multiple lines of code can be passed to inside", {
  an_expr = expr({
    x = data.frame(x = 1:10)
    y = data.frame(y = 11:20)
  })
  
  inside(M, {
    x = data.frame(x = 1:10)
  })
  
  inside(M, {
    y = data.frame(y = 11:20)
  })
  
  expect_equal(attr(M, "code"), an_expr)
})

test_that("throws error when object is not of type `multiverse`", {
  M.1 = list(a = 1)
  M.2 = data.frame(a = 1)
  
  expect_error( inside(M.1, {x = data.frame(x = 1:10)}) )
  expect_error( inside(M.2, {x = data.frame(x = 1:10)}) )
})
