# Tests to verify if expressions can be passed to the multiverse
# and if passed expressions are stored properly
context("inside")

library(rlang)
library(dplyr)

test_that("inside works on new multiverse object", {
  an_expr = expr({x = data.frame(x = 1:10)})
  
  M = multiverse()
  inside(M, {
    x = data.frame(x = 1:10)
  })
  
  expect_equal(f_rhs( code(M) ), f_rhs(an_expr))
})

test_that("multiple lines of code can be passed to inside", {
  an_expr = expr({
    x = data.frame(x = 1:10)
    y = data.frame(y = 11:20)
  })
  
  M = multiverse()
  inside(M, {
    x = data.frame(x = 1:10)
  })
  
  inside(M, {
    y = data.frame(y = 11:20)
  })
  
  expect_equal(f_rhs( code(M) ), f_rhs(an_expr))
})

test_that("throws error when object is not of type `multiverse`", {
  M.1 = list(a = 1)
  M.2 = data.frame(a = 1)
  
  expect_error( inside(M.1, {x = data.frame(x = 1:10)}) )
  expect_error( inside(M.2, {x = data.frame(x = 1:10)}) )
})

# `$<-.multiverse` // multiverse shorthand assignment ___________________________
test_that("`$<-.multiverse` works on new multiverse object", {
  an_expr = expr({x = data.frame(x = 1:10)})
  
  M = multiverse()
  M$x <- ~ data.frame(x = 1:10)
  
  expect_equal(f_rhs( code(M) ), f_rhs(an_expr))
})

test_that("multiple lines of code can be passed to inside", {
  an_expr = expr({
    x = data.frame(x = 1:10)
    y = data.frame(y = 11:20)
  })
  
  M = multiverse()
  M$x <- ~ data.frame(x = 1:10)
  M$y <- ~ data.frame(y = 11:20)
  
  expect_equal(f_rhs( code(M) ), f_rhs(an_expr))
})

# add_and_parse_code ___________________________
test_that("`add_and_parse_code` stores code as `language`", {
  an_expr = expr({
    x = data.frame(x = 1:10)
    y = data.frame(y = 11:20)
  })
  
  M = multiverse()
  M.R6 = attr(M, "multiverse")
  
  add_and_parse_code(M.R6, an_expr)
  
  expect_true( is.language( f_rhs( code(M) ) ))
})

test_that("`add_and_parse_code` parses the code", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })
  
  M = multiverse()
  M.R6 = attr(M, "multiverse")
  
  add_and_parse_code(M.R6, an_expr)
  
  expect_equal( dim(M.R6$multiverse_table), c(4, 4) )
  expect_equal( length(M.R6$parameters), 4 )
})

test_that("`add_and_parse_code` executes the default analysis", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })
  
  M = multiverse()
  M.R6 = attr(M, "multiverse")
  
  add_and_parse_code(M.R6, an_expr)
  
  df = M.R6$multiverse_table$.results[[1]]$x
  df.ref =  data.frame(x = 1:10) %>%  mutate( y = 0 )
  
  expect_equal( as.list(df), as.list(df.ref) )
})

