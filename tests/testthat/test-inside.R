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
  an_expr = expr({x <- data.frame(x = 1:10)})

  M = multiverse()
  M$x <- ~ data.frame(x = 1:10)

  expect_equal(f_rhs( code(M) ), f_rhs(an_expr))
})

test_that("multiple lines of code can be passed to inside", {
  an_expr = expr({
    x <- data.frame(x = 1:10)
    y <- data.frame(y = 11:20)
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
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), an_expr)

  expect_true( is.language( f_rhs( code(M) ) ))
})

test_that("`add_and_parse_code` parses the code", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })

  M = multiverse()
  M.R6 = attr(M, "multiverse")
  add_and_parse_code(M.R6, attr(M, "multiverse_super_env"), an_expr)

  expect_equal( dim(M.R6$multiverse_table), c(4, 5) )
  expect_equal( length(M.R6$parameters), 1 )
  expect_equal( length(M.R6$parameters$value_y), 4 )
})

test_that("`add_and_parse_code` executes the default analysis", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })

  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), an_expr)

  df = attr(M, "multiverse")$multiverse_table$.results[[1]]$x
  df.ref =  data.frame(x = 1:10) %>%  mutate( y = 0 )

  expect_equal( as.list(df), as.list(df.ref) )
})

test_that("continuous parameters defined in the multiverse are evaluated", {
  .expr_1 = expr({
    y <- branch(foo, "option1" ~ 1, .options = 2:10)
  }) %>% eval_seq_in_code()

  .expr_2 = expr({
    y <- branch(foo, "option1" ~ 1, .options = seq(2, 3, by = 0.1))
  }) %>% eval_seq_in_code()

  .ref_expr_1 = expr({
    y <- branch(foo, "option1" ~ 1, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)
  })

  .ref_expr_2 = expr({
    y <- branch(foo, "option1" ~ 1, 2, 2.1, 2.2, 2.3, 2.4, 2.5,
                2.6, 2.7, 2.8, 2.9, 3)
  })

  expect_equal(.expr_1, .ref_expr_1)
  expect_equal(.expr_2, .ref_expr_2)
})

