# Tests to verify if expressions can be passed to the multiverse
# and if passed expressions are stored properly
context("inside")

library(rlang)
library(purrr)
library(dplyr)

test_that("inside works on new multiverse object", {
  an_expr = list( `1` = expr({x = data.frame(x = 1:10)}) )

  M = multiverse()
  inside(M, {
    x = data.frame(x = 1:10)
  })

  expect_equal( code(M), an_expr )
})

test_that("multiple lines of code can be passed to inside", {
  some_exprs = list(
    `1` = quote({  x = data.frame(x = 1:10) }),
    `2` = quote({  y = data.frame(y = 11:20)  })
  )

  M = multiverse()
  inside(M, {
    x = data.frame(x = 1:10)
  })

  inside(M, {
    y = data.frame(y = 11:20)
  })

  expect_equal( code(M), some_exprs)
})

test_that("multiple lines of code can be passed to inside in a single block", {
  some_exprs = list(
    `1` = quote({
      x <- data.frame(x = 1:10)
      y <- data.frame(y = 11:20)
    })
  )
  
  M = multiverse()
  inside(M, {
    x <- data.frame(x = 1:10)
    y <- data.frame(y = 11:20)
  })
  
  expect_equal( code(M), some_exprs)
})

test_that("throws error when object is not of type `multiverse`", {
  M.1 = list(a = 1)
  M.2 = data.frame(a = 1)

  expect_error( inside(M.1, {x = data.frame(x = 1:10)}) )
  expect_error( inside(M.2, {x = data.frame(x = 1:10)}) )
})

# add_and_parse_code ___________________________
test_that("`add_and_parse_code` stores code as a list of `language`", {
  expr.1 = expr({
    x = data.frame(x = 1:10)
  })
  
  expr.2 = expr({
    y = data.frame(y = 11:20)
  })

  M = multiverse()
  add_and_parse_code(M, expr.1)
  add_and_parse_code(M, expr.2)

  expect_true( is.list(code(M)) )
  expect_true( all(map_lgl(code(M), is.language)) )
})

test_that("`add_and_parse_code` parses the code", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })

  M = multiverse()
  add_and_parse_code(M, an_expr)

  M_tbl = expand(M)
  expect_equal( dim(M_tbl), c(4, 5) )
  expect_equal( length(parameters(M)), 1 )
  expect_equal( length(parameters(M)$value_y), 4 )
  expect_equal( M_tbl$.universe, 1:4 )
  expect_equal( M_tbl$value_y, c("0", "3", "x + 1", "x^2") )
  expect_equal( M_tbl$.parameter_assignment, 
    lapply(c("0", "3", "x + 1", "x^2"), function(x) list(value_y = x))
  )
})

test_that("`inside` executes the default analysis", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })

  M = multiverse()
  inside(M, !!an_expr)

  df = M$x
  df.ref =  data.frame(x = 1:10) %>%  mutate( y = 0 )

  expect_equal( as.list(df), as.list(df.ref) )
})

test_that("continuous parameters defined in the multiverse are evaluated", {
  .expr_1 = expr({
    y <- branch(foo, "option1" ~ 1, .options = 2:10)
  }) %>% expand_branch_options()

  .expr_2 = expr({
    y <- branch(foo, "option1" ~ 1, .options = seq(2, 3, by = 0.1))
  }) %>% expand_branch_options()

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


test_that("`inside` can access variables defined in the caller environment / parameters can be reused", {
  M = multiverse()
  df <- data.frame(x = 1:10) %>% mutate( y = x^2 + sample(10:20, 10))
  
  expect_error(inside(M, {
    df <- df %>% mutate( z = branch( value_y, y, log(y)))
  }), NA)
  
  expect_error(inside(M, {
    df2 <- df %>% mutate( z = branch( value_y, y, log(y)))
  }), NA)
})

test_that("inside cannot access variables which is not accessible from the environment the multiverse was declared in", {
  M <- multiverse()
  
  myfun <- function() {
    dat <- data.frame(x = 1:10) %>% mutate( y = x^2 + sample(10:20, 10))
    
    inside(M, { dat <- dat %>% mutate( z = branch( value_y, log(y), y)) })
  }
  
  expect_warning(myfun())
})
