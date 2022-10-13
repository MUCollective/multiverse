# Tests to verify if expressions can be passed to the multiverse
# and if passed expressions are stored properly

library(rlang)
library(purrr)
library(dplyr)

test_that("inside works on new multiverse object", {
  an_expr = list( `1` = expr({x = data.frame(x = 1:10)}) )

  M = multiverse()
  inside(M, {
    x = data.frame(x = 1:10)
  })

  expect_equal( attr(M, 'multiverse')[['code']], an_expr )
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

  expect_equal( attr(M, 'multiverse')[['code']], some_exprs)
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
  
  expect_equal( attr(M, 'multiverse')[['code']], some_exprs)
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

  expect_true( is.list(attr(M, 'multiverse')[['code']]) )
  expect_true( all(map_lgl(attr(M, 'multiverse')[['code']], is.language)) )
})

test_that("`add_and_parse_code` parses the code", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })

  M = multiverse()
  add_and_parse_code(M, an_expr)

  M_tbl = expand(M)
  expect_equal( dim(M_tbl), c(4, 6) )
  expect_equal( length(parameters(M)), 1 )
  expect_equal( length(parameters(M)$value_y), 4 )
  expect_equal( M_tbl$.universe, 1:4 )
  expect_equal( M_tbl$value_y, c("0", "3", "x + 1", "x^2") )
  expect_equal( M_tbl$.parameter_assignment, 
    lapply(c("0", "3", "x + 1", "x^2"), function(x) list(value_y = x))
  )
})

test_that("`inside` executes the default analysis by default", {
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

test_that("`inside` does not execute the default analysis when specified as such", {
  an_expr = expr({
    x = data.frame(x = 1:10) %>%
      mutate( y = branch( value_y, 0, 3, x + 1, x^2))
  })
  
  M = multiverse()
  inside(M, !!an_expr, .execute_default = FALSE)
  expect_error(M$x, "object 'x' not found")
})

test_that("`expand_branch_options()`: continuous parameters defined in the multiverse are evaluated", {
  .expr_1 = expr({
    y <- branch(foo, "option1" ~ 1, .options = 2:10)
  })

  .expr_2 = expr({
    y <- branch(foo, "option1" ~ 1, .options = seq(2, 3, by = 0.1))
  })

  .ref_expr_1 = expr({
    y <- branch(foo, "option1" ~ 1, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)
  })

  .ref_expr_2 = expr({
    y <- branch(foo, "option1" ~ 1, 2, 2.1, 2.2, 2.3, 2.4, 2.5,
                2.6, 2.7, 2.8, 2.9, 3)
  })

  expect_equal(expand_branch_options(.expr_1), .ref_expr_1)
  expect_equal(expand_branch_options(.expr_2), .ref_expr_2)
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

test_that("multiverse with labels are correctly created and execution does not impact", {
  M <- multiverse()
  
  inside(M, {
    df <- tibble(x = 1:5)
    df <- df %>%
      mutate(y = branch(times,
                        "2" ~ x*2,
                        "3" ~ x*3,
                        "4" ~ x*4
      ))
  }, .label = "b1")
  
  inside(M, {
    df <- df %>%
      mutate(z = branch(exp,
                        "2" %when% (times != "3") ~ x^2,
                        "3" ~ x^3,
                        "4" ~ x^4
      ))
  }, .label = "b2")
  
  m_list = attr(M, "multiverse")$multiverse_diction$as_list()
  expect_equal(names(m_list), c("b1", "b2"))
  
  execute_multiverse(M)
  expect_equal(names(m_list), c("b1", "b2"))
})

test_that("multiverse with labels are correctly created and execution does not impact #2", {
  M <- multiverse()
  
  inside(M, {
    df <- tibble(x = 1:5)
    df <- df %>%
      mutate(y = branch(times,
                        "2" ~ x*2,
                        "3" ~ x*3,
                        "4" ~ x*4
      ))
  }, .label = "b1", .execute_default = FALSE)
  
  inside(M, {
    df <- df %>%
      mutate(z = branch(exp,
                        "2" %when% (times != "3") ~ x^2,
                        "3" ~ x^3,
                        "4" ~ x^4
      ))
  }, .label = "b2", .execute_default = FALSE)
  
  m_list = attr(M, "multiverse")$multiverse_diction$as_list()
  expect_equal(names(m_list), c("b1", "b2"))
  
  execute_multiverse(M)
  expect_equal(names(m_list), c("b1", "b2"))
})


test_that("inside: unchanged_until and exec_until are correct", {
  M <- multiverse()

  # unchanged_until should be undefined at the beginning
  expect_equal(attr(M, "multiverse")$unchanged_until, NA)

  inside(M, {
    x <- branch(value_x, 0, 5, 14)
    z <- branch(value_z, 2, 3)
  })

  inside(M, {
    y = branch(value_y, 2, 7)
  })

  expect_equal(attr(M, "multiverse")$unchanged_until, 1) # we've added two code blocks to the multiverse

  inside(M, {
    x <- branch(value_x, 3, 6, 9)
    z <- branch(value_z, 3, 4)
  }, .label = "1") # we've changed the first code block

  expect_equal(attr(M, "multiverse")$unchanged_until, NA)

  inside(M, {
    w = branch(value_w, 0, 1)
  })

  inside(M, {
    a = branch(value_a, "true", "false")
  })

  expect_equal(attr(M, "multiverse")$unchanged_until, 3)

  inside(M, {
    y = branch(value_y, 2, 7)
  }, .label = "2") # we've changed the second code block now

  expect_equal(attr(M, "multiverse")$unchanged_until, 1)
})
