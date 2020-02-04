context("accessors")

library(dplyr)
library(tidyr)

test_that("basic assignment / retrieval works with `$`", {
  M <- multiverse()
  M$x <- ~ 5

  expect_equal(M$x, 5)
})

test_that("basic retrieval works with `code()`", {
  M <- multiverse()
  M$x <- ~ 5

  expect_equal(code(M), list(expr({ x <- 5 })) )
})

test_that("`code()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(code(M))
})

test_that("basic retrieval works with `parameters()`", {
  M <- multiverse()
  M$x <- ~ branch( x_values, 0, 3, 5 )

  expect_equal(parameters(M), list(x_values = list("0", "3", "5")) )
})

test_that("`parameters()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(parameters(M))
})

test_that("basic retrieval works with `conditions()`", {
  M <- multiverse()
  inside(M, {
    df <- data.frame (x = 1:10 ) %>%
      mutate( y = branch( values_y,
                          TRUE,
                          FALSE
      )) %>%
      mutate(
        z = branch( values_z,
                    "constant" ~ 5,
                    "linear" ~ x + 1,
                    "sum" ~ (x + y) %when% (values_y == TRUE)
        )
      )
  })

  expect_equal( conditions(M), list(expr((values_z != "sum" | (values_y == TRUE)))) )
})

test_that("`conditions()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(conditions(M))
})

test_that("basic retrieval works with `multiverse()`", {
  M <- multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
    df <- data.frame (x = 1:10 ) %>%
      mutate( y = branch( values_y, TRUE, FALSE )) %>%
      mutate(
        z = branch( values_z,
                    "constant" ~ 5,
                    "linear" ~ x + 1,
                    "sum" ~ (x + y)
        )
      )
  }), execute = FALSE)

  m_tbl = multiverse_table(M) %>% select(-.parameter_assignment, -.code, -.results)
  m_tbl.ref = expand.grid(list(
    values_y = list("TRUE", "FALSE"),
    values_z = list("constant", "linear", "sum")
  ), KEEP.OUT.ATTRS = FALSE)  %>%
    unnest(cols = everything()) %>%
    mutate( .universe = seq(1:nrow(.)) ) %>%
    select(.universe, everything())

  expect_equal( as.list(m_tbl), as.list(m_tbl.ref) )
})

test_that("`multiverse_table()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(multiverse_table(M))
})
