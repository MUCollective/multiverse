library(dplyr)
library(tidyr)

test_that("basic assignment with `$` throws error", {
  M <- multiverse()
  expect_error(M$x <- ~ 5)
  expect_error(M$x <- 5)
})

test_that("basic retrieval with `$` returns correct output", {
  M <- multiverse()
  inside(M, {
    x = 5
    y = "a"
  })
  inside(M, {
    z = 2
  })
  expect_equal(M$x, 5)
  expect_equal(M$y, "a")
})

test_that("basic retrieval with `code()` returns correct output", {
  M <- multiverse()
  inside(M, {x <- 5})

  expect_equal(unname(code(M)), style_multiverse_code(quote({x <- 5})) )
})

test_that("`code()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(code(M))
})

test_that("basic retrieval with `parameters()` returns correct output", {
  M <- multiverse()
  inside(M, {branch( x_values, 0, 3, 5 )})

  expect_equal(parameters(M), list(x_values = list("0", "3", "5")) )
})

test_that("`parameters()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(parameters(M))
})

test_that("`conditions()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(conditions(M))
})

test_that("expand returns correct output", {
  M <- multiverse()
  add_and_parse_code(M, expr({
    df <- data.frame (x = 1:10 ) %>%
      mutate( y = branch( values_y, TRUE, FALSE )) %>%
      mutate(
        z = branch( values_z,
                    "constant" ~ 5,
                    "linear" ~ x + 1,
                    "sum" ~ (x + y)
        )
      )
  }))

  m_tbl = expand(M) %>% select(-.parameter_assignment, -.code, -.results, -.errors)
  params.list <- list(
    values_y = list("TRUE", "FALSE"),
    values_z = list("constant", "linear", "sum")
  )
  m_tbl.ref = expand.grid(rev(params.list), KEEP.OUT.ATTRS = FALSE)  %>%
    unnest(cols = everything()) %>%
    select(names(params.list)) %>%
    mutate( .universe = seq(1:nrow(.)) ) %>%
    select(.universe, everything())

  expect_equal( as.list(m_tbl), as.list(m_tbl.ref) )
})

test_that("size returns correct output for object with no universes", {
  M <- multiverse()
  expect_equal( size(M), 1 ) ## should return one
})

test_that("size returns correct output for object with 6 universes", {
  M <- multiverse()
  add_and_parse_code(M, expr({
    df <- data.frame (x = 1:10 ) %>%
      mutate( y = branch( values_y, TRUE, FALSE )) %>%
      mutate(
        z = branch( values_z,
                    "constant" ~ 5,
                    "linear" ~ x + 1,
                    "sum" ~ (x + y)
        )
      )
  }))
  
  expect_equal( size(M), 6 )
})

test_that("`multiverse_table()` throws error for objects of class other than multiverse", {
  M <- data.frame(x = 1:10)
  expect_error(multiverse_table(M))
})

test_that("`conditions()` returns correct output", {
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

test_that("`extract_variable_from_universe()` returns correct output", {
  M <- multiverse()
  
  inside(M, {
    x = 1
    y = branch( values_y, TRUE, FALSE )
    z = branch( values_z,
                "constant" ~ 5,
                "linear" ~ x + 2,
                "sum" ~ (x + y) %when% (values_y == TRUE)
    )
  })
  
  execute_multiverse(M)
  
  expect_equal( extract_variable_from_universe(M, 1, z), 5 )
  expect_equal( extract_variable_from_universe(M, 2, z), 3 )
  expect_equal( extract_variable_from_universe(M, 3, z), 2 )
})
