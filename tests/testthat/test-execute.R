
context("execute")

values_x = exprs("a", 0, 5, 10, 14)

test_that("auto-executes the default analysis in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  y <- M$x
  expect_equal(y, "a")
})

test_that("`execute_multiverse` executes the all the analyses in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  
  execute_multiverse(M)
  expect_equal(extract_variable_from_universe(M, 1, x), values_x[[1]])
  expect_equal(extract_variable_from_universe(M, 2, x), values_x[[2]])
  expect_equal(extract_variable_from_universe(M, 3, x), values_x[[3]])
  expect_equal(extract_variable_from_universe(M, 4, x), values_x[[4]])
  expect_equal(extract_variable_from_universe(M, 5, x), values_x[[5]])
})


test_that("`execute_multiverse` sends error and warning messages but does not stop execution", {
  M <- multiverse()
  inside(M, {
    y <- 10
    x <- branch(value_x, 1, 0, stop("error"), 10, 14)
  } )
  
  expect_warning(execute_multiverse(M))
  
  df.extracted <- extract_variables(M, x) %>%
    select(-.parameter_assignment, -.code, -.results)
  
  ref <- tibble(
    .universe = 1:5,
    value_x = c('1', '0', expr('stop("error")'), '10', '14'),
    x = c(1, 0, NA, 10, 14)
  )
  
  expect_equal(as.list(df.extracted), as.list(ref))
})


test_that("`execute_universe` sends message about error but does not stop execution", {
  M <- multiverse()
  an_expr <- expr({
    y <- runif(10, 2, 1)
    x <- branch(value_x, 1, 0, stop("error"), 10, 14)
  })
  
  expect_warning( inside(M, !! an_expr) )
  
  M2 <- multiverse()
  inside(M2, {
    y <- runif(10, 0, 1)
    x <- branch(value_x, 1, 0, stop("error"), 10, 14)
  })
  expect_warning(execute_multiverse(M2))
  
  expect_true( env_has(expand(M2)$.results[[4]], "x") )
  expect_equal( env_get(expand(M2)$.results[[4]], "x"), 10 )
  expect_true( env_has(expand(M2)$.results[[5]], "x") )
  expect_equal( env_get(expand(M2)$.results[[5]], "x"), 14 )
})

test_that("multiverse sends warning message and continues to execute when warnings are encountered", {
  M <- multiverse()
  
  inside(M, {
    x <- branch(a_param,
                "1" ~ 1,
                "warning" ~ warning(10),
                "3" ~ 3
    )
  })
  
  expect_warning(execute_multiverse(M))
  expect_true(env_has(expand(M)$.results[[2]], "x"))
})
