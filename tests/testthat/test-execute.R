library(future)
library(tidyr)
library(dplyr)
library(rlang)

values_x = exprs("a", 0, 5, 10, 14)
values_y = exprs("b", 2, 1, 4, 3)
values_z = exprs(0, 0, 5, 10, 14, 28)

test_that("auto-executes the default analysis in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  y <- M$x
  expect_equal(y, "a")
})

test_that("`execute_multiverse` executes all the analyses in the multiverse", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, "a", 0, 5, 10, 14)} )
  
  execute_multiverse(M)
  expect_equal(extract_variable_from_universe(M, 1, x), values_x[[1]])
  expect_equal(extract_variable_from_universe(M, 2, x), values_x[[2]])
  expect_equal(extract_variable_from_universe(M, 3, x), values_x[[3]])
  expect_equal(extract_variable_from_universe(M, 4, x), values_x[[4]])
  expect_equal(extract_variable_from_universe(M, 5, x), values_x[[5]])
})

test_that("`execute_multiverse` executes all the analyses across two inside blocks", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, 0, 5, 14)} )
  
  inside(M, {
    y <- branch(value_y, 1, 2)
    
    z <- x * y
  })
  
  execute_multiverse(M)
  expect_equal(extract_variable_from_universe(M, 1, z), values_z[[1]])
  expect_equal(extract_variable_from_universe(M, 2, z), values_z[[2]])
  expect_equal(extract_variable_from_universe(M, 3, z), values_z[[3]])
  expect_equal(extract_variable_from_universe(M, 4, z), values_z[[4]])
  expect_equal(extract_variable_from_universe(M, 5, z), values_z[[5]])
  expect_equal(extract_variable_from_universe(M, 6, z), values_z[[6]])
})

test_that("`execute_linear` executes all the analyses across two inside blocks", {
  M <- multiverse()
  inside(M, {x <- branch(value_x, 0, 5, 14)} )
  
  inside(M, {
    y <- branch(value_y, 1, 2)
    
    z <- x * y
  } )
  
  execute_linear(M, FALSE, FALSE)
  expect_equal(extract_variable_from_universe(M, 1, z), values_z[[1]])
  expect_equal(extract_variable_from_universe(M, 2, z), values_z[[2]])
  expect_equal(extract_variable_from_universe(M, 3, z), values_z[[3]])
  expect_equal(extract_variable_from_universe(M, 4, z), values_z[[4]])
  expect_equal(extract_variable_from_universe(M, 5, z), values_z[[5]])
  expect_equal(extract_variable_from_universe(M, 6, z), values_z[[6]])
})

test_that("`execute_linear` executes all the analyses across two inside blocks during parallel computation", {
  M <- multiverse()
  
  inside(M, {x <- branch(value_x, 0, 5, 14)} )
  
  inside(M, {
    y <- branch(value_y, 1, 2)
    
    z <- x * y
  } )
  
  plan(multisession, workers = 2)
  execute_linear(M, TRUE, FALSE)
  plan(sequential)
  
  expect_equal(extract_variable_from_universe(M, 1, z), values_z[[1]])
  expect_equal(extract_variable_from_universe(M, 2, z), values_z[[2]])
  expect_equal(extract_variable_from_universe(M, 3, z), values_z[[3]])
  expect_equal(extract_variable_from_universe(M, 4, z), values_z[[4]])
  expect_equal(extract_variable_from_universe(M, 5, z), values_z[[5]])
  expect_equal(extract_variable_from_universe(M, 6, z), values_z[[6]])
})

test_that("parallel computation is supported", {
  M_cores_2.1 <- multiverse()

  inside(M_cores_2.1, {x <- branch(value_x, 0, 5, 10, 14)} )
  inside(M_cores_2.1, {y <- branch(value_y, 1, 2, 3, 4)} )
  
  ref_list.1 = crossing(
    x = c(0, 5, 10, 14),
    y = c(1, 2, 3, 4)
  )
  
  M_cores_2.2 <- multiverse()
  
  inside(M_cores_2.2, {
    var1 <- branch(
      var_num,
      "var1" ~ 1,
      "var2" ~ 2,
      "var3" ~ 3
    )
  })
  
  inside(M_cores_2.2, {
    var2 <- branch(
      var_char,
      "vara" ~ "a",
      "varb" ~ "b",
      "varc" ~ "c"
    )
  })
  
  ref_list.2 = crossing(
    var1 = c(1, 2, 3),
    var2 = c("a", "b", "c")
  )
  
  plan(multisession, workers = 2)
  execute_multiverse(M_cores_2.1, parallel = TRUE)
  execute_multiverse(M_cores_2.2, parallel = TRUE)
  plan(sequential)
  
  expect_equal(
    lapply(as.list(select(extract_variables(M_cores_2.1, x, y), x, y)), unname), 
    as.list( ref_list.1 )
  )
  
  expect_equal(
    lapply(as.list(mutate(select(extract_variables(M_cores_2.2, var1, var2), var1, var2), across(var1:var2, unlist))), unname),
    as.list( ref_list.2)
  )
})

test_that("`execute_multiverse` sends error and warning messages but does not stop execution", {
  M <- multiverse()
  inside(M, {
    y <- 10
    x <- branch(value_x, 1, 0, stop("error"), 10, 14)
  } )
  
  expect_warning(execute_multiverse(M))
  
  df.extracted <- extract_variables(M, x) %>%
    select(-.parameter_assignment, -.code, -.results, -.errors)
  
  ref <- tibble(
    .universe = 1:5,
    value_x = c('1', '0', expr('stop("error")'), '10', '14'),
    x = c(1, 0, NA, 10, 14)
  )
  
  expect_equal(lapply(as.list(df.extracted), unname), as.list(ref))
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

test_that("`execute_universe` executes the correct universe", {
  M <- multiverse()
  an_expr <- expr({
    x <- branch(value_x, 1, 2, 3, 4)
  })
  
  inside(M, !!an_expr)
  
  execute_universe(M, .universe = 3)
  expect_true(env_has(expand(M)$.results[[3]], "x"))
  expect_equal(env_get(expand(M)$.results[[3]], "x"), 3)
  expect_false(env_has(expand(M)$.results[[2]], "x"))
})

test_that("test 1/N: `execute_universe` executes the correct universe with multiple levels", {
  M <- multiverse()
  an_expr.1 <- expr({
    x <- branch(value_x, 1, 2, 3, 4)
  })
  
  an_expr.2 <- expr({
    y <- branch(value_y, 5, 6, 7)
    z <- x + y
  })
  
  inside(M, !!an_expr.1)
  inside(M, !!an_expr.2)
  
  execute_universe(M, .universe = 3)
  expect_true(env_has(expand(M)$.results[[3]], "z"))
  expect_equal(env_get(expand(M)$.results[[3]], "z"), 8)
  expect_false(env_has(expand(M)$.results[[10]], "x"))
  expect_false(env_has(expand(M)$.results[[10]], "z"))
  
  execute_universe(M, .universe = 10)
  expect_true(env_has(expand(M)$.results[[10]], "x"))
  expect_equal(env_get(expand(M)$.results[[10]], "x"), 4)
  expect_equal(env_get(expand(M)$.results[[10]], "z"), 9)
})

test_that("test 2/N: `execute_universe` executes the correct universe with multiple levels", {
  M <- multiverse()
  an_expr.1 <- expr({
    x <- branch(value_x, 4, 5, 6)
  })
  
  an_expr.2 <- expr({
    y <- branch(value_y, 11, 21, 31)
  })
  
  an_expr.3 <- expr({
    z <- branch(calc, x + y, x * y)
  })
  
  inside(M, !!an_expr.1, .execute_default = FALSE)
  inside(M, !!an_expr.2, .execute_default = FALSE)
  inside(M, !!an_expr.3, .execute_default = FALSE)
  
  execute_universe(M, .universe = 2)
  expect_true(env_has(expand(M)$.results[[2]], "z"))
  expect_true(env_has(expand(M)$.results[[2]], "x"))
  expect_true(env_has(expand(M)$.results[[2]], "y"))
  expect_equal(env_get(expand(M)$.results[[2]], "z"), 44)
  expect_false(env_has(expand(M)$.results[[1]], "x"))
  expect_false(env_has(expand(M)$.results[[1]], "y"))
  expect_false(env_has(expand(M)$.results[[5]], "x"))
  expect_false(env_has(expand(M)$.results[[5]], "y"))
  
  execute_universe(M, .universe = 9)
  expect_equal(env_get(expand(M)$.results[[9]], "z"), 26)
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

test_that("multiverses created with `tree = FALSE` executes all analyses correctly", {
  options(tree = FALSE)
  
  M <- multiverse()
  inside(M, {x <- branch(value_x, 0, 5, 14)}, .execute_default = FALSE )
  inside(M, {
    y <- branch(value_y, 1, 2)
    
    z <- x * y
  }, .execute_default = FALSE)
  
  execute_multiverse(M, FALSE, FALSE)
  options(tree = TRUE)
  
  expect_equal(extract_variable_from_universe(M, 1, z), values_z[[1]])
  expect_equal(extract_variable_from_universe(M, 2, z), values_z[[2]])
  expect_equal(extract_variable_from_universe(M, 3, z), values_z[[3]])
  expect_equal(extract_variable_from_universe(M, 4, z), values_z[[4]])
  expect_equal(extract_variable_from_universe(M, 5, z), values_z[[5]])
  expect_equal(extract_variable_from_universe(M, 6, z), values_z[[6]])
})

test_that("`execute_linear` executes all the analyses across two inside blocks during parallel computation when tree = FALSE", {
  options(tree = FALSE)
  
  M <- multiverse()
  inside(M, {x <- branch(value_x, 0, 5, 14)}, .execute_default = FALSE )
  inside(M, {
    y <- branch(value_y, 1, 2)
    
    z <- x * y
  }, .execute_default = FALSE)
  
  plan(multisession, workers = 2)
  execute_linear(M, TRUE, FALSE)
  plan(sequential)
  
  options(tree = TRUE)
  
  expect_equal(extract_variable_from_universe(M, 1, z), values_z[[1]])
  expect_equal(extract_variable_from_universe(M, 2, z), values_z[[2]])
  expect_equal(extract_variable_from_universe(M, 3, z), values_z[[3]])
  expect_equal(extract_variable_from_universe(M, 4, z), values_z[[4]])
  expect_equal(extract_variable_from_universe(M, 5, z), values_z[[5]])
  expect_equal(extract_variable_from_universe(M, 6, z), values_z[[6]])
})

test_that("execute_universe outputs correct result during parallel execution", {
  M = multiverse()
  
  df = data.frame(a = 1, b = 2)
  
  inside(M, {
    x = branch(x_values, 1, 2, 3)
    y = "a"
  }, .execute_default = FALSE)
  
  inside(M, {
    z = branch(z_values, 4, 5, 6)
    w = "b"
    df = df %>%
      mutate(
        astr = paste0(y, w),
        anmbr = x*z + a
      )
  }, .execute_default = FALSE) 

  plan(multisession, workers = 3)
  execute_universe(M, 4:6)
  plan(sequential)
  
  .res = expand(M) %>%
    extract_variables(df) %>%
    unnest(df) %>%
    select(-c(.parameter_assignment, .code, .results, .errors))
  
  .res_ref = expand.grid(z_values = c('4', '5', '6'), x_values = c('1', '2', '3'), KEEP.OUT.ATTRS = FALSE, stringsAsFactors=FALSE) %>%
    mutate(
      .universe = row_number(),
      a = 1, b = 2, 
      astr = c(rep(NA, 3), rep("ab", 3), rep(NA, 3)),
      anmbr = c(rep(NA, 3), 4:6*2 + 1, rep(NA, 3))
    ) %>%
    select(.universe, x_values, z_values, everything())
  
  expect_equal(as.list(.res), as.list(.res_ref))
})

test_that("execute_universe outputs correct result during parallel execution", {
  M = multiverse()
  
  df = data.frame(a = 1, b = 2)
  
  inside(M, {
    x = branch(x_values, 1, 2, 3)
    y = "a"
  }, .execute_default = FALSE)
  
  inside(M, {
    z = branch(z_values, 4, 5, 6)
    w = "b"
    df = df %>%
      mutate(
        astr = paste0(y, w),
        anmbr = x*z + a
      )
  }, .execute_default = FALSE) 
  
  N = nrow(expand(M))
  
  plan(multisession, workers = 3)
  execute_universe(M, 1:N, TRUE)
  plan(sequential)
  
  .res = expand(M) %>%
    extract_variables(df) %>%
    unnest(df) %>%
    select(-c(.parameter_assignment, .code, .results, .errors))
  
  .res_ref = expand.grid(z_values = c('4', '5', '6'), x_values = c('1', '2', '3'), KEEP.OUT.ATTRS = FALSE, stringsAsFactors=FALSE) %>%
    mutate(
      .universe = row_number(),
      a = 1, b = 2, 
      astr = 'ab',
      anmbr = c(4:6, 4:6*2, 4:6*3) + 1
    ) %>%
    select(.universe, x_values, z_values, everything())
  
  expect_equal(as.list(.res), as.list(.res_ref))
})

test_that("execute_universe outputs correct result during parallel execution of a partial multiverse", {
  M = multiverse()
  
  df = data.frame(a = 1, b = 2)
  
  inside(M, {
    x = branch(x_values, 1, 2, 3)
    y = "a"
  }, .execute_default = FALSE)
  
  inside(M, {
    z = branch(z_values, 4, 5, 6)
    w = "b"
    df = df %>%
      mutate(
        astr = paste0(y, w),
        anmbr = x*z + a
      )
    
    astr = df$astr
    anmbr = df$anmbr
  }, .execute_default = FALSE) 
  
  N = 3:6
  
  plan(multisession, workers = 3)
  execute_universe(M, N, TRUE)
  plan(sequential)
  
  .res = expand(M) %>%
    extract_variables(astr, anmbr) %>%
    select(-c(.parameter_assignment, .code, .results, .errors))
  
  .res_ref = expand.grid(z_values = c('4', '5', '6'), x_values = c('1', '2', '3'), KEEP.OUT.ATTRS = FALSE, stringsAsFactors=FALSE) %>%
    mutate(
      .universe = row_number(),
      astr = c(rep(NA, 2), rep("ab", 4), rep(NA, 3)),
      anmbr = c(rep(NA, 2), N*2 + 1, rep(NA, 3))
    ) %>%
    select(.universe, x_values, z_values, everything())
  
  expect_equal(as.list(unnest(.res, c(astr, anmbr))), as.list(.res_ref))
})

