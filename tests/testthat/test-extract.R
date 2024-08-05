library(dplyr)
library(tidyr)


test_that("can extract one or more single valued variables from the multiverse", {
  M <- multiverse()
  
  inside(M, {
    x_value = branch(x, .options = 1:10)
    x2 = x_value^2
    x3 = x_value^3
  })
  
  execute_multiverse(M)
  
  out <- extract_variables(expand(M), x_value, x2, x3) %>%
    select(x, x_value, x2, x3)
  
  ref <- tibble(x = 1:10) %>% mutate(
    x_value = x,
    x2 = x^2,
    x3 = x^3,
    x = as.character(x)
  )
  
  expect_equal(lapply(as.list(out), unname), as.list(ref))
})

test_that("can extract variables from the multiverse which are not at the last level of the tree", {
  M <- multiverse()
  
  inside(M, {
    x_value = branch(x, x1 = "a", x2 = "b" )
  })
  
  inside(M, {
    y_value = branch(y, .options = 11:20)
  })
  
  execute_multiverse(M)
  
  out <- extract_variables(expand(M), x_value) %>%
    select(x_value)
  
  ref <- tibble(x_value = rep(c("a", "b"), each = 10))
  
  expect_equal(lapply(as.list(out), unname), as.list(ref))
})

test_that("can extract vectors and lists from the multiverse as list columns", {
  M <- multiverse()
  
  inside(M, {
    x_value = branch(x, .options = 1:10)
    x2 = x_value^2
    x3 = x_value^3
    
    avec <- c(x_value, x2, x3)
    alist <- list(x_value, x2, x3)
  })
  
  execute_multiverse(M)
  
  out.1 <- extract_variables(expand(M), avec) %>%
    select(x, avec)
  
  ref.1 <- tibble(x = 1:10) %>% mutate(
    avec = map(x, function(i) c(i, i^2, i^3)),
    x = as.character(x)
  )
  
  out.2 <- extract_variables(expand(M), alist) %>%
    select(x, alist)
  
  ref.2 <- tibble(x = 1:10) %>% mutate(
    alist = map(x, function(i) list(i, i^2, i^3)),
    x = as.character(x)
  )
  
  expect_equal(lapply(as.list(out.1), unname), as.list(ref.1))
  expect_equal(lapply(as.list(out.2), unname), as.list(ref.2))
})

test_that("can extract vectors from the multiverse as list columns", {
  M <- multiverse()
  
  inside(M, {
    x_value = branch(x, .options = 1:10)
    df <- tibble(y = 1:3) %>%
      mutate( x_value = x_value ^ y)
  })
  
  execute_multiverse(M)
  
  out <- extract_variables(expand(M), df) %>%
    select(x, df)
  
  ref <- tibble(x = 1:10) %>% mutate(
    df = map(x, function(i) tibble(y = 1:3, x_value = i^y)),
    x = as.character(x)
  )
  
  expect_equal(lapply(as.list(out), unname), as.list(ref))
})