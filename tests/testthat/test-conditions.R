test_that("correct parent environments are identified for multiverse with conditions", {
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
  
  block_1 <- attr(M, "multiverse")$multiverse_diction$as_list()[["b1"]]
  envs_block1 <- lapply(block_1, `[[`, "env")
  
  block_2 <- attr(M, "multiverse")$multiverse_diction$as_list()[["b2"]]
  envs_block2 <- lapply(lapply(block_2, `[[`, "env"), parent.env)
  
  expect_true(identical(envs_block2[[1]], envs_block1[[1]]))
  expect_true(identical(envs_block2[[2]], envs_block1[[1]]))
  expect_true(identical(envs_block2[[3]], envs_block1[[1]]))
  expect_true(identical(envs_block2[[4]], envs_block1[[2]]))
  expect_true(identical(envs_block2[[5]], envs_block1[[2]]))
  expect_true(identical(envs_block2[[6]], envs_block1[[3]]))
  expect_true(identical(envs_block2[[7]], envs_block1[[3]]))
  expect_true(identical(envs_block2[[8]], envs_block1[[3]]))
})


test_that("invalid conditions are removed in multiverse_diction when parsing a multiverse #1", {
  M <- multiverse()
  
  inside(M, {
    df <- data.frame(
      x = branch(x_values, 
                 "1to10" ~ runif(100, 1, 10), 
                 "-1to1" ~ runif(100, -1, 1), 
                 "5to10" ~ runif(100, 5, 10)
      ),
      y = runif(100, 5, 10),
      z = 0
    )
    
    df2 <- branch(transform_values,
                  "log_x" %when% (x_values != "-1to1") ~ mutate(df, logx = log(x)),
                  "log_y" ~ mutate(df, logy = log(y))
    )
  })
  
  m_dict <- attr(M, "multiverse")$multiverse_diction
  
  diction.assignment <- map(m_dict$as_list()[[1]], `[[`, "parameter_assignment")
  
  ref.assignment <- list(
    list( transform_values = "log_x", x_values = "1to10" ),
    list( transform_values = "log_y", x_values = "1to10" ),
    list( transform_values = "log_y", x_values = "-1to1" ),
    list( transform_values = "log_x", x_values = "5to10" ),
    list( transform_values = "log_y", x_values = "5to10" )
  )
  
  parsed.code <- unlist(map(m_dict$as_list()[[1]], ~ unname(.[["code"]])), recursive = FALSE)
  
  ref.code <- list(
    expr({
      df <- data.frame(x = runif(100, 1, 10), y = runif(100, 5, 10), z = 0)
      df2 <- mutate(df, logx = log(x))
    }), 
    expr({
      df <- data.frame(x = runif(100, 1, 10), y = runif(100, 5, 10), z = 0)
      df2 <- mutate(df, logy = log(y))
    }), 
    expr({
      df <- data.frame(x = runif(100, -1, 1), y = runif(100, 5, 10), z = 0)
      df2 <- mutate(df, logy = log(y))
    }), 
    expr({
      df <- data.frame(x = runif(100, 5, 10), y = runif(100, 5, 10), z = 0)
      df2 <- mutate(df, logx = log(x))
    }), 
    expr({
      df <- data.frame(x = runif(100, 5, 10), y = runif(100, 5, 10), z = 0)
      df2 <- mutate(df, logy = log(y))
    })
  )
  
  expect_equal(length(m_dict$as_list()[[1]]), 5)
  expect_equal(ref.assignment, diction.assignment)
  expect_equal(parsed.code, ref.code)
})

test_that("invalid conditions are removed in multiverse_diction when parsing a multiverse #1", {
  M <- multiverse()
  
  inside(M, {
    df <- data.frame(y = 1:10) %>%
      mutate(
        x = branch(x_value, "a" ~ "a", "b" ~ "b"),
        z = branch(z_value, "c" %when% (x_value != "a") ~ "c", "d" %when% (x_value != "b") ~ "d"),
        w = branch(w_value, 1, 2)
      )
  })
  
  m_dict <- attr(M, "multiverse")$multiverse_diction
  
  execute_multiverse(M)
  
  diction.assignment <- map(m_dict$as_list()[[1]], `[[`, "parameter_assignment")
  
  ref.assignment <- list(
    list( w_value = "1", z_value = "d", x_value = "a" ),
    list( w_value = "2", z_value = "d", x_value = "a" ),
    list( w_value = "1", z_value = "c", x_value = "b" ),
    list( w_value = "2", z_value = "c", x_value = "b" )
  )
  
  parsed.code <- unlist(map(m_dict$as_list()[[1]], ~ unname(.[["code"]])), recursive = FALSE)
  
  ref.code <- list(
    expr({ df <- data.frame(y = 1:10) %>% mutate(x = "a", z = "d", w = 1) }),
    expr({ df <- data.frame(y = 1:10) %>% mutate(x = "a", z = "d", w = 2) }),
    expr({ df <- data.frame(y = 1:10) %>% mutate(x = "b", z = "c", w = 1) }),
    expr({ df <- data.frame(y = 1:10) %>% mutate(x = "b", z = "c", w = 2) })
  )
  
  expect_equal(length(m_dict$as_list()[[1]]), 4)
  expect_equal(ref.assignment, diction.assignment)
  expect_equal(parsed.code, ref.code)
})