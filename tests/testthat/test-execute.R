
context("execute")

values_x = exprs("a", 0, 5, 10, 14)

test_that("`execute_default` executes the default analysis in the multiverse", {
  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"),
                     expr({x <- branch(value_x, "a", 0, 5, 10, 14)}), execute = FALSE
  )
  execute_default(M)
  expect_equal(M$x, "a")
})

test_that("`execute_multiverse` executes the all the analyses in the multiverse", {
  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"),
                     expr({x <- branch(value_x, "a", 0, 5, 10, 14)}), execute = FALSE
  )
  execute_multiverse(M)
  expect_equal(from_universe_i(M, 1, x), values_x[[1]])
  expect_equal(from_universe_i(M, 2, x), values_x[[2]])
  expect_equal(from_universe_i(M, 3, x), values_x[[3]])
  expect_equal(from_universe_i(M, 4, x), values_x[[4]])
  expect_equal(from_universe_i(M, 5, x), values_x[[5]])
})
