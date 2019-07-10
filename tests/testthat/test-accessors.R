context("accessors")


test_that("basic assignment / retrieval works with `$`", {
  M <- multiverse()
  M$x <- ~ 5
  
  expect_equal(M$x, 5)
})
