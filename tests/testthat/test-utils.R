context("utils")

test_that("`expr_type()` returns correct output", {
  expect_equal(expr_type("a"), "constant")
  expect_equal(expr_type(expr(a)), "symbol")
  expect_equal(expr_type(expr(a(x = y))), "call")
  expect_equal(expr_type(~ a), "call")
  expect_equal(expr_type( x ~ a), "call")
})

test_that("`safe_f_rhs()` and `safe_f_lhs()` returns correct output", {
  # returns NULL for objects which are not of call
  expect_null(safe_f_lhs(a)$result)
  expect_null(safe_f_rhs(a)$result)

  expect_equal(safe_f_lhs(a ~ x)$result, expr(a))
  expect_equal(safe_f_rhs(a ~ x)$result, expr(x))
  expect_null(safe_f_lhs(a ~ x)$error)
  expect_null(safe_f_rhs(a ~ x)$error)

  expect_equal(safe_f_lhs(expr(f(a, x)))$result, expr(a))
  expect_equal(safe_f_rhs(expr(f(a, x)))$result, expr(x))
  expect_null(safe_f_lhs(expr(f(a, x)))$error)
  expect_null(safe_f_rhs(expr(f(a, x)))$error)

  expect_null(safe_f_lhs(expr(~a))$result)
  expect_equal(safe_f_rhs(expr(~a))$result, expr(a))
  expect_null(safe_f_lhs(expr(~a))$error)
  expect_null(safe_f_rhs(expr(~a))$error)
})

test_that("`create_name_from_expr()` returns correct output", {
  expect_equal(create_name_from_expr(quote(x + 1)), "x + 1")
  expect_equal(create_name_from_expr(quote(x + y + z)), "x + y + z")
  expect_equal(create_name_from_expr(quote(y)), "y")
  expect_equal(create_name_from_expr(quote(abc)), "abc")
  expect_equal(create_name_from_expr(quote(1)), 1)
  expect_equal(create_name_from_expr(quote(2)), 2)
})

test_that("`get_code_universe() returns correct output", {
  M = multiverse()
  
  inside(M, {
    x = branch(values_x, 0, 1, 2)
    y = branch(values_y, "true", "false")
  })
  
  inside(M, {
    z = branch(values_z, 13, 500)
  })
  
  m_dict_list = attr(M, "multiverse")$multiverse_diction$as_list()
  
  expect_equal(
    get_code_universe(m_dict_list, 1, 1),
    list(
      `1` = quote({ 
        x = 0  
        y = "true"
      })
    ))
  
  expect_equal(
    get_code_universe(m_dict_list, 2, 2),
    list(
      `1` = quote({ 
        x = 0  
        y = "true"
      }),
      `2` = quote({ 
        z = 500
      })
    ))
})
