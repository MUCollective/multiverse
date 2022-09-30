test_that("`print()` returns correct output", {
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
  
  out.print = capture.output(print(M))
  
  out = c(
    'Multiverse',
    '',
    '  Multiverse consists of 5 different analyses',
    '',
    '   Parameters: ',
    '     Parameter name: values_y ',
    '        options: TRUE FALSE ',
    '     Parameter name: values_z ',
    '        options: constant linear sum ',
    '',
    '   Conditions: ',
    '         values_z != "sum" | (values_y == TRUE) '
  )
  
  expect_equal(out.print, out)
})

