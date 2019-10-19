# Tests for succesfully parsing the expressions passed into the multiverse
# to identify parameters and branches (values for each parameter)
context("parse_multiverse")

library(rlang)
library(tidyr)
library(dplyr)
library(purrr)


# get_branch_parameter_conditions -----------------------------------------

test_that("the output of `get_branch_parameter_conditions` on a `branch` call", {
  a_branch_call = expr(
    branch(mentrual_calculation,
           "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
           "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
           "mc_option3" ~ StartDateNext)
    )
  parameter_names = list(mentrual_calculation = list("mc_option1", "mc_option2", "mc_option3"))
  branch_parameters = get_branch_parameter_conditions(a_branch_call)

  expect_equal(names(branch_parameters), c("parameters", "conditions"))
  expect_equal(branch_parameters[["parameters"]], parameter_names)
})


# combine_parameter_conditions -----------------------------------------

test_that("combine_parameter_conditions properly combines non-overlapping parameters and conditions", {
   l.1 = list(parameters = list(a = 1:3, b = 3:4), conditions = letters[1:3])
   l.2 = list(parameters = list(c = 4:6, d = 1:2), conditions = letters[4:6])
   l = list(parameters = list(a = 1:3, b = 3:4, c = 4:6, d = 1:2), conditions = letters[1:6])

  expect_equal(combine_parameter_conditions(l.1, l.2), l)
})

test_that("combine_parameter_conditions properly combines overlapping parameters and conditions", {
  l.1 = list(parameters = list(a = 1:3, b = 3:4), conditions = letters[1:3])
  l.2 = list(parameters = list(a = c(2,4,6), b = 1:5), conditions = letters[4:6])
  l = list(parameters = list(a = c(1,2,3,4,6), b = c(3,4,1,2,5)), conditions = letters[1:6])

  expect_equal(combine_parameter_conditions(l.1, l.2), l)
})

# get_parameter_conditions ------------------------------------------------
test_that("option names of integer types are supported in branches", {
  an_expr = expr({
    df = df %>%
      mutate(x = branch(
        values_x,
        0 ~ 0,
        3 ~ 3
      ))
  })
  option_names = get_parameter_conditions(an_expr)$parameters
  option_names.ref = list(values_x = list(0, 3))

  expect_equal( option_names, option_names.ref )
})

test_that("option names of more than one type (numeric & character) throw an error", {
  an_expr = expr({
    df = df %>%
      mutate(x = branch(
        values_x,
        0 ~ 0,
        3 ~ 3,
        "y + 5" ~ y + 5
      ))
  })

  expect_error(get_parameter_conditions(an_expr))
})

test_that("unnamed option names of type call are converted into strings", {
  an_expr = expr({
    df = df %>%
      mutate(x = branch(
        values_x,
        y^2 + 1,
        y + 5
      ))
  })

  option_names = get_parameter_conditions(an_expr)$parameters
  option_names.ref = list(values_x = list("y^2 + 1", "y + 5"))

  expect_equal( option_names, option_names.ref )
})

test_that("unnamed options in branches are converted into option names: `char` -> `char`, `numeric` -> `numeric`, `call` -> `char` ", {
  an_expr = expr({
    df = mutate(df, x = branch(
      values_x,
      0,
      3,
      "abc",
      y + 5
    ))
  })

  option_names = get_parameter_conditions(an_expr)$parameters
  option_names.ref = list(values_x = list("0", "3", "abc", "y + 5"))
  expect_equal( option_names, option_names.ref )
})

test_that("`get_parameter_conditions` returns an empty list when input expression has no branch", {
  an_expr = expr({
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  })

  a_list.no_branch = expect_warning(flatten(get_parameter_conditions(an_expr)))
  expect_equal(a_list.no_branch, list())
})

test_that("`get_parameter_conditions` returns the correct output (list) when input expression has a single branch", {
  an_expr = expr({
    df <- test_df %>%
      mutate(NextMenstrualOnset = branch(mentrual_calculation,
           "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
           "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
           "mc_option3" ~ StartDateNext)
      )
  })

  output = list(
    parameters = list(mentrual_calculation = list("mc_option1", "mc_option2", "mc_option3")),
    conditions = list()
  )

  expect_equal(get_parameter_conditions(an_expr), output)
})

test_that("`get_parameter_conditions` returns the correct output (list) when input expression has multiple branches", {
  an_expr = expr({
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = branch(mentrual_calculation,
          "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
          "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
          "mc_option3" ~ StartDateNext)
      ) %>%
      mutate(Relationship = branch( relationship_status,
          "rs_option1" ~ factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')),
          "rs_option2" ~ factor(ifelse(Relationship==1, 'Single', 'Relationship')),
          "rs_option3" ~ factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) )
      ) %>%
      filter( branch(cycle_length,
          "cl_option1" ~ TRUE,
          "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
          "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      ))
  })

  output = list(
    parameters = list(
        mentrual_calculation = list("mc_option1", "mc_option2", "mc_option3"),
        relationship_status = list("rs_option1", "rs_option2", "rs_option3"),
        cycle_length = list("cl_option1", "cl_option2", "cl_option3")
    ),
    conditions = list()
  )

  expect_equal(get_parameter_conditions(an_expr), output)
})


# get_conditions ------------------------------------------------------------
test_that("`get_condition` is able to extract conditions in the correct format", {
  an_expr = expr(branch(value_z,
                        "constant" ~ 5,
                        "linear" ~ x + 1,
                        "sum" ~ (x + y) %when% (value_y == TRUE)
  ))

  conditions_list = map(an_expr[-1:-2], ~ get_condition(.x, 'value_z'))
  conditions_list.ref = list(NULL, NULL, expr(("value_z" != "sum" | (value_y == TRUE))))

  expect_equal(conditions_list, conditions_list.ref)
})

test_that("`get_condition` is able to extract conditions in the correct format", {
  an_expr = expr(branch(value_z,
                        5,
                        x + 1,
                        (x + y) %when% (value_y == TRUE)
  ))

  conditions_list = map(an_expr[-1:-2], ~ get_condition(.x, 'value_z'))
  conditions_list.ref = list(NULL, NULL, expr(("value_z" != "(x + y)" | (value_y == TRUE))))

  expect_equal(conditions_list, conditions_list.ref)
})

test_that("`get_condition` ignores `%when%` if assigned to subexpression of an option", {
  an_expr.1 = expr(branch(value_z,
                        "constant" ~ 5,
                        "linear" ~ x + 1,
                        "sum" %when% (values_y == TRUE) ~ x + y,
                        "sum" ~ (x %when% (value_y == TRUE)) + y
  ))
  an_expr.2 = expr(branch(value_z,
                          "constant" ~ 5,
                          "linear" ~ x + 1,
                          "sum" %when% (values_y == TRUE) ~ x + y,
                          "sum" ~ (x + (y %when% (value_y == TRUE)))
  ))

  ref = list( NULL, NULL, expr( ("value_z" != "sum" | (values_y == TRUE)) ), NULL)

  expect_equal( map(an_expr.1[-1:-2], ~ get_condition(.x, 'value_z')), ref )
  expect_equal( map(an_expr.2[-1:-2], ~ get_condition(.x, 'value_z')), ref )
})


# parse_multiverse --------------------------------------------------------

test_that("`parse_multiverse` returns the complete parameter table", {
  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = branch(menstrual_calculation,
          "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
          "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
          "mc_option3" ~ StartDateNext)
      ) %>%
      mutate(Relationship = branch( relationship_status,
          "rs_option1" ~ factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')),
          "rs_option2" ~ factor(ifelse(Relationship==1, 'Single', 'Relationship')),
          "rs_option3" ~ factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) )
      )%>%
      dplyr::filter( branch(cycle_length,
          "cl_option1" ~ TRUE,
          "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
          "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      dplyr::filter( branch(certainty,
          "cer_option1" ~ TRUE,
          "cer_option2" ~ Sure1 > 6 | Sure2 > 6
      ))
  }), execute = FALSE)

  p_tbl_df = multiverse_table(M) %>% select(-.code, -.results)

  p_tbl_df.ref <- list(
    menstrual_calculation = list("mc_option1", "mc_option2", "mc_option3"),
    relationship_status = list("rs_option1", "rs_option2", "rs_option3"),
    cycle_length = list("cl_option1", "cl_option2", "cl_option3"),
    certainty = list("cer_option1", "cer_option2")
  ) %>%
    expand.grid(KEEP.OUT.ATTRS = FALSE) %>%
    unnest( cols = everything() ) %>%
    mutate( .universe = seq(1:nrow(.)) ) %>%
    select(.universe, everything())

  param.assgn = lapply(seq_len(nrow(p_tbl_df.ref)), function(i) lapply(select(p_tbl_df.ref, -.universe), "[[", i))
  p_tbl_df.ref <- p_tbl_df.ref %>% mutate(.parameter_assignment = param.assgn)

  # comparing nested tibbles doesn't work correctly, but can convert to lists
  # to make the comparison work. See https://stackoverflow.com/a/56220028
  expect_equal(as.list(p_tbl_df), as.list(p_tbl_df.ref))
})

test_that("`parse_multiverse` creates an empty data.frame for the 'multiverse_tbl' slot when it is passed an expression without any branches", {
  p_tbl_df.ref = tibble::tibble(
    .parameter_assignment = list( list() ),
    .code = list( rlang::expr( df <- data.frame(x = 1:10) ) )
  )

  M = multiverse()
  # this should NOT generate a warning
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr( df <- data.frame(x = 1:10) ), execute = FALSE)
  p_tbl_df = multiverse_table(M) %>% select(-.results)

  expect_equal( as.list(p_tbl_df), as.list(p_tbl_df.ref) )
})

test_that("`parse_multiverse` works when conditions are specified", {
  p_tbl_df.ref = list(
    values_y = list("TRUE", "FALSE"),
    values_z = list("constant", "linear", "sum")
  ) %>%
  expand.grid(KEEP.OUT.ATTRS = FALSE)  %>%
  unnest( cols = everything() ) %>%
  mutate( .universe = seq(1:nrow(.)) ) %>%
  select(.universe, everything()) %>%
  filter( values_z != "sum" | values_y == TRUE )

  M <- multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
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
  }), execute = FALSE)

  p_tbl_df = multiverse_table(M) %>% select( -.parameter_assignment, -.code, -.results )
  expect_equal( as.list(p_tbl_df), as.list(p_tbl_df.ref) )
})

test_that("`parse_multiverse` requires multiple uses of the same paramater to cover all options", {
  M = multiverse()
  expect_error(add_and_parse_code(attr(M, "multiverse"), execute = FALSE, expr({
    some_var = branch(parameter,
      "option1" ~ expr1,
      "option2" ~ expr2
    )

    some_other_var = branch(parameter,
      "option1" ~ expr4
    )
  })))
})

# parameter_assigment -----------------------------------------------------

test_that("`parameter_assignment` is created appropriately for single parameter multiverses", {
  ref_list = lapply(c("mc_option1", "mc_option2", "mc_option3"), function(x) list(menstrual_calculation = x))

  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = branch(menstrual_calculation,
                                          "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                          "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                          "mc_option3" ~ StartDateNext)
      )
  }), execute = FALSE)

  m.tbl = multiverse_table(M)

  expect_equal(m.tbl$.parameter_assignment, ref_list)
})

test_that("`parameter_assignment` is created appropriately for two or more parameter multiverses", {
  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = branch(menstrual_calculation,
                                          "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                          "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                          "mc_option3" ~ StartDateNext)
      ) %>%  filter( branch(certainty, "cer_option1" ~ TRUE, "cer_option2" ~ Sure1 > 6 | Sure2 > 6 ))
  }), execute = FALSE)

  m.list = multiverse_table(M)$.parameter_assignment

  ref_list = expand.grid(
    menstrual_calculation = list("mc_option1", "mc_option2", "mc_option3"),
    certainty = list("cer_option1", "cer_option2")
  ) %>%
    transpose()

  expect_equal( m.list, ref_list )
})

test_that("unnamed options in branches are supported", {
    df = data.frame( y = 1:10 )

    M.1 = multiverse()
    inside(M.1, {
      df = mutate(df, x = branch(
            values_x,
            0,
            3,
            "abc",
            y + 5
        ))
    })
    M.tbl.1 = multiverse_table(M.1) %>% select(-.parameter_assignment, -.code, -.results)

    M.2 = multiverse()
    inside(M.2, {
      df = mutate(df, x = branch(
            values_x,
            "0" ~ 0,
            "3" ~ 3,
            "abc" ~ "abc",
            "y + 5" ~ y + 5
        ))
    })
    M.tbl.2 = multiverse_table(M.2) %>% select(-.parameter_assignment, -.code, -.results)
    expect_equal( as.list(M.tbl.1), as.list(M.tbl.2) )
})

test_that("conditions are extracted when specified using `branch_assert`", {
  M = multiverse()
  add_and_parse_code(attr(M, "multiverse"), attr(M, "multiverse_super_env"), expr({
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate(NextMenstrualOnset = branch(menstrual_calculation,
                                         "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                         "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                         "mc_option3" ~ StartDateNext)
      ) %>%
      mutate(Relationship = branch( relationship_status,
                                    "rs_option1" ~ factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')),
                                    "rs_option2" ~ factor(ifelse(Relationship==1, 'Single', 'Relationship')),
                                    "rs_option3" ~ factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) )
      ) %>%
      mutate(
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      dplyr::filter( branch(cycle_length,
                            "cl_option1" ~ TRUE,
                            "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                            "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      dplyr::filter( branch(certainty,
                            "cer_option1" ~ TRUE,
                            "cer_option2" ~ Sure1 > 6 | Sure2 > 6
      )) %>%
      mutate( Fertility = branch( fertile,
                                  "fer_option1" ~ factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", "medium")) ),
                                  "fer_option2" ~ factor( ifelse(CycleDay >= 6 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 27, "low", "medium")) ),
                                  "fer_option3" ~ factor( ifelse(CycleDay >= 9 & CycleDay <= 17, "high", ifelse(CycleDay >= 18 & CycleDay <= 25, "low", "medium")) ),
                                  "fer_option4" ~ factor( ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low") )
      )) %>%
      branch_assert(cycle_length != "cl_option2" | menstrual_calculation == "mc_option2") %>%
      branch_assert(relationship_status != "rs_option3" | menstrual_calculation == "mc_option1") %>%
      branch_assert(fertile != "fer_option4" | certainty == "cer_option2")
  }),  execute = FALSE)

  cond.ref = list(
    expr(cycle_length != "cl_option2" | menstrual_calculation == "mc_option2"),
    expr(relationship_status != "rs_option3" | menstrual_calculation == "mc_option1"),
    expr(fertile != "fer_option4" | certainty == "cer_option2")
  )

  expect_equal(conditions(M), cond.ref)
})
