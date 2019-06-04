
context("parse-multiverse")

library(rlang)

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

test_that("reduction function which combines two lists work", {
   l.1 = list(parameters = 1:3, conditions = letters[1:3])
   l.2 = list(parameters = 4:6, conditions = letters[4:6])
   l = list(parameters = 1:6, conditions = letters[1:6])
  
  expect_equal(combine_parameter_conditions(l.1, l.2), l)
})

test_that("`get_parameter_conditions` returns an empty list when input expression has no branch", {
  an_expr = expr({
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  })
  
  expect_equal(flatten(get_parameter_conditions(an_expr)), list())
})

test_that("`get_parameter_conditions` returns the correct output (list) when input expression has a single branch", {
  an_expr = expr({
    df <- data.raw.study2  %>%
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
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate(NextMenstrualOnset = branch(mentrual_calculation, 
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
