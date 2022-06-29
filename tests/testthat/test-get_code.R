# Tests to check if the single universe syntax tree can be extracted from the multiverse
context("get_code")

library(dplyr)

set.seed(123)
make_data <- function(nrow = 500) {
  data.frame(
    Relationship = sample(1:4, nrow, replace = TRUE),
    Sure1 = sample(1:9, nrow, replace = TRUE),
    Sure2 = sample(1:9, nrow, replace = TRUE),
    StartDateofLastPeriod = as.Date(ISOdate(2012, sample(4:5, nrow, TRUE), sample(1:22, nrow, TRUE))),
    DateTesting = as.Date(ISOdate(2012, 5, sample(21:26, nrow, TRUE)))
  ) %>%
    mutate(
      StartDateofPeriodBeforeLast = StartDateofLastPeriod - sample(20:28, nrow, TRUE),
      StartDateNext = StartDateofLastPeriod - sample(20:28, nrow, TRUE),
      ReportedCycleLength = sample(14:28, nrow, TRUE)
    )
}

test_df <- make_data()

M = multiverse()
an_expr = quote(
  df <- test_df  %>%
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
    ))
)

inside(M, {!!an_expr} )

# get_option_name ----------------------------------
test_that("can extract the option names from a branch", {
  an_expr.1 = quote(
    branch(values,
           "zero" ~ 0,
           "three" ~ 3,
           "null" ~ NULL,
           "y + 5" ~ y + 5
  ))

  an_expr.2 = quote(
    branch(cycle_length,
           "no_filter" ~ TRUE,
           "computed_25to35" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
           "reported_25to35" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
  ))

  an_expr.3 = quote(branch(value_x, "a", 1, x + 1, x^2, TRUE))

  lgl.1.ref = list("zero", "three", "null", "y + 5")
  lgl.2.ref = list("no_filter", "computed_25to35", "reported_25to35")
  lgl.3.ref = list("a", "1", "x + 1", "x^2", "TRUE")

  expect_equal(map(an_expr.1[-1:-2], ~ get_option_name(.x)), lgl.1.ref)
  expect_equal(map(an_expr.2[-1:-2], ~ get_option_name(.x)), lgl.2.ref)
  expect_equal(map(an_expr.3[-1:-2], ~ get_option_name(.x)), lgl.3.ref)
})

test_that("exact matching occurs when extracting options_names from a assignment", {
  an_expr = quote(branch( value_x, 0, 10, 12, 30, 23, 40))
  compute_branch(an_expr, list(value_x = 0))

  expect_equal(compute_branch(an_expr, list(value_x = 0)), 0 )
})

# get_option_value ----------------------------------
test_that("can identify the correct option index from a branch", {
  an_expr.1 = quote(
    branch(values,
           "zero" ~ 0,
           "three" ~ 3,
           "null" ~ NULL,
           "y + 5" ~ y + 5
    ))
  an_expr.2 = quote(
    branch(cycle_length,
           "no_filter" ~ TRUE,
           "computed_25to35" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
           "repored_25to35" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
    ))

  parameter_values.1 = an_expr.1[-1:-2]
  parameter_values.2 = an_expr.2[-1:-2]

  idx.1 = 3
  idx.2 = 2

  value.1 = parameter_values.1 %>%
    extract2(idx.1) %>%
    get_option_value()

  value.2 = parameter_values.2 %>%
    extract2(idx.2) %>%
    get_option_value()

  expect_equal(value.1, as.list(an_expr.1)[[2 + idx.1]][[3]])
  expect_equal(value.2, as.list(an_expr.2)[[2 + idx.2]][[3]])
})

# compute_branch ----------------------------------
test_that("given parameter assignment, an expression with branch is converted into proper R code for execution", {
  an_expr.1 = quote(
    branch(menstrual_calculation,
           "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
           "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
           "mc_option3" ~ StartDateNext)
  )
  an_expr.2 = quote( branch(cycle_length,
                           "cl_option1" ~ TRUE,
                           "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                           "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
  ))

  .assgn_list = list(
    menstrual_calculation = "mc_option1",
    relationship_status = "rs_option3",
    cycle_length = "cl_option3",
    certainty = "cer_option2",
    fertile = "fer_option4"
  )

  .code.1 = compute_branch( an_expr.1, .assgn_list )
  .code.2 = compute_branch( an_expr.2, .assgn_list )
  .code_ref.1 = quote(StartDateofLastPeriod + ComputedCycleLength)
  .code_ref.2 = quote(ReportedCycleLength > 25 & ReportedCycleLength < 35)
  expect_equal(.code.1, .code_ref.1)
  expect_equal(.code.2, .code_ref.2)
})

# get_code ----------------------------------
test_that("throws error if multiverse object passed is not R6", {
  param_assgn = 2

  expect_error( M %>% get_code(param_assgn) )
})

test_that("syntax tree without branches is correctly returned", {
  an_expr <- list( `1` = quote({df <- test_df  %>%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )}) )

  M.no_branch = multiverse()

  add_and_parse_code(M.no_branch, quote({
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  }))

  expect_equal(code(M.no_branch), an_expr)
})

test_that("syntax tree for each universe is computed correctly", {
  M2 = multiverse()
  
  inside(M2, {
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = branch(menstrual_calculation, 
                                          "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                          "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                          "mc_option3" ~ StartDateNext)
      ) %>%
      filter( branch(certainty,
                     "cer_option1" ~ TRUE,
                     "cer_option2" ~ Sure1 > 6 | Sure2 > 6
      ))
  })
  
  u.expr.ref.1 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>% 
      filter(TRUE)
  })
  u.expr.ref.2 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>% 
      filter(Sure1 > 6 | Sure2 > 6)
  })
  u.expr.ref.3 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>% 
      filter(TRUE)
  })
  u.expr.ref.4 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateofLastPeriod + ReportedCycleLength) %>% 
      filter(Sure1 > 6 | Sure2 > 6)
  })
  u.expr.ref.5 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateNext) %>% 
      filter(TRUE)
  })
  u.expr.ref.6 = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>% 
      mutate(NextMenstrualOnset = StartDateNext) %>% 
      filter(Sure1 > 6 | Sure2 > 6)
  })
  
  expect_equal( expand(M2)$.code[[1]], list("1" = u.expr.ref.1) )
  expect_equal( expand(M2)$.code[[2]], list("1" = u.expr.ref.2) )
  expect_equal( expand(M2)$.code[[3]], list("1" = u.expr.ref.3) )
  expect_equal( expand(M2)$.code[[4]], list("1" = u.expr.ref.4) )
  expect_equal( expand(M2)$.code[[5]], list("1" = u.expr.ref.5) )
  expect_equal( expand(M2)$.code[[6]], list("1" = u.expr.ref.6) )
})

# rm_branch_assert ----------------------------------------------------
## write test cases for rm_branch_assert which is called from get_code
test_that("syntax tree with `%when% conditional` is returned", {
  expr.1 <- expr({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate(NextMenstrualOnset = branch(menstrual_calculation,
                                         "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                         "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                         "mc_option3" ~ StartDateNext)
      ) %>%
      filter( branch(cycle_length,
                     "cl_option1" ~ TRUE,
                     "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                     "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      branch_assert(cycle_length != "cl_option2" | menstrual_calculation == "mc_option2")
  })
  
  expr.2 <- expr({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate(NextMenstrualOnset = branch(menstrual_calculation,
                                         "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength ,
                                         "mc_option2" ~ (StartDateofLastPeriod + ReportedCycleLength) %when% (cycle_length != "cl_option2"),
                                         "mc_option3" ~ StartDateNext)
      ) %>%
      filter( branch(cycle_length,
                     "cl_option1" ~ TRUE,
                     "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                     "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      ))
  })
  
  .assgn <- list("menstrual_calculation" = "mc_option1", "cycle_length" = "cl_option1")
  
  parsed_expr.1 <- get_parameter_code(expr.1, .assgn)
  parsed_expr.2 <- get_parameter_code(expr.2, .assgn)
    
    
  ref_expr <- expr({
    df <- test_df %>% 
      mutate(ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast) %>%
      mutate(NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength) %>% 
      filter(TRUE)
  })
  
  expect_equal( parsed_expr.1, ref_expr)
  expect_equal( parsed_expr.2, ref_expr)
})

# get_parameter_code ----------------------------------

test_that("syntax tree with branches is correctly returned when a parameter is assigned", {
  param.assgn <- list(
    menstrual_calculation = "mc_option1",
    relationship_status = "rs_option3",
    cycle_length = "cl_option2",
    certainty = "cer_option1",
    fertile = "fer_option4"
  )

  u.expr = lapply(code(M), get_parameter_code, param.assgn)

  u.expr.ref = list(`1` = quote({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength ) %>%
      mutate( Relationship = factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) ) %>%
      mutate(
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      dplyr::filter( ComputedCycleLength > 25 & ComputedCycleLength < 35 ) %>%
      dplyr::filter( TRUE ) %>%
      mutate( Fertility = factor( ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low") ) )
  }))

  expect_equal(u.expr, u.expr.ref)
})

test_that("is able to handle missing values passed as index to lists / df / matrices", {
  .expr <- expr({
    x <- branch( zero_or_one, 0, 1)
    
    y <- matrix(rnorm(16, 0, 5), 8, 2)
    z <- y[,1]
  })
  
  .l_output <- lapply(.expr, get_parameter_code, list(zero_or_one = "0"))
  .l_ref <- list( quote(`{`), quote(x <- 0), quote(y <- matrix(rnorm(16, 0, 5), 8, 2)), quote(z <- y[,1]) )
  
  expect_equal(.l_output, .l_ref)
})



