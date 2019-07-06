# Tests to check if the single universe syntax tree can be extracted from the multiverse
context("get_code")

library(dplyr)
library(lubridate)

make_data <- function(nrow = 500) {
  data.frame(
    Relationship = sample(1:4, nrow, replace = TRUE),
    Sure1 = sample(1:9, nrow, replace = TRUE),
    Sure2 = sample(1:9, nrow, replace = TRUE),
    StartDateofLastPeriod = make_date(2012, sample(4:5, nrow, TRUE), sample(1:22, nrow, TRUE)),
    DateTesting = make_date(2012, 5, sample(21:26, nrow, TRUE))
  ) %>%
    mutate(
      StartDateofPeriodBeforeLast = StartDateofLastPeriod - sample(20:28, nrow, TRUE),
      StartDateNext = StartDateofLastPeriod - sample(20:28, nrow, TRUE),
      ReportedCycleLength = sample(14:28, nrow, TRUE)
    )
}
test_df = make_data()

M = multiverse()

inside(M, {
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
})

test_that("throws error if multiverse object passed is not R6", {
  param_assgn = 2
  
  expect_error( M %>% get_code(param_assgn) )
})

test_that("syntax tree without branches is correctly returned", {
  an_expr <- expr({df <- test_df  %>%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )})
  
  M.no_branch = multiverse()
  
  inside(M.no_branch, {
    df <- test_df %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  })
  
  expect_equal(f_rhs(code(M.no_branch)), f_rhs(an_expr))
})

test_that("syntax tree with branches is correctly returned when no parameter is assigned", {
  u.expr = attr(M, "multiverse") %>% get_code()
  
  u.expr.ref = expr({
    df <- test_df  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength ) %>%
      mutate( Relationship = factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')) ) %>%
      mutate( 
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      dplyr::filter( TRUE ) %>%
      dplyr::filter( TRUE ) %>%
      mutate( Fertility = factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", "medium")) ) )
  })
  
  expect_equal(f_rhs(u.expr), f_rhs(u.expr.ref))
})

test_that("syntax tree with branches is correctly returned when a parameter is assigned", {
  param.assgn <- list(
    menstrual_calculation = "mc_option1",
    relationship_status = "rs_option3",
    cycle_length = "cl_option2",
    certainty = "cer_option1",
    fertile = "fer_option4"
  )
  
  u.expr = code(M) %>% get_parameter_code(param.assgn)
  
  u.expr.ref = expr({
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
  })
  
  expect_equal(f_rhs(u.expr), f_rhs(u.expr.ref))
})
