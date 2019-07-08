# Tests for multiverse object
context("multiverse")

library(dplyr)
library(lubridate)

M = multiverse()
M.obj = attr(M, "multiverse")

test_that("user facing multiverse object is assigned proper class", {
  expect_true(is(M, "multiverse"))
})

test_that("identify multiverse object using 'inherits' function", {
  expect_equal(is_multiverse(M), TRUE)
  expect_equal(is.multiverse(M), TRUE)
})

test_that("new multiverse object is initialised properly", {
  expect_null(M.obj[['code']])
  expect_null(M.obj[['default_parameter_assignment']])
  expect_warning( expect_mapequal( M.obj[['parameters']], list()) )
  expect_warning( expect_mapequal( M.obj[['conditions']], list()) )
  expect_true( is.data.frame(M.obj[['multiverse_table']]) )
  expect_equal( nrow(M.obj[['multiverse_table']]), 0 )
})

test_that("accessor functions work on newly initialised object", {
  expect_null(code(M))
  expect_null(default_parameter_assignment(M))
  expect_warning( expect_mapequal( parameters(M), list()) )
  expect_warning( expect_mapequal( conditions(M), list()) )
  expect_true( is.data.frame(multiverse_table(M)) )
  expect_equal( nrow(multiverse_table(M)), 0 )
})

# accessor functions ------------------------------------------------

M.2 <- multiverse()

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

inside(M.2, {
  df <- test_df %>%
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
    )
})

test_that("accessor functions for getting default code", {
  ref_code = expr({
    df <- test_df  %>%
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
      )
  })
  
  expect_true( is.language(code(M.2)) )
  expect_equal( code(M.2), ref_code )
})

test_that("accessor function for parameter list", {
  ref_list = list(
    menstrual_calculation = list("mc_option1", "mc_option2", "mc_option3"),
    relationship_status = list("rs_option1", "rs_option2", "rs_option3")
  )
  
  expect_true( is.list(parameters(M.2)) )
  expect_mapequal( parameters(M.2), ref_list )
})

# test_that("accessor function for conditions list", {  })

test_that("accessor functions get default parameter assignment", {
  ref_list = list(menstrual_calculation = "mc_option1", relationship_status = "rs_option1")
  
  expect_true( is.list(default_parameter_assignment(M.2)) )
  expect_mapequal( default_parameter_assignment(M.2), ref_list )
})

test_that("accessor functions retrieve the multiverse table", {
  ref_expr = expr({
    df <- test_df  %>%
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
      )
  })
  
  ref_list = list(
    menstrual_calculation = list("mc_option1", "mc_option2", "mc_option3"),
    relationship_status = list("rs_option1", "rs_option2", "rs_option3")
  )
  
  ref_df = expand.grid(ref_list)
  
  param.assgn = lapply(seq_len(nrow(ref_df)), function(i) lapply(ref_df, "[[", i)) 
  
  ref_df = ref_df %>%
    mutate(
      .parameter_assignment = param.assgn,
      .code = map(.parameter_assignment, ~ get_parameter_code(ref_expr, .x))
    ) %>%
    as_tibble()
  
  df = multiverse_table(M.2) %>% select(-.results)
  
  expect_true( tibble::is_tibble(multiverse_table(M.2)) )
  expect_equal( as.list(ref_df), as.list(df) )
})



