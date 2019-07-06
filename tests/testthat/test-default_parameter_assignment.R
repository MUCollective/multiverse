context("default_parameter_assignment")

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


test_that("default_parameter_assignment retrieves the first row of the multiverse table", {
    ref_assgn = list(
      menstrual_calculation = "mc_option1", 
      relationship_status = "rs_option1",
      cycle_length = "cl_option1",
      certainty = "cer_option1",
      fertile = "fer_option1"
    )
    
    assgn = default_parameter_assignment(M)
    
    expect_equal(ref_assgn, assgn)
})

test_that("default_parameter_assignment accepts a numeric value and updates the default for the multiverse", {
  ref_assgn = list(
    menstrual_calculation = "mc_option3", 
    relationship_status = "rs_option2",
    cycle_length = "cl_option1",
    certainty = "cer_option1",
    fertile = "fer_option3"
  )
  
  default_parameter_assignment(M) <- 114
  assgn = default_parameter_assignment(M)
  
  expect_equal(ref_assgn, assgn)
})

test_that("default_parameter_assignment accepts a list and updates the default for the multiverse", {
  ref_assgn = list(
    menstrual_calculation = "mc_option1", 
    relationship_status = "rs_option1",
    cycle_length = "cl_option1",
    certainty = "cer_option1",
    fertile = "fer_option1"
  )
  
  default_parameter_assignment(M) <- ref_assgn
  assgn = default_parameter_assignment(M)
  
  expect_equal(ref_assgn, assgn)
})




