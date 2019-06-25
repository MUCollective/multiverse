# Tests to check if the single universe syntax tree can be extracted from the multiverse
context("get_code")

test_that("syntax tree without branches is correctly returned", {
  an_expr <- expr({df <- data.raw.study2  %>%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )})
  
  M = new("multiverse")
  M = inside(M, {
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast )
  })
  
  expect_equal(f_rhs(get_code(M)), f_rhs(an_expr))
})

test_that("syntax tree with branches is correctly returned when no parameter is assigned", {
  M = new("multiverse")
  
  M = inside(M, {
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
      mutate(
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      filter( branch(cycle_length, 
                     "cl_option1" ~ TRUE,
                     "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                     "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      filter( branch(certainty,
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
  u.expr = parse_multiverse(M) %>%
              get_code()
  
  u.expr.ref = expr({
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength ) %>%
      mutate( Relationship = factor(ifelse(Relationship==1 | Relationship==2, 'Single', 'Relationship')) ) %>%
      mutate( 
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      filter( TRUE ) %>%
      filter( TRUE ) %>%
      mutate( Fertility = factor( ifelse(CycleDay >= 7 & CycleDay <= 14, "high", ifelse(CycleDay >= 17 & CycleDay <= 25, "low", "medium")) ) )
  })
  
  expect_equal(f_rhs(u.expr), f_rhs(u.expr.ref))
})

test_that("syntax tree with branches is correctly returned when a parameter is assigned", {
  M = new("multiverse")
  
  M = inside(M, {
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
      mutate(
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      filter( branch(cycle_length, 
                     "cl_option1" ~ TRUE,
                     "cl_option2" ~ ComputedCycleLength > 25 & ComputedCycleLength < 35,
                     "cl_option3" ~ ReportedCycleLength > 25 & ReportedCycleLength < 35
      )) %>%
      filter( branch(certainty,
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
  
  param.assgn <- list(
    mentrual_calculation = "mc_option1",
    relationship_status = "rs_option3",
    cycle_length = "cl_option2",
    certainty = "cer_option1",
    fertile = "fer_option4"
  )
  
  u.expr = parse_multiverse(M) %>%
    get_code(param.assgn)
  
  u.expr.ref = expr({
    df <- data.raw.study2  %>%
      mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) %>%
      mutate( NextMenstrualOnset = StartDateofLastPeriod + ComputedCycleLength ) %>%
      mutate( Relationship = factor(ifelse(Relationship==1, 'Single', ifelse(Relationship==3 | Relationship==4, 'Relationship', NA))) ) %>%
      mutate( 
        CycleDay = 28 - (NextMenstrualOnset - DateTesting),
        CycleDay = ifelse(CycleDay > 1 & CycleDay < 28, CycleDay, ifelse(CycleDay < 1, 1, 28))
      ) %>%
      filter( ComputedCycleLength > 25 & ComputedCycleLength < 35 ) %>%
      filter( TRUE ) %>%
      mutate( Fertility = factor( ifelse(CycleDay >= 8 & CycleDay <= 17, "high", "low") ) )
  })
  
  expect_equal(f_rhs(u.expr), f_rhs(u.expr.ref))
})
