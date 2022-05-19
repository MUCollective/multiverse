#' Define multiple analysis paths for a step in the multiverse
#' 
#' The \code{branch} function allows the user to define multiple analysis options for a particular step
#' in the analysis.
#' 
#' @details For every step in the analysis, there may be more than one analysis option. 
#' We use \code{branch} to declare these different analysis options. Each branch is
#' characterised by a parameter. The first argument passed into the branch is the parameter.
#' 
#' All the other arguments passed into branch are the different analysis options corresponding 
#' to that parameter (that particular step in the analysis process). Naturally, at least two 
#'or more options should be declared. Thus, the branch function will provide a warning
#' if the total number arguments passed is less than three.
#' 
#' Please refer to \code{vignette("branch")} for more details on how to use this function to 
#' create a complete multiverse analysis.
#' 
#' @param parameter A string to identify the branch. Each branch is characterised using a
#' parameter which takes different options.
#'  
#' @param ... Different options for completing a particular step in the analysis. Each option is
#' declared as <option_name> ~ <option_calculation>. See examples for more details.
#' 
#' @param .options Declare a continuous value as the option of a parameter using a sequence 
#' (see examples for details), and the expanded sequence will be included as options for that
#' parameter in the multiverse. 
#'  
#' @examples 
#' \donttest{
#' library(dplyr)
#' 
#' # Example 1: declaring multiple options for a data processing step 
#' set.seed(123)
#' x = rnorm(100, 30, 10)
#' 
#' # Say that you have a variable, x. You want to discretise this variable into two ordinal 
#' # categories — high (if x >= 30) and low (if x < 30). However, another researcher might argue 
#' # for discretising this variable into three ordinal categories — high (if x >= 40), 
#' # medium (if 20 <= x < 40), and low (if x < 20).
#' 
#' M.1 = multiverse() # create a new multiverse object
#' 
#' inside(M.1, {
#' y = branch(discretisation, 
#'         "two_levels" ~ ifelse(x < 30, "low", "high"),
#'         "three_levels" ~ ifelse(x < 20, "low", ifelse(x > 40, "high", "medium"))
#'     )
#' })
#' 
#' 
#' # Example 2: using branch with tidyverse and `%>%`
#' # Let’s say that we have some data which indicates the amount of time spent by a user
#' # in four different conditions which are indexed 1, 2, 3 and 4
#' # (the modality column in the following dataset). 
#' # We will first load the data and convert the column into factor from integer.
#' 
#' data("userlogs")
#' data.userlogs.raw = userlogs %>%
#'     mutate( modality = factor(modality) ) %>%
#'     arrange( modality )
#'     
#' M.2 = multiverse() # create a new multiverse object
#' 
#' inside(M.2, {
#'     df = data.userlogs.raw %>%
#'         select(modality, duration) %>%
#'         mutate( duration = branch( data_transform, 
#'                             "none" ~ duration,
#'                             "log" ~ log(duration)))
#' })                             
#' 
#' 
#' # Example 3: using branch with tidyverse and `%>%`
#' # Consider a scenario where there are more than one alternatives to
#' # identifying and removing outliers
#' 
#' data("hurricane")
#' 
#' M.3 = multiverse()
#' 
#' # here, we perform a `filter` operation in the multiverse
#' inside(M.3, {
#'     df.filtered = hurricane %>%
#'         filter(branch(death_outliers,
#'                    "no_exclusion" ~ TRUE,
#'                    "most_extreme" ~ name != "Katrina",
#'                    "two_most_extreme" ~ !(name %in% c("Katrina", "Audrey"))
#'     ))
#' })
#' 
#' 
#' # Example 4: using branch as a function
#' # An alternate way of implementing the `branch()` function from Example 2 may be:
#' 
#' M.4 = multiverse()
#' 
#' inside(M.4, {
#'     duration_transform = branch(data_trans,
#'         "log-transformed" ~ log,
#'         "un-transformed" ~ identity
#'     )
#' 
#'     duration = duration_transform(data.userlogs.raw$duration)
#' })
#' 
#' 
#' 
#' # Example 5: continuous option values for a parameter
#' 
#' M.5 = multiverse()
#' inside(M.5, {
#'   branch(foo, "option1" ~ 1, .options = 1:10)
#' })
#'
#' M.6 = multiverse()
#' # alternatively, we could specify how we want the vector to be expanded
#' # for continuous parameters
#' inside(M.6, {
#'   branch(foo, "option1" ~ 1, .options = seq(0, 1, by = 0.1))
#' })
#' 
#' }
#' 
#' @name branch
NULL
