#' Stylises the code of multiverse for printing
#'
#' Stylises the code declared with a multiverse object according to the tidyverse style guide.
#'
#' @details Since multiverse captures unevaluated expressions, the formatting is not preserved. 
#' However, if we want to view the code that is delcared for a particular multiverse analysis, the
#' lack of formatting would lead to difficulty in readability. `style_multiverse_code` is 
#' primarily intended for internal use to stylise the output based on the tidyverse style guide
#' and custom rules necessary for multiverse.
#'
#' @param .code a quoted (or unevaluated) expression surrounded; usually the expression within 
#' a single multiverse code block or inside function.
#'
#' @return an object of class vertical
#'
#' @importFrom styler create_style_guide
#' @importFrom styler style_text
#' 
#' @rdname stylise_code
#' @export
style_multiverse_code = function(.code) {
  lapply(.code[2:length(.code)], function(x) {
    style_text(
      style_text(
        paste0(deparse(x), collapse = ""),
        transformers = multiverse_branch_style()
      )
    )
  })
}

# functions for properly formatting code
# entered into the multiverse for printing
add_newline_after_branch_options = function(pd_flat) {
  if (pd_flat$text[[1]] == 'branch') {
    # print(pd_flat)
    option_after <- pd_flat$token == "','"
    if (!any(option_after)) {
      return(pd_flat)
    }
    pd_flat$newlines[option_after] <- 1L
    pd_flat$lag_newlines[lag(option_after)] <- 1L
  }
  return(pd_flat)
}

## break long line functions
break_long_fun_arguments = function(pd_flat) {
  # print(pd_flat)
  is_call <- pd_flat$token_before[2] == "SYMBOL_FUNCTION_CALL"
  if (! is.na(is_call) & is_call) {
    if (sum(nchar(pd_flat$text)) > 80) {
      option_after <- pd_flat$token == "','"
      if (!any(option_after)) {
        return(pd_flat)
      }
      pd_flat$newlines[option_after] <- 1L
      pd_flat$lag_newlines[lag(option_after)] <- 1L
    }
  }
  return(pd_flat)
}

multiverse_branch_style <- function() {
  create_style_guide(
    line_break = tibble::lst(
      add_newline_after_branch_options, 
      break_long_fun_arguments
    ),
    style_guide_name = "multiverse-style",
    style_guide_version = "version-0.1"
  )
}