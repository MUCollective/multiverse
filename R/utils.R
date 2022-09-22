setClassUnion("listORnumeric", c("list", "numeric"))

expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

create_name_from_expr <- function(x, as.str = FALSE) {
  if (rlang::is_syntactic_literal(x)) {
    if (as.str) {
      as.character(x)
    } else {
      x
    }
  } else {
    expr_text(x)
  }
}

safe_f_rhs <- purrr::safely(f_rhs)

safe_f_lhs <- purrr::safely(f_lhs)

# returns code for a specific universe upto a certain depth
# in the tree of nodes, starting from the root node
# eg. for a tree of depth 4 (4 branches each in distinct inside() calls or multiverse code chunks), 
# if .level = 2, only the code upto the first two branches
# will be returned
get_code_universe <- function(.m_list, .uni, .level) {
  if (.level >= 1) {
    .p <- .m_list[[.level]][[.uni]]$parent
    c(get_code_universe(.m_list, .p, .level - 1), .m_list[[.level]][[.uni]]$code)
  }
}


# returns errors/warnings/messages for a specific universe upto a certain depth
# in the tree of nodes, starting from the root node
# eg. for a tree of depth 4 (4 branches each in distinct inside() calls or multiverse code chunks), 
# if .level = 2, only the errors/warnings/messages upto the first two branch calls
# will be returned
get_error_universe <- function(.m_list, .uni, .level) {
  if (.level >= 1) {
    .p <- .m_list[[.level]][[.uni]]$parent
    error = .m_list[[.level]][[.uni]]$error
    c(get_error_universe(.m_list, .p, .level - 1), ifelse(is.null(error), NA, error))
  }
}


# functions for properly formatting code
# entered into the multiverse for printing
add_newline_after_branch_options = function(pd_flat) {
  if (pd_flat$text[[1]] == 'branch') {
    option_after <- pd_flat$token == "','"
    if (!any(option_after)) {
      return(pd_flat)
    }
    pd_flat$newlines[option_after] <- 1L
    pd_flat$lag_newlines[lag(option_after)] <- 1L
  }
  return(pd_flat)
}

add_newline_around_branch_parens = function(pd_flat) {
  if (pd_flat$text[[1]] == 'branch') {
    open_paren <- pd_flat$token == "'('"
    close_paren <- pd_flat$token == "')'"
    if (! (any(open_paren) | any(close_paren))) {
      return(pd_flat)
    }
    pd_flat$newlines[open_paren] <- 1L
    pd_flat$newlines[lead(close_paren)] <- 1L
    
    pd_flat$lag_newlines[lag(open_paren)] <- 1L
    pd_flat$lag_newlines[close_paren] <- 1L
  }
  return(pd_flat)
}

multiverse_branch_style <- function() {
  styler::create_style_guide(
    line_break = tibble::lst(add_newline_after_branch_options, add_newline_around_branch_parens),
    style_guide_name = "multiverse-style",
    style_guide_version = "some-version"
  )
}

style_multiverse_code = function(x) {
  styler::style_text(
    styler::style_text(
      paste0(deparse(x[[2]]), collapse = ""),
      transformers = multiverse_branch_style()
    )
  )
}