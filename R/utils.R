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