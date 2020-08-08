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

switch_expr <- function(x, ...) {
  switch(expr_type(x),
         ...,
         stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

safe_f_rhs <- purrr::safely(f_rhs)

safe_f_lhs <- purrr::safely(f_lhs)

get_code_universe <- function(.m_list, .uni, .level) {
  if (.level >= 1){
    .p <- .m_list[[.level]][[.uni]]$parent
    c(get_code_universe(.m_list, .p, .level - 1), .m_list[[.level]][[.uni]]$code)
  }
}

some_function <- function() stop("throw errors at me")