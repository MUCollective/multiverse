#' Accessing contents of the multiverse object
#'
#' @description A multiverse object contains several \strong{Object variables}. These can be accessed using convenient functions.
#' Variables from the analysis that is being performed within the multiverse can be accessed using the `$`.
#' Object variables such as the `code`, the `expanded parameter options table`, the `parameters` and the `conditions` can be accessed using respective functions

#' @name accessors
#' @param multiverse Object of class multiverse
#' @param name a variable name
#' @param value a new value to be assigned
#'
#'@importFrom dplyr select
#'
#' @export
`$.multiverse` <- function(multiverse, name) {
  .idx = 1
  m_obj = attr(multiverse, "multiverse")
  .env <- unlist(unname(tail(attr(multiverse, "multiverse")$multiverse_diction$as_list(), n = 1)), recursive = FALSE)[[1]]$env
  
  get(as.character(name), .env)
}

#' @rdname accessors
#' @export
`$<-.multiverse` <- function(multiverse, name, value) {
  stop(
    "cannot assign to objects in the multiverse. 
    The `$` can only be used to extract objects from the default analysis of the multiverse"
  )
}

#' @rdname accessors
#' @export
expand <- function(multiverse) {
  UseMethod("expand")
}

#' @rdname accessors
#' @export
expand.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `expand`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
expand.multiverse <- function(multiverse) {
  .m_obj = attr(multiverse, "multiverse")
  .m_list = .m_obj$multiverse_diction$as_list()
  
  df <- data.frame( lapply(expand.grid(rev(.m_obj$parameters), KEEP.OUT.ATTRS = FALSE), unlist), stringsAsFactors = FALSE ) %>%
    select(names(.m_obj$parameters))
  
  if (length(.m_obj$conditions) > 0) {
    all_conditions <- parse_expr(paste0("(", .m_obj$conditions, ")", collapse = "&"))
  } else {
    all_conditions <- expr(TRUE)
  }
  
  if (nrow(df) == 0) {
    n <- 1
    param.assgn =  list(list())
    .code = list(code(multiverse))
    if (length(.m_list) == 0) {
      .res = list(list())
    } else {
      .res = lapply( unlist(unname(tail(.m_list, n = 1)), recursive = FALSE), `[[`, "env" )
    }
    df <- tibble(.universe = seq(1:n))
  } else {
    df <- filter(df, eval(all_conditions))
    n <- nrow(df)
    param.assgn =  lapply(seq_len(n), function(i) lapply(df, "[[", i))
    .code = lapply(seq_len(n), get_code_universe, .m_list = .m_list, .level = length(.m_list))
    .res = lapply( unlist(unname(tail(.m_list, n = 1)), recursive = FALSE), `[[`, "env" )
  }
  
  select(mutate(as_tibble(df), 
                      .universe = 1:nrow(df), 
                      .parameter_assignment = param.assgn, 
                      .code = .code, 
                      .results = .res
                    ), .universe, everything())
}


#' @rdname accessors
#' @export
size <- function(multiverse) {
  UseMethod("size")
}

#' @rdname accessors
#' @export
size.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `size`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
size.multiverse <- function(multiverse) {
  nrow(expand(multiverse))
}


#' @rdname accessors
#' @export
code <- function(multiverse) {
  UseMethod("code")
}

#' @rdname accessors
#' @export
code.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `code`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
code.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['code']]
}

#' @rdname accessors
#' @export
parameters <- function(multiverse) {
  UseMethod("parameters")
}

#' @rdname accessors
#' @export
parameters.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `parameters`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
#' @export
parameters.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['parameters']]
}

#' @rdname accessors
#' @export
conditions <- function(multiverse) {
  UseMethod("conditions")
}

#' @rdname accessors
#' @export
conditions.default <- function(multiverse) {
  stop(
    "Objects of type ", deparse(class(multiverse)), " do not have method `conditions`. \n",
    "Please use objects of type `multiverse."
  )
}

#' @rdname accessors
conditions.multiverse <- function(multiverse) {
  attr(multiverse, "multiverse")[['conditions']]
}

#' @rdname accessors
#' @param idx index of the universe in the multiverse (corresponds to the row in the table)
#' @export
extract_variable_from_universe <- function(multiverse, idx, name) {
  name = enquo(name)
  stopifnot( is.multiverse(multiverse) )
  m_diction = attr(multiverse, "multiverse")$multiverse_diction
  env_list <- lapply(m_diction$get(unlist(tail(m_diction$keys(), n = 1))), `[[`, "env")
  
  #m_tbl$.results[[idx]][[quo_text(name)]]
  get(quo_text(name), env_list[[idx]])
}


