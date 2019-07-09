knit_engines$set(multidy = function(options) {
    .multiverse_name = options$multiverse
    if ( !(.multiverse_name %in% ls(envir = globalenv()))) {
      stop("Cannot add code to multiverse object which has not been created")
    }
    
    .multiverse = get(.multiverse_name, envir = globalenv())
    .code = options$code
    .parsed_expr = expr({ !!parse_expr( paste0(.code, collapse = "") ) })
    
    inside(.multiverse, !!.parsed_expr)
})