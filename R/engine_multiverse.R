multiverse_engine <- function(options) {
  .c = multiverse_block_code(options$inside, options$label, options$code)
  
  multiverse_default_block_exec(.c, options)
  # custom_block_exec(options)
  # knitr::engine_output(options, code = code, out = .out)
  # knitr::engine_output(options, code = code, out = evaluate::evaluate(options$code))
}

multiverse_block_code <- function(.multiverse_name, .label, .code) {
  if ( !(.multiverse_name %in% ls(envir = globalenv()))) {
    stop("Cannot add code to multiverse object which has not been created")
  }
  
  if (strsplit(.label, "-[0-9]+") == "unnamed-chunk") {
    stop("Please provide a label to your multiverse code block")
  }
  
  .multiverse = get(.multiverse_name, envir = globalenv())
  code = .code
  n <- length(code)
  
  pasted <- paste(code, collapse = "\n")
  .expr <- parse(text = c("{", pasted, "}"))[[1]]
  .m = attr(.multiverse, "multiverse")
  
  inside(.multiverse, !!.expr, .label)
  
  if ( is.list(.m[['parameters']]) & length(.m[['parameters']]) == 0 ) {
    # executing everything in the default universe
    # since there are no branches in the multiverse
    .c = .m[['multiverse_table']][['.code']][[1]]
  } else {
    idx = .m[['default_parameter_assignment']]
    .c = .m[['multiverse_table']][['.code']][[idx]]
  }
  
  .c = deparse(.c[[.label]])
  
  .c
}

multiverse_default_block_exec <- function(.code, options) {
  options$engine = "R"
  options$code = .code[2:(length(.code)-1)]
  options$comment = ""
  options$dev = 'png'
  
  block_exec_R(options)
}

knitr::knit_engines$set(multiverse = multiverse_engine)


