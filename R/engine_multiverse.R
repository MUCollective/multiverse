multiverse_engine <- function(options) {
  .c = multiverse_block_code(options$inside, options$label, options$code)
  if(is.null(getOption("knitr.in.progress"))) {
     multiverse_default_block_exec(.c, options)
  } else {
     multiverse_default_block_exec(options$code, options, TRUE)
  }
}

multiverse_block_code <- function(.multiverse_name, .label, .code) {
  if ( !(.multiverse_name %in% ls(envir = globalenv()))) {
    stop("Cannot add code to multiverse object which has not been created")
  }
  
  if (strsplit(.label, "-[0-9]+") == "unnamed-chunk") {
    stop("Please provide a label to your multiverse code block")
  }
  
  .multiverse = get(.multiverse_name, envir = globalenv())
  n <- length(.code)
  
  pasted <- paste(.code, collapse = "\n")
  .expr <- parse(text = c("{", pasted, "}"))[[1]]
  .m = attr(.multiverse, "multiverse")
  
  # within the call to `inside()` we detect whether the 
  # execution is in interactive mode or during knit mode.
  # If in knit mode, it auto-executes all the universes in the multiverse.
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

multiverse_default_block_exec <- function(.code, options, knit = FALSE) {
  if (knit) {
    # when knitting we are not performing any traditional evaluation
    # hence we can not evaluate the code chunk using default evaluation
    options$eval = FALSE
    options$class.source = "multiverse"
  } else {
    options$code = .code
  }
  
  options$engine = "R"
  options$comment = ""
  options$dev = 'png'
  
  block_exec_R(options)
}

knitr::knit_engines$set(multiverse = multiverse_engine)


