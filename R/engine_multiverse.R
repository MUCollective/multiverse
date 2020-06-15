multiverse_engine <- function(options) {
  # print(typeof(options$code))
  .multiverse_name = options$inside
  if ( !(.multiverse_name %in% ls(envir = globalenv()))) {
    stop("Cannot add code to multiverse object which has not been created")
  }
  
  if (strsplit(options$label, "-[0-9]+") == "unnamed-chunk") {
    stop("Please provide a label to your multiverse code block")
  }
  
  .multiverse = get(.multiverse_name, envir = globalenv())
  code = options$code
  n <- length(code)
  
  
  pasted <- paste(code, collapse = "\n")
  .expr <- parse(text = c("{", pasted, "}"))[[1]]
  .m = attr(.multiverse, "multiverse")
  
  #add_and_parse_code(
  #  m_obj = .m, 
  #  .super_env = attr(.multiverse, "multiverse_super_env"), 
  #  .code = eval_seq_in_code(.expr), 
  #  .name = options$label, 
  #  execute = FALSE
  #)
  
  inside(.multiverse, !! .expr, options$label)
  
  if ( is.list(.m[['parameters']]) & length(.m[['parameters']]) == 0 ) {
    # executing everything in the default universe
    # since there are no branches in the multiverse
    
    .c = .m[['multiverse_table']][['.code']][[1]]
  } else {
    idx = .m[['default_parameter_assignment']]
    
    .c = .m[['multiverse_table']][['.code']][[idx]]
  }
  .c = deparse(.c[[options$label]])
  
  # print(options)
  options$engine = "R"
  options$code = .c[2:(length(.c)-1)]
  options$comment = ""
  options$dev = 'png'
  
  # block_exec_R(options)
  invisible(block_exec_R(options))
  #knitr::engine_output(options, code = code, out = .out)
  # knitr::engine_output(options, code = code, out = evaluate::evaluate(options$code))
}

knitr::knit_engines$set(multiverse = multiverse_engine)


