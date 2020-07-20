multiverse_engine <- function(options) {
  .c = multiverse_block_code(options$inside, options$label, options$code)
  
  if(is.null(getOption("knitr.in.progress"))) {
     multiverse_default_block_exec(.c, options)
  } else {
     multiverse_default_block_exec(options$code, options, TRUE)
  }
}

multiverse_block_code <- function(.multiverse_name, .label, .code) {
  if ( !(.multiverse_name %in% ls(envir = knit_global()))) {
    stop(
      "Multiverse object `", .multiverse_name, "` was not found.\n",
      "You may need to execute `", .multiverse_name, " <- multiverse()` to create the multiverse\n",
      "before executing a multiverse code block."
    )
  }
  
  if (strsplit(.label, "-[0-9]+") == "unnamed-chunk") {
    stop("Please provide a label to your multiverse code block")
  }
  
  .multiverse = get(.multiverse_name, envir = knit_global())
  n <- length(.code)
  
  pasted <- paste(.code, collapse = "\n")
  .expr <- parse(text = c("{", pasted, "}"), keep.source = FALSE)[[1]]
  .m = attr(.multiverse, "multiverse")
  
  add_and_parse_code(.multiverse, .expr, .label)
  
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
  # ugly hack to get around `:::` warnings (TODO: delete eventually)
  `%:::%` = `:::`

  if (knit) {
    # when knitting we are not performing any traditional evaluation
    # hence we can not evaluate the code chunk using default evaluation
    options$eval = FALSE
    options$class.source = "multiverse"

    options$engine = "R"
    options$comment = ""
    options$dev = 'png'
    
    block_exec_R(options)
  } else {
    # when in interactive mode, execute the default analysis in the knitr global environment
    
    # first and last elements of `code` are "{" and "}" so we have to strip them.
    # (otherwise only the last thing would be printed as the entire expression would
    # only return one object)
    code = .code[-c(1, length(.code))]
    
    # when not knitting (i.e. interactive mode) we just use evaluate()
    # to evaluate the various pieces of code in the code chunk and return
    # the output strings from each line of code.
    outputs = evaluate::evaluate(
      code, 
      # must have new_device = FALSE otherwise plots don't seem to be written to
      # the graphics device inside rmarkdown in RStudio
      new_device = FALSE,
      envir = knit_global()
    )

    # only output character vectors and conditions (warnings, etc) values (not plots or
    # source code) here, as everything else (e.g. graphics, messages) will have already
    # been output during evaluate()
    outputs[sapply(outputs, function(x) is.character(x) || is_condition(x))]
  }
}

knitr::knit_engines$set(multiverse = multiverse_engine)


