#' @importFrom knitr knit_global
#' @importFrom knitr knit_engines
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom formatR tidy_source
#' @importFrom purrr map_chr
#' @importFrom rlang is_condition
#' 
multiverse_engine <- function(options) {
  if(is.null(options$inside)) stop("A multiverse object should be specified with", 
                                   "a multiverse code block using the `inside` argument")
  
  .multiverse_name = options$inside 
  
  # in interactive use, `.multiverse_name` is a character: the name of the multiverse object
  # in kniting, this this is not the name of the multiverse, but the multiverse object itself
  # so during kniting, we don't need to retrieve it
  if (is.character(.multiverse_name)) {
    # check to see if a random character is not being declared
    if ( !(.multiverse_name %in% ls(envir = knit_global()))) {
      stop(
        "Multiverse object `", .multiverse_name, "` was not found.\n",
        "You may need to execute `", .multiverse_name, " <- multiverse()` to create the multiverse\n",
        "before executing a multiverse code block."
      )
    }
    
    # get the multiverse object associated with the name
    .multiverse = get(.multiverse_name, envir = knit_global())
  } else if (is.multiverse(.multiverse_name)) {
    .multiverse = .multiverse_name
  } # maybe an error?
  
  .c = multiverse_block_code(.multiverse, options$label, options$code)
  
  if(is.null(getOption("knitr.in.progress"))) {
    # during interactive execution
    if (!is.null(getOption("execute"))) {
      if (getOption("execute") == "all") {
        execute_multiverse(.multiverse)
      } else if (getOption("execute") == "default") {
        execute_universe(.multiverse)
      }
    }
    multiverse_default_block_exec(.c, options)
  } else {
    # during knitting
    multiverse_default_block_exec(options$code, options, TRUE)
  }
}

multiverse_block_code <- function(.multiverse, .label, .code) {
  if (strsplit(.label, "-[0-9]+") == "unnamed-chunk") {
    stop("Please provide a label to your multiverse code block")
  }
  
  if (!is(.multiverse, "multiverse")) {
    stop("Objects passed to inside should be a multiverse object")
  }
  
  pasted <- paste(.code, collapse = "\n")
  .expr <- parse(text = c("{", pasted, "}"), keep.source = FALSE)[[1]]
  
  add_and_parse_code(.multiverse, .expr, .label)
  
  .m_list = attr(.multiverse, "multiverse")$multiverse_diction$as_list()
  
  if ( is.list(parameters(.multiverse)) & length(parameters(.multiverse)) == 0 ) {
    # executing everything in the default universe
    # since there are no branches in the multiverse
    .c = get_code_universe(.m_list = .m_list, .uni = 1, .level = length(.m_list))
  } else {
    idx = 1 #.m[['default_parameter_assignment']]
    .c = get_code_universe(.m_list = .m_list, .uni = idx, .level = length(.m_list))
  }
  
  deparse(.c[[.label]])
}

multiverse_default_block_exec <- function(.code, options, knit = FALSE) {
  # if the code chunk is not supposed to be evaluated
  # skip all these steps while knitting.
  if (knit && options$eval) {
    .multiverse = options$inside
    
    # when knitting we are not performing any traditional evaluation
    # hence we can not evaluate the code chunk using default evaluation
    # changing this to TRUE would execute the default universe and show 
    # the relevant output
    # What we want is to create a `div` for each universe
    # options$eval = TRUE
    # options$class.source = "multiverse"
    options$eval = FALSE
    options$engine = "R"
    options$comment = ""
    options$dev = 'png'
    
    eng_r = knit_engines$get("R")
    
    if (getOption("multiverse_code_blocks", 1) == "asis") {
      return(eng_r(options))
    }

    # if (options$eval != FALSE) {
    options_list <- lapply(1:size(.multiverse), function(x) {
      temp_options <- options
      temp_options$code = tidy_source(text = map_chr(
        tail(head(deparse(expand(.multiverse)[[".code"]][[x]][[options$label]]), -1), -1),
        ~ gsub(pattern = " ", replacement = "", x = .)
      ))$text.tidy

      # assuming default is the first universe,
      # conditional should be change to use the default universe argument
      if (x == 1) {
          temp_options$class.source = paste0("multiverse universe-", x, " default")
          temp_options$class.output = paste0("multiverse universe-", x, " default")
      } else {
          temp_options$class.source = paste0("multiverse universe-", x, "")
          temp_options$class.output = paste0("multiverse universe-", x, "")
      }

      temp_options
    })
    
    unlist(lapply(options_list, eng_r))
  } else {
    # when in interactive mode, execute the default analysis in the knitr global environment
    
    # when not knitting (i.e. interactive mode) we just use evaluate()
    # to evaluate the various pieces of code in the code chunk and return
    # the output strings from each line of code.
    code = .code[-c(1, length(.code))]
      
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
    Filter(function(x) is.character(x) || is_condition(x), outputs)
  }
}

knitr::knit_engines$set(multiverse = multiverse_engine)
