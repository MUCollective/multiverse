#' @import knitr
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom formatR tidy_source


m.eng_r = function(options, env = knit_global()) {
  # eval chunks (in an empty envir if cache)
  obj.before = ls(env, all.names = TRUE)  # global objects before chunk
  
  keep = options$fig.keep
  keep.idx = NULL
  if (is.logical(keep)) keep = which(keep)
  if (is.numeric(keep)) {
    keep.idx = keep
    keep = "index"
  }
  
  if (keep.pars <- opts_knit$get('global.par')) on.exit({
    opts_knit$set(global.pars = par(no.readonly = TRUE))
  }, add = TRUE)
  
  tmp.fig = tempfile(); on.exit(unlink(tmp.fig), add = TRUE)
  # open a device to record plots if not using a global device or no device is
  # open, and close this device if we don't want to use a global device
  if (!opts_knit$get('global.device') || is.null(dev.list())) {
    knitr:::chunk_device(options, keep != 'none', tmp.fig)
    dv = dev.cur()
    if (!opts_knit$get('global.device')) on.exit(dev.off(dv), add = TRUE)
    knitr:::showtext(options)  # showtext support
  }
  # preserve par() settings from the last code chunk
  if (keep.pars) par2(opts_knit$get('global.pars'))
  
  res.before = knitr:::run_hooks(before = TRUE, options, env) # run 'before' hooks
  
  code = options$code
  echo = options$echo  # tidy code if echo
  if (!isFALSE(echo) && !isFALSE(options$tidy) && length(code)) {
    tidy.method = if (isTRUE(options$tidy)) 'formatR' else options$tidy
    if (is.character(tidy.method)) tidy.method = switch(
      tidy.method,
      formatR = function(code, ...) {
        if (!loadable('formatR')) stop2(
          'The formatR package is required by the chunk option tidy = TRUE but ',
          'not installed; tidy = TRUE will be ignored.'
        )
        formatR::tidy_source(text = code, output = FALSE, ...)$text.tidy
      },
      styler = function(code, ...) unclass(styler::style_text(text = code, ...))
    )
    res = try_silent(do.call(tidy.method, c(list(code), options$tidy.opts)))
    
    if (!inherits(res, 'try-error')) code = res else warning(
      "Failed to tidy R code in chunk '", options$label, "'. Reason:\n", res
    )
  }
  # only evaluate certain lines
  if (is.numeric(ev <- options$eval)) {
    # group source code into syntactically complete expressions
    if (isFALSE(options$tidy)) code = sapply(xfun::split_source(code), one_string)
    iss = seq_along(code)
    code = comment_out(code, '##', setdiff(iss, iss[ev]), newline = FALSE)
  }
  # guess plot file type if it is NULL
  if (keep != 'none') options$fig.ext = knitr:::dev2ext(options)
  
  cache.exists = knitr:::cache$exists(options$hash, options$cache.lazy)
  evaluate = knit_hooks$get('evaluate')
  # return code with class 'source' if not eval chunks
  res = if (knitr:::is_blank(code)) list() else if (isFALSE(ev)) {
    knitr:::as.source(code)
  } else if (cache.exists && isFALSE(options$cache.rebuild)) {
    fix_evaluate(cache$output(options$hash, 'list'), options$cache == 1)
  } else knitr:::in_input_dir(
    evaluate(
      code, envir = env, new_device = FALSE,
      keep_warning = !isFALSE(options$warning),
      keep_message = !isFALSE(options$message),
      stop_on_error = if (is.numeric(options$error)) options$error else {
        if (options$error && options$include) 0L else 2L
      },
      output_handler = knitr:::knit_handlers(options$render, options)
    )
  )
  if (options$cache %in% 1:2 && (!cache.exists || isTRUE(options$cache.rebuild))) {
    # make a copy for cache=1,2; when cache=2, we do not really need plots
    res.orig = if (options$cache == 2) remove_plot(res, keep == 'high') else res
  }
  
  # eval other options after the chunk
  if (!isFALSE(ev))
    for (o in opts_knit$get('eval.after'))
      options[o] = list(knitr:::eval_lang(options[[o]], env))
  
  # remove some components according options
  if (isFALSE(echo)) {
    res = Filter(Negate(evaluate::is.source), res)
  } else if (is.numeric(echo)) {
    # choose expressions to echo using a numeric vector
    res = if (isFALSE(ev)) {
      knitr:::as.source(code[echo])
    } else {
      knitr:::filter_evaluate(res, echo, evaluate::is.source)
    }
  }
  if (options$results == 'hide') res = Filter(Negate(is.character), res)
  if (options$results == 'hold') {
    i = vapply(res, is.character, logical(1))
    if (any(i)) res = c(res[!i], merge_character(res[i]))
  }
  res = knitr:::filter_evaluate(res, options$warning, evaluate::is.warning)
  res = knitr:::filter_evaluate(res, options$message, evaluate::is.message)
  
  # rearrange locations of figures
  res = knitr:::rearrange_figs(res, keep, keep.idx, options$fig.show)
  
  # number of plots in this chunk
  if (is.null(options$fig.num))
    options$fig.num = if (length(res)) sum(sapply(res, function(x) {
      if (inherits(x, 'knit_image_paths')) return(length(x))
      if (knitr:::is_plot_output(x)) return(1)
      0
    })) else 0L
  
  # # merge neighbor elements of the same class into one element
  for (cls in c('source', 'message', 'warning')) res = knitr:::merge_class(res, cls)
  
  if (isTRUE(options$fig.beforecode)) res = fig_before_code(res)
  
  # on.exit({
  #   plot_counter(reset = TRUE)
  #   shot_counter(reset = TRUE)
  #   opts_knit$delete('plot_files')
  # }, add = TRUE)  # restore plot number
  
  output = unlist(sew(res, options)) # wrap all results together
  res.after = knitr:::run_hooks(before = FALSE, options, env) # run 'after' hooks
  
  output = paste(c(res.before, output, res.after), collapse = '')  # insert hook results
  output = knit_hooks$get('chunk')(output, options)
  
  if (options$cache > 0) {
    # if cache.vars has been specifically provided, only cache these vars and no
    # need to look for objects in globalenv()
    obj.new = if (is.null(options$cache.vars)) setdiff(ls(globalenv(), all.names = TRUE), obj.before)
    copy_env(globalenv(), env, obj.new)
    objs = if (isFALSE(ev) || length(code) == 0) character(0) else
      options$cache.vars %n% codetools::findLocalsList(parse_only(code))
    # make sure all objects to be saved exist in env
    objs = intersect(c(objs, obj.new), ls(env, all.names = TRUE))
    if (options$autodep) {
      # you shall manually specify global object names if find_symbols() is not reliable
      cache$objects(
        objs, cache_globals(options$cache.globals, code), options$label,
        options$cache.path
      )
      dep_auto()
    }
    if (options$cache < 3) {
      if (options$cache.rebuild || !cache.exists) block_cache(options, res.orig, objs)
    } else block_cache(options, output, objs)
  }
  
  if (options$include) output else if (is.null(s <- options$indent)) '' else s
}


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
  # .multiverse = get(.multiverse_name, envir = knit_global())
  
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
    # execute_multiverse(.multiverse)
    
    # when knitting we are not performing any traditional evaluation
    # hence we can not evaluate the code chunk using default evaluation
    # changing this to TRUE would execute the default universe and show 
    # the relevant output
    # What we want is to create a `div` for each universe
    # options$eval = TRUE
    # options$class.source = "multiverse"
    
    options$engine = "R"
    options$comment = ""
    options$dev = 'png'
    multiverse_options = options
    
    # eng_r = knit_engines$get("R")
    
    options_list <- lapply(1:size(.multiverse), function(x) {
      temp_options <- options
      temp_options$code = tidy_source(text = map_chr(
        tail(head(deparse(expand(.multiverse)[[".code"]][[x]][[options$label]]), -1), -1),
        ~ gsub(pattern = " ", replacement = "", x = .)
      ))$text.tidy

      temp_env = expand(.multiverse)[[".results"]][[x]]
      
      .assignment = expand(M)[[".parameter_assignment"]][[x]]
      class_name = paste(names(.assignment), .assignment, sep="---", collapse=" ")

      # assuming default is the first universe, we add a class `default` to the relevant HTML elements
      # conditional should be changed to use the default universe argument
      if (x == 1) {
          temp_options$class.source = paste0("multiverse universe ", class_name, " default")
          temp_options$class.output = paste0("multiverse universe ", class_name, " default")
      } else {
          temp_options$class.source = paste0("multiverse universe ", class_name, "")
          temp_options$class.output = paste0("multiverse universe ", class_name, "")
      }

      m.eng_r(temp_options, temp_env)
    })
    
    # preserves the original declaration of the multiverse code block
    # i.e. with the branch syntax which specifies alternative analyses
    multiverse_options$eval = FALSE
    multiverse_options$class.source = "multiverse-spec"
    multiverse_options$class.output = "multiverse-spec"
    unlist(append(options_list, m.eng_r(multiverse_options)))
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

