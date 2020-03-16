#' Create custom code chunks for multiverse analysis
#' 
#' @description An easier way to interact with a multiverse object by using a custom code engine
#' 
#' @param options The knitr options associated with the code chunk
#' 
#' @details This is a custom code engine that allows the users to interface directly with a created multiverse object, 
#' without using functions such as [inside]. See examples for more details
#' 
#' 
#' @examples
#' \dontrun{
#' # Typically R users, using RMarkdown could specify code by creating a code chunk, 
#' # and use the functions provided to add code to the multiverse :
#' ```{r}
#' M = multiverse()
#' inside(M, { df = data.frame( x = 1:10 ) })
#' ```
#' 
#' # Here, they would need to reference the multiverse object everytime they want to add anythign to it
#' # Instead, they could add code to the multiverse by using a custom code engine:
#' ```{multiverse, name = M}
#' df = data.frame( x = 1:10 )
#' }
#' 
#' @import knitr
# @importFrom knitr knit_engines
# @importFrom knitr engine_output
#' 
#' @name multiverse_engine
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
  parsed <- parse_expr(c("{", pasted, "}"))
  
  inside(.multiverse, !! parsed, options$label)
  # lapply(parsed, function(x) inside(.multiverse, !!x ))
  
  .m = attr(.multiverse, "multiverse")
  
  if ( is.list(.m[['parameters']]) & length(.m[['parameters']]) == 0 ) {
    # executing everything in the default universe
    # since there are no branches in the multiverse
    
    # env = .m[['multiverse_table']][['.results']][[1]]
    .c = .m[['multiverse_table']][['.code']][[1]]
  } else {
    idx = .m[['default_parameter_assignment']]
    
    # env = .m[['multiverse_table']][['.results']][[idx]]
    .c = .m[['multiverse_table']][['.code']][[idx]]
  }
  
  options$engine = "R"
  options$code = deparse(.c[[options$label]])
  options$comment = ""
  
  engine_output( options, code = code, out = block_exec_R(options))
}

knitr::knit_engines$set(multiverse = multiverse_engine)


