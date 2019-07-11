#' Create custom code chunks for multidy
#' 
#' @description An easier way to interact with a multiverse object by using a custom code engine
#' 
#' @details This is a custom code engine that allows the users to interface directly with a created multiverse object, 
#' without using functions such as [inside]. See examples for more details
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
#' ```{multidy, multiverse = M}
#' df = data.frame( x = 1:10 )
#' }
#' 
#' @importFrom knitr knit_engines

knit_engines$set(multidy = function(options) {
    .multiverse_name = options$multiverse
    if ( !(.multiverse_name %in% ls(envir = globalenv()))) {
      stop("Cannot add code to multiverse object which has not been created")
    }
    
    .multiverse = get(.multiverse_name, envir = globalenv())
    .code = options$code
    .parsed_expr = expr({ !!parse_expr( paste0(.code, collapse = "") ) })
    
    inside(.multiverse, !!.parsed_expr)
    
    engine_output(options, .code, '')
})