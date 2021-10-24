#' Create custom code blocks for multiverse analysis
#' 
#' @description An easier way to interact with a multiverse object by using a custom code block
#' 
#' @details This is a custom code block that allows the users to interface directly with a created multiverse object. 
#' This allows users to implement analysis in RMarkdown without using auxiliary functions such as [inside]. However,
#' [inside] is still required to define and execute multiverse analyses in a RScript using this package. 
#' See examples for more details.
#' 
#' In RStudio, you can create a shortcut for this using RStudio addins
#' (we recommend `Cmd + Opt + M` in Mac and `Ctrl + Alt + M` in Windows).
#' To add a shortcut, go to Tools > AddIns > Browse AddIns... > Keyboard shortcuts. 
#' Search for `insert multiverse code chunk` and add a keyboard shortcut to this function. 
#' Once you have set this up, the keyboard shortcut will create a code block in 
#' any RMarkdown document.
#' 
#' @section Code Block Options:
#' The multiverse code blocks require two named arguments:
#' \enumerate{
#'   \item label: this is a unique identifier for each code block. 
#'   If the same label is used for two different code blocks, the code 
#'   associated with the previous block in the multiverse will be 
#'   overwritten by the subsequent one. If a code block is created using the keyboard
#'   shortcut, it will auto-generate a (unique) label
#'   \item inside: the multiverse object this code block will be associated with.
#'   Defaults to "M"
#' }
#'  
#' @examples
#' \dontrun{
#' # Typically R users, using RMarkdown could specify code by creating a code block, 
#' # and use the functions provided to add code to the multiverse:
#' 
#' ```{r}
#' M = multiverse()
#' inside(M, { df = data.frame( x = 1:10 ) })
#' ```
#' 
#' # Here, they would need to reference the multiverse object
#' #every time they want to add anything to it.
#' # Instead, they could add code to the multiverse by using a custom code engine:
#' 
#' ```{multiverse, default-m-1, inside = M}
#' df = data.frame( x = 1:10 )
#' ```
#' }
#'  
#' @name mulitiverse_code_block
NULL