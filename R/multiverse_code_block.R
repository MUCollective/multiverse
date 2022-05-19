#' Create custom code blocks for multiverse analysis
#' 
#' @description An easier way to interact with a multiverse object by using a custom code block
#' 
#' @details This is a custom code block that allows the users to interface directly with a created multiverse object. 
#' This allows users to implement analysis in RMarkdown without using auxiliary functions such as \code{inside()}. However,
#' \code{inside()} is still required to define and execute multiverse analyses in a RScript using this package. 
#' See \code{vignette("multiverse-in-rmd")} for more details.
#' 
#' In RStudio, you can create a shortcut for this using RStudio AddIns
#' (we recommend \code{Cmd + Opt + M} in Mac and \code{Ctrl + Alt + M} in Windows).
#' To add a shortcut, go to Tools > AddIns > Browse AddIns... > Keyboard shortcuts. 
#' Search for \emph{insert multiverse code chunk} and add a keyboard shortcut to this function. 
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
#' @name mulitiverse_code_block
NULL
