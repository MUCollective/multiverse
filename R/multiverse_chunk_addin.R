#' @title Create a multiverse code chunk
#' @description Addin for RStudio for creating multiverse code chunks
#'
#' @export
insert_multiverse_code <- function() {
  
  # Get cursor position
  context <- rstudioapi::getActiveDocumentContext()
  start <- context$selection[1][[1]]$range$start
  
  rstudioapi::insertText(start, "```{multierse label = , name = M}\n\n```")
  rstudioapi::setCursorPosition(c(start['row'] + 1, 1), id = NULL)
}