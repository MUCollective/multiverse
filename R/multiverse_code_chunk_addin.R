#' @title Create a multiverse code chunk
#' @description Addin for RStudio for creating multiverse code chunks
#'
#' @export
multiverse_code_chunk_addin <- function() {
  
  # Get cursor position
  context <- rstudioapi::getActiveDocumentContext()
  label_index <- length(grep("```\\{multiverse .*?, inside = .*\\}", context[["contents"]], value = TRUE)) + 1
  start <- context$selection[1][[1]]$range$start
  
  rstudioapi::insertText(start, 
                         paste("```{multiverse label = default-m-", label_index, ", inside = M}\n\n```", sep = ""))
  rstudioapi::setCursorPosition(c(start['row'] + 1, 1), id = NULL)
}