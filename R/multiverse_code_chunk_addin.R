#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom rstudioapi insertText
#' @importFrom rstudioapi setCursorPosition

multiverse_code_chunk_addin <- function() {
  
  # Get cursor position
  context <- getActiveDocumentContext()
  label_index <- length(grep("```\\{multiverse .*?, inside = .*\\}", context[["contents"]], value = TRUE)) + 1
  start <- context$selection[1][[1]]$range$start
  
  insertText(start, 
                         paste("```{multiverse default-m-", label_index, ", inside = M}\n\n```", sep = ""))
  setCursorPosition(c(start['row'] + 1, 1), id = NULL)
}