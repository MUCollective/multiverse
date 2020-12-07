#' @importFrom utils assignInNamespace

.onLoad <- function(libname, pkgname){
  # `%:::%` = `:::`
  
  knitr::knit_engines$set(multiverse = multiverse_engine)
  
  # custom_block_exec <- multiverse%:::%custom_block_exec
  
  ## Hot patch for knitting: re-assign the function in the relevant namespace
  # environment(custom_block_exec) <- asNamespace('knitr')
  # assignInNamespace("block_exec", custom_block_exec, ns = "knitr")
  
  invisible()
}