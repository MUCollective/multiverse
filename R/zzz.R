#' @importFrom utils assignInNamespace

.onLoad <- function(libname, pkgname){
  knit_engines$set(multiverse = multiverse_engine)
  
  invisible()
}


.onAttach <- function(libname, pkgname){
  `%assignInNamespace%` = assignInNamespace
  ## Hot patch for knitting: re-assign the function in the relevant namespace
  ## TODO: delete eventually
  environment(custom_block_exec) <- asNamespace('knitr')
  `%assignInNamespace%`("block_exec", custom_block_exec, ns = "knitr")
  
  invisible()
}