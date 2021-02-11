#' Accessing contents of the multiverse object
#'
#' @description prints objects of class multiverse. Provides a quick overview of the multiverse by 
#' listing the parameters and conditions declared, to get an overview of the number of combinations.
#' The function outputs upto 20 of the parameters declared along with their option names (upto 5), 
#' and upto 10 of the conditions declared.
#' 
#' @name print
#' 
#' @param x An object of class multiverse
#' @param ... further arguments passed to or from other methods. Currently ignored.
#'
#' @export
print.multiverse <- function(x, ...) {
  y <- parameters(x)
  z <- conditions(x)
  ly <- ifelse(length(y) > 20, 20, length(y))
  lz <- ifelse(length(z) > 10, 10, length(z))
  
  cat("Multiverse\n\n")
  cat("  Multiverse consists of", nrow(expand(x)), "different analyses\n\n")
  
  cat("  ", "Parameters: \n")
  if (ly == 0) {
    cat("  ", "No parameters have been defined yet\n\n")
  } else {
    for (i in 1:ly) {
      cat("    ", "Parameter name:", names(y)[[i]], "\n")
      cat("        ")
      if (length(y[[i]]) > 5) {
        cat("options:", unlist(y[[i]])[1:4], "...")
        cat(" [", length(y[[i]]), " options", "] ", "\n", sep = "")
      } else {
        cat("options:", unlist(y[[i]]), "\n")
      }
    }
    if (length(y) > 20) {
      cat("  ", "...")
      cat(" [", (length(y) - 20), " parameters not shown]", "\n", sep = "")
    }
  }

  cat("\n  ", "Conditions:", "\n")  
  if (lz == 0) {
    cat("    ", "No conditions have been defined yet\n")
  } else {
    for (i in 1:lz) {
      cat("        ", as.character(z[[1]])[2], "\n")
    }
    if (length(z) > 10) {
      cat("  ", "...")
      cat(" [", (length(z) - 20), " conditions not shown]", "\n", sep = "")
    }
  }
}

