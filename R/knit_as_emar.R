#' Compile a RMarkdown into an Explorable Multiverse Analysis Report
#' 
#' @description Include the function in your setup code chunk to compile a RMarkdown into an Explorable Multiverse Analysis Report
#' 
#' @details Explorable Multiverse Analysis Reports are interactive documents which allow the user
#' to interactively explor and look at each distinct end-to-end analysis path within a multiverse analysis. 
#' The idea was put forth by Dragicevic et al. (2019) in \doi{10.1145/3290605.3300295}{\emph{Increasing the 
#' Transparency of  Research Papers with Explorable Multiverse Analyses}}. 
#' Tangle.js is used to provide interactivity and allow the toggle through different specifications.
#'
#' @importFrom htmltools tags
#' @importFrom htmltools tagList
#' 
#' @name knit_as_emar
#' @export
knit_as_emar <- function() {
  htmltools::tagList(
    htmltools::tags$script(src = "https://code.jquery.com/jquery-3.6.0.min.js"),
    htmltools::tags$script(src = system.file("js/Tangle.js", package="multiverse")),
    htmltools::tags$script(src = system.file("js/TangleKit/mootools.js", package="multiverse")),
    htmltools::tags$script(src = system.file("js/TangleKit/sprintf.js", package="multiverse")),
    htmltools::tags$script(src = system.file("js/TangleKit/BVTouchable.js", package="multiverse")),
    htmltools::tags$script(src = system.file("js/TangleKit/TangleKit.js", package="multiverse")),
    htmltools::tags$script(src = system.file("js/custom.js", package="multiverse"))
  )
}