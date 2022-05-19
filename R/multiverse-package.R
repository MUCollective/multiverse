#' 'Explorable Multiverse' Data Analysis and Reports in R
#'
#' @docType package
#' @name multiverse-package
#'
#' @description
#'
#' \code{multiverse} is an R package that aims to make it easy to declare 'multiverse-style'
#' analysis in R and R notebooks. The 'multiverse' package allows users to 
#' concisely and flexibly declare alternate ways of performing an analysis step 
#' in order to show how fragile or robust their findings are.
#'
#' @details
#'
#' 'Multiverse style' analyses (Steegen 2016) is intended to highlight the robustness of
#' an analysis to arbitrary decisions that are present in any data analysis. Considering all
#' possible combinations of reasonable decisions that can be made at each step of an analysis,
#' 'multiverse style' analysis can surface whether an outcome is an artifact of a particular 
#' idiosyncratic combination of analysis choices, or if it is robust against such arbitrary
#' choices.
#' 
#' However, current tools do not support declaring 'multiverse' analysis easily, requiring users
#' to declare custom control flows and multiple nested 'if-else' blocks. The `multiverse` package
#' aims to simplify the process of composing 'multiverse' analysis using a flexible and concise
#' syntax.
#' 
#' To get started with the multiverse package please refer to \code{vignette("branch")}`, 
#' \code{vignette("conditions")} and \code{vignette("multiverse-in-rmd")}.
#' For example implementations of analysis using the multiverse package, see the case studies
#' \code{vignette("durante-multiverse-analysis")} and \code{vignette("hurricane")}.
#'
#' @references
#'
#' Steegen, Sara, Francis Tuerlinckx, Andrew Gelman, and Wolf Vanpaemel. (2016). 
#' Increasing transparency through a multiverse analysis. _Perspectives on Psychological Science_,
#' 11(5), 702-712. \doi{10.1177/1745691616658637}.
#'
NULL