% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiverse-package.R
\docType{package}
\name{multiverse-package}
\alias{multiverse-package}
\title{'Explorable Multiverse' Data Analysis and Reports in R}
\description{
Implement 'multiverse' style analyses (Steegen S., Tuerlinckx F, Gelman A., Vanpaemal, W., 2016) \doi{10.1177/1745691616658637} to show the robustness of statistical inference. 'Multiverse analysis' is a philosophy of statistical reporting where paper authors report the outcomes of many different statistical analyses in order to show how fragile or robust their findings are. The 'multiverse' package (Sarma A., Kale A., Moon M., Taback N., Chevalier F., Hullman J., Kay M., 2021) \doi{10.31219/osf.io/yfbwm} allows users to concisely and flexibly implement 'multiverse-style' analysis, which involve declaring alternate ways of performing an analysis step, in R and R Notebooks.

\code{multiverse} is an R package that aims to make it easy to declare 'multiverse-style'
analysis in R and R notebooks. The 'multiverse' package allows users to 
concisely and flexibly declare alternate ways of performing an analysis step 
in order to show how fragile or robust their findings are.
}
\details{
'Multiverse style' analyses (Steegen 2016) is intended to highlight the robustness of
an analysis to arbitrary decisions that are present in any data analysis. Considering all
possible combinations of reasonable decisions that can be made at each step of an analysis,
'multiverse style' analysis can surface whether an outcome is an artifact of a particular 
idiosyncratic combination of analysis choices, or if it is robust against such arbitrary
choices.

However, current tools do not support declaring 'multiverse' analysis easily, requiring users
to declare custom control flows and multiple nested 'if-else' blocks. The `multiverse` package
aims to simplify the process of composing 'multiverse' analysis using a flexible and concise
syntax.

To get started with the multiverse package please refer to \code{vignette("branch")}`, 
\code{vignette("conditions")} and \code{vignette("multiverse-in-rmd")}.
For example implementations of analysis using the multiverse package, see the case studies
\code{vignette("durante-multiverse-analysis")} and \code{vignette("hurricane")}.
}
\references{
Steegen, Sara, Francis Tuerlinckx, Andrew Gelman, and Wolf Vanpaemel. (2016). 
Increasing transparency through a multiverse analysis. _Perspectives on Psychological Science_,
11(5), 702-712. \doi{10.1177/1745691616658637}.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://mucollective.github.io/multiverse/}
  \item \url{https://github.com/mucollective/multiverse/}
  \item Report bugs at \url{https://github.com/MUCollective/multiverse/issues/new}
}

}
\author{
\strong{Maintainer}: Abhraneel Sarma \email{abhraneel@u.northwestern.edu}

Authors:
\itemize{
  \item Matthew Kay \email{mjskay@northwestern.edu}
}

Other contributors:
\itemize{
  \item Michael Moon \email{moon@utstat.toronto.edu} [contributor]
  \item Mark Miller \email{725mrm@gmail.com} [contributor]
  \item Kyle Hwang \email{kyle.sg.hwang@gmail.com} [contributor]
  \item Hadley Wickham \email{hadley@posit.co} [contributor]
  \item Alex Kale \email{kalea@uw.edu} [contributor]
  \item Nathan Taback \email{nathan.taback@utoronto.ca} [contributor]
  \item Fanny Chevalier \email{fanny@dgp.toronto.edu} [contributor]
  \item Jessica Hullman \email{jessica.hullman@gmail.com} [contributor]
  \item Pierre Dragicevic \email{pierre.dragicevic@inria.fr} [contributor]
  \item Yvonne Jansen \email{yvonne.jansen@sorbonne-universite.fr} [contributor]
}

}
