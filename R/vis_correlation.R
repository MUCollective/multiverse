#' Participants responses in the study by Harrison et al., "Ranking Visualizations of correlation according to Weber's law"
#'
#' A dataset containing the aggregated responses of the participants. 
#' In this study, participants were asked to make multiple judgements about the correlation.
#'
#' @format A data frame with 502 rows and 26 variables:
#' \describe{
#'   \item{participant}{Participant identifier}
#'   \item{vis}{Type of visualization used}
#'   \item{rdirection}{The direction of the slope of the line (positive or negative)}
#'   \item{sign}{The direction of the slope of the line (1 or -1)}
#'   \item{visandsign}{A combination variable of visualization type and rdirection}
#'   \item{rbase}{The coefficient of correlation using which the stimuli was generated}
#'   \item{approach}{NA}
#'   \item{jnd}{Estimated JND value for that participant}
#'   \item{condition}{Condition that the participant was placed in (which is a combination of vis, rbase and approach)}
#' }
#' 
#' @name vis_correlation
#' @docType data
#' @keywords datasets
#' 
#' @references
#' Pierre Dragicevic, Yvonne Jansen, Abhraneel Sarma, Matthew Kay and Fanny Chevalier. (2019).
#' "Increasing the transparency of research papers with explorable multiverse analyses."
#' *Proceedings of the 2019 CHI Conference on Human Factors in Computing Systems* pp. 1-15.
#' 
#' Kay, Matthew, and Jeffrey Heer. (2016).
#' "Beyond Weber's law: A second look at ranking visualizations of correlation."
#' *IEEE transactions on visualization and computer graphics* 22.1: 469-478.
#'
#' Lane Harrison, Fumeng Yang, Steven Franconeri, and Remco Chang. (2014).
#' "Ranking visualizations of correlation using Weber's law."
#' *IEEE transactions on visualization and computer graphics* 20.12: 1943-1952.
NULL


