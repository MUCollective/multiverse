#' Participants responses in the study by Harrison et al., "Ranking Visualizations of correlation according to Weber's law"
#'
#' A dataset containing the responses of the participants. In this study, participants were asked to make multiple judgements about the correlation.
#'
#' @format A data frame with 502 rows and 26 variables:
#' \describe{
#'   \item{participant}{Participant identifier}
#'   \item{vis}{Type of visualization used}
#'   \item{rdirection}{The direction of the slope of the line (positive or negative)}
#'   \item{sign}{The direction of the slope of the line (1 or -1)}
#'   \item{visandsign}{A combination variable of vis(ualization type) and rdirection}
#'   \item{rbase}{The coefficiant of correlation using which the stimuli was generated}
#'   \item{approach}{NA}
#'   \item{jnd}{Estimated JND value for that participant}
#'   \item{condition}{Condition that the participant was placed in (which is a combination of vis, rbase and approach)}
#' }
#'
#' @details
#'
"vis_correlation"

