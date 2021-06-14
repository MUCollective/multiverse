#' Survey to study the effect of fertility on religiosity and political attitudes
#'
#' A dataset for the study conducted by Jung et al. (2014)
#' in their study, \doi{10.1073/pnas.1402786111}{\emph{Female hurricanes are deadlier than male hurricanes}}.
#' 
#'
#' @format A data frame with 502 rows and 26 variables:
#' \describe{
#'   \item{Year}{Year in which the hurricane occurred}
#'   \item{Name}{Name given to the hurricane}
#'   \item{MasFem}{A score of how Masculine or Feminine a hurricane is, 
#'   on a scale of 1 - 11  where 1 is the most masculine and 11 is the most 
#'   feminine. Each hurricane was rated by 9 independent coders}
#'   \item{MinPressure_before}{The minimum pressure of hurricanes at the time of landfall in the United States
#'    obtained from NOAA (www.aoml.noaa.gov/hrd/hurdat/All_U.S._Hurricanes.html)}
#'   \item{Minpressure_Updated_2014}{The minimum pressure of hurricanes at the time of landfall in the United States
#'    obtained from NOAA (www.aoml.noaa.gov/hrd/hurdat/All_U.S._Hurricanes.html)}
#'   \item{Gender_MF}{"Marriage is between a man and a woman."}
#'   \item{Category}{Category labels on a scale of 1 - 5, with 5 being the most severe or extreme}
#'   \item{alldeaths}{Total number of deaths caused by the hurricane. Information on death tolls 
#'   of hurricanes were obtained primarily from monthly weather reports in the digital archive of the 
#'   National Oceanic and Atmospheric Administration (www.aoml.noaa.gov/hrd/hurdat/mwr_pdf/)}
#'   \item{HighestWindSpeed}{The maximum wind speed of hurricanes at the time of landfall in the
#'    United States obtained from NOAA (www.aoml.noaa.gov/hrd/hurdat/All_U.S._Hurricanes.html). 
#'    This data is only available for storms after 1979}
#'   \item{NDAM}{normalized damage (in million $)}
#'   \item{Source}{Source from where the data was gathered}
#'   \item{Elapsed.Yrs}{Time since hurricane}
#' }
#'
#' @details
#' This dataset was collated by Jung et. al in their study \emph{Female hurricanes are deadlier than male hurricanes}
#' They hypothesised that hurricanes with more feminine names might be perceived as less dangerous and hence lead to 
#' people taking fewer precautionary measures, resulting in more death and damages.
#' 
#' @name hurricane
#' @docType data
#' @keywords datasets
#' 
#' @references
#' Kiju Jung and Sharon Shavitta and Madhu Viswanathana and Joseph M. Hilbed. (2014).
#' "Female hurricanes are deadlier than male hurricanes."
#' *Proceedings of the National Academy of Sciences*, 111(24), 8782-8787.
#' 
#' Yang Liu and Alex Kale and Tim Althoff and Jeff Heer. (2020). 
#' Boba: Authoring and Visualizing Multiverse Analyzes. 
#' *arXiv preprint* arXiv: 2007.05551 .
#' 
NULL
