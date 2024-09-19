#' Userlogs
#'
#' Data collected by Jansen et al. in their study,
#' \emph{Evaluating the Efficiency of Physical Visualizations}
#' which investigated factors contributing to the efficiency of physical visualizations.
#'
#' @format A data frame with 512 observations and 19 variables:
#' \describe{
#'     \item{subject}{Subject identifier}
#'     \item{group}{ Group / experiment session number in which the participant was involved in the experiment}
#'     \item{formerSubject}{Yes/No. Whether subject had participated in a previous experiment conducted by Jansen et al. }
#'     \item{conditionrank}{}
#'     \item{modalityname}{Name of the modality of interaction. One of "Virtual Mouse", "Virtual prop", "Physical touch", "Physical no-touch"}
#'     \item{repetition}{ 1/2. Whether the participant was interacting with the visualization for the first time.
#'     Participants interacted with visualizations of two different datasets and this variable stores the order.}
#'     \item{modality}{Index of the modality of interaction}
#'     \item{question}{ Index of question asked to the participant as part of the experiment.
#'     Each participant was asked 4 questions. All questions involved a comparison task. }
#'     \item{trial}{ Each participant performed 32 trials. Participants answered 4 questions
#'     for 8 different datasets which resulted in 32 trials per participant. }
#'     \item{datasetname}{ Each participant was presented with 8 different datasets through the visualizations.
#'     These were: "army", "carmortality", "education" "externaldebt", "grosscapital", "health", "hiv", "military"}
#'     \item{readingTime}{ Time taken by participant to read the visualization in seconds }
#'     \item{error}{ If the question was answered correctly by the participants. }
#'     \item{duration}{ If the question was answered correctly by the participants. }
#'     \item{perceivedDifficulty}{ Self reported perceived difficulty of the task by each participant. }
#'     \item{perceivedTime}{ Self reported perceived time taken to perform the task by each participant. }
#' }
#' 
#' @name userlogs
#' @docType data
#' @keywords datasets
#' 
#' @references
#' Yvonne Jansen and Pierre Dragicevic and Jean-Daniel Fekete. (2013)
#' "Evaluating the Efficiency of Physical Visualizations."
#' *Proceedings of the SIGCHI Conference on Human Factors in Computing Systems* pp. 2593-2602.
#' 
NULL