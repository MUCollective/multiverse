#' Survey to study the effect of fertility on religiosity and political attitudes
#'
#' A dataset containing the responses to the survey conducted by Durante, Rae, and Griskevicius (2013)
#' in their study, \emph{The
#' fluctuating female vote: Politics, religion, and the ovulatory cycle}.
#' Durante et al. study effect of fertility on religiosity and political attitudes.
#' Steegen, Tuerlinckx, Gelman, and Vanpaemel (2016) used this dataset in their analysis to illustrate how a
#' multiverse analysis can highlight the robustness of the conclusions reached by the original author.
#'
#' @format A data frame with 502 rows and 26 variables:
#' \describe{
#'   \item{Abortion}{"Abortion is a women’s [sic] right."}
#'   \item{DateTesting}{Date of participant filling in the questionnaire.}
#'   \item{Donate}{"For the next part of the study we will donate $1 to the presidential
#'   campaign of your preferred candidate. Please indicate which candidate’s campaign
#'   you would like us to donate $1 to.” Mitt Romney — Barack Obama"}
#'   \item{FreeMarket}{"In nearly every instance, the free market allocates resources most efficiently."}
#'   \item{Marijuana}{"Marijuana should be legal."}
#'   \item{Marriage}{"Marriage is between a man and a woman."}
#'   \item{PrivSocialSec}{"Privatize Social Security." 1 – 7}
#'   \item{Profit}{"Business corporations make too much profit."}
#'   \item{Rel1}{"How much do you believe in God?"}
#'   \item{Rel2}{"I see myself as a religiously oriented person."}
#'   \item{Rel3}{"I believe that God or a Higher Power is responsible for my existence"}
#'   \item{Relationship}{What is your current romantic relationship status?”
#'   (1) not dating/romantically involved with anyone,
#'   (2) dating or involved with only one partner,
#'   (3) engaged or living with my partner,
#'   (4) married, or
#'   (5) other. If participants picked response (5), they were prompted to provide a description of their
#'   relationship, which was subsequently coded into one of the four options by the
#'   original authors. The data here has already been coded into another response option. }
#'   \item{ReportedCycleLength}{How many days long are your menstrual cycles? (for most
#'   women, the range is between 25-35 days) Keep in mind this is the number of
#'   days from the start of one menstrual period to the start of the next menstrual
#'   period and NOT the length of your menstrual bleeding.}
#'   \item{RestrictAbortion}{"Laws should restrict abortion in all or most cases."}
#'   \item{RichTax}{"The rich should pay a higher tax rate than the middle class."}
#'   \item{StLiving}{"Government should ensure that all citizens meet a certain minimum standard of living"}
#'   \item{StartDateNext}{Indicates the expected start date of their next menstrual period
#'   (the research material does not contain a question about the variable. However,
#'   the data file for Study 2 contained this variable.)}
#'   \item{StartDateofLastPeriod}{Please give your best estimate of the date on which
#'   you started your last period (please be as precise as possible). This date was
#'   probably within the last few weeks. Sometimes thinking of where you were when
#'   you started your last period helps. For instance, was it on a weekend?, were you
#'   at work, was it during a football game?, etc. Please write the date in
#'   mm/dd/yyyy format (e.g., 8/18/2012).}
#'   \item{StartDateofPeriodBeforeLast}{Please give your best estimate of the date on
#'   which you started the period before your last period (please be as precise as
#'   possible). Please write the date in mm/dd/yyyy format (e.g., 7/18/2012).”}
#'   \item{StemCell}{"Stem cell research is moral and can be useful for science."}
#'   \item{Sure1}{"How sure are you about that date (StartDateofLastPeriod)?"}
#'   \item{Sure2}{How sure are you about that date (StartDateofPeriodBeforeLast)?}
#'   \item{Vote}{"Imagine walking into the voting booth today. Who would you vote for in the
#'   presidential election?” Mitt Romney (republican) – Barack Obama (democrat)"}
#'   \item{WorkerID}{ ID of participant }
#' }
#'
#' @details
#' All questions were preceded by the prompt --- "Please indicate how much you agree
#' with the following statements"
#'
#' The following items were responses to  religiosity items (on a scale of 1 - 9):
#' \emph{Rel1}, \emph{Rel2}, \emph{Rel3}
#'
#' The following items were responses to fiscal political attitudes items (on a scale of 1 - 7):
#' \emph{RichTax}, \emph{TooMuchProfit}, \emph{StandardLiving}, \emph{FreeMarket}, \emph{PrivSocialSec}
#'
#' The following items were responses to social political attitudes items (on a scale of 1 - 7):
#' \emph{Abortion}, \emph{Marriage}, \emph{StemCell}, \emph{Marijuana}, \emph{RestrictAbortion}
#' 
#' In addition, the values of \emph{StartDateofLastPeriod}, \emph{StartDateofPeriodBeforeLast} 
#' and \emph{StartDateNext} are missing for WorkerID 15 and 16. This impacts the calculation of 
#' \emph{CycleDay} variable in the dataset. Steegen et al. "reconstructed this variable
#' for their analysis after fixing some coding errors". To ensure that their results were identical
#' to Durante et al.'s, they used the processed variable Cycle Day from the original data file 
#' (11 and 18 for WorkerIDs 15 and 16, respectively).
#' 
#' @name durante
#' @docType data
#' @keywords datasets
#' 
#' @references
#' Kristina M Durante, Ashley Rae and Vladas Griskevicius. (2013).
#' "The fluctuating female vote: Politics, religion, and the ovulatory cycle."
#' *Psychological Science* 24(6), 1007-1016.
#' 
#' Sara Steegen and Francis Tuerlinckx and Andrew Gelman and Wolf Vanpaemel. (2015).
#' "Increasing transparency through a multiverse analysis."
#' *Perspectives on Psychological Science* 11(5), 702-712.
#' 
NULL
