% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inside.R
\name{inside}
\alias{inside}
\title{Pass code into the multiverse}
\usage{
inside(multiverse, .expr, .label = NULL, .execute_default = TRUE)
}
\arguments{
\item{multiverse}{A multiverse object. A multiverse object is an S3 object which can be defined using \code{multiverse()}}

\item{.expr}{R syntax. All the operations that the user wants to perform within the multiverse can be passed.
Since it accepts a single argument, chunks of code can be passed using `\{\}`. See example for details.}

\item{.label}{It is extracted automatically from the code block of type \code{multiverse}
when run in an RMarkdown document. This should be used only within an RMarkdown document. 
Defaults to NULL.}

\item{.execute_default}{Should the default multiverse be executed as part of this call?}
}
\value{
a multiverse object
}
\description{
Add code to the multiverse using the function using a function call, or an assignment operator, which is
a wrapper around the function
}
\details{
The inside function can only access variables which can be accessed at the same environment 
where the multiverse object was declared in.

To perform a multiverse analysis, we will need to write code to be executed within the multiverse.
The \code{inside()} functions allows us to do this. Use \code{inside()} to pass any code to the specified multiverse,
which is captured as an expression. To define multiple analysis options in the code passed to the multiverse,
use the \code{branch()} function.
}
\examples{
\donttest{
M.1 <- multiverse()

# using `inside` to declare multiverse code
inside(M.1, {
  data <- rnorm(100, 50, 20)

  x.mean <- mean(data, trim = branch(
    trim_values,
    "trim_none" ~ 0,
    "trim_1pc" ~ 0.05,
    "trim_5pc" ~ 0.025,
    "trim_10pc" ~ 0.05
  ))
})

# declaring multiple options for a data processing step (calculating a new variable)
data(durante)
df <- durante

inside(M.1, {
  df <- df  \%>\%
    mutate( ComputedCycleLength = StartDateofLastPeriod - StartDateofPeriodBeforeLast ) \%>\%
    mutate( NextMenstrualOnset = branch(menstrual_calculation,
                                   "mc_option1" ~ StartDateofLastPeriod + ComputedCycleLength,
                                   "mc_option2" ~ StartDateofLastPeriod + ReportedCycleLength,
                                   "mc_option3" ~ StartDateNext
  ))
})

}

}
\seealso{
\code{branch()} for more details on how to declare multiple analysis options.

The \code{inside} function only stores the code, and does not execute any code at this step. To execute, we
provide separate functions. See \code{execute()} for executing the code.

Instead of using the \code{inside()} function, an alternate implementation of the multiverse is using
the assignment operator, `<-` (please refer to examples below).

\emph{Note}: the \code{inside()} function can only access variables which can be accessed at the same level as the multiverse
object. Since \code{inside()} is merely an interface to add analysis to the multiverse object, even if it is being called 
by another function, it is actually manipulating the multiverse object, which will have a different parent environment
from where \code{inside()} is called, and hence not have access to variables which might be accessible in the environment 
within the function from where \code{inside()} is called.
}
