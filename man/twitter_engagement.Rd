% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twitter_engagement.R
\name{twitter_engagement}
\alias{twitter_engagement}
\title{Returns a float indicating engagement score for the text}
\usage{
twitter_engagement(text, version = NULL, ...)
}
\arguments{
\item{text}{text for analysis}

\item{version}{for api version}

\item{...}{additional arguments to passed to request}
}
\value{
float engagement score
}
\description{
Given input text, returns a probability distribution over the political
alignment of the speaker.
}
\examples{
text <- "I am so proud to stand here today
           as Prime Minister of four nations
           in one United Kingdom."
engagement <- twitter_engagement(text)
cat(sprintf("This text has twitter engagement of \%f",
            engagement))

}
\seealso{
\code{\link{sentiment}}, \code{\link{language}}
}
\keyword{API}
\keyword{analysis}
\keyword{engagement}
\keyword{indico.io}
\keyword{learning}
\keyword{machine}
\keyword{twitter}

