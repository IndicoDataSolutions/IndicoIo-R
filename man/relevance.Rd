% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relevance.R
\name{relevance}
\alias{relevance}
\title{Determines how relevant a query string is to a given document}
\usage{
relevance(text, queries, version = NULL, ...)
}
\arguments{
\item{text}{text for analysis}

\item{version}{for api version}

\item{...}{additional arguments to passed to request}
}
\value{
float or array of floats
}
\description{
Determines how relevant a query string is to a given document
}
\examples{
result <- relevance("Barack Obama", "president")
cat(sprintf("\%s is mentioned in the input text", result[0]['text']))

}
\seealso{
\code{\link{keywords}}
}

