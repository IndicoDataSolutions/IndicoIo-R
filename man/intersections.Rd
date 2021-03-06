% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi.R
\name{intersections}
\alias{intersections}
\title{Returns strength of correlation between API results}
\usage{
intersections(data, apis = NULL, version = NULL, ...)
}
\arguments{
\item{data}{input data}

\item{apis}{vector of provided string api names}

\item{version}{- api version}

\item{...}{additional arguments to passed to request}

\item{api_key}{your personal indico API key}

\item{cloud}{subdomain for indico private cloud}
}
\value{
nested associated array of correlation strengths
}
\description{
Given an array of data and a vector of apis, returns a nested associate array of correlation strengths
}

