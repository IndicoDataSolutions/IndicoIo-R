% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/face_emotion.R
\name{face_emotion}
\alias{face_emotion}
\title{Detects face emotion}
\usage{
face_emotion(img, version = NULL, ...)
}
\arguments{
\item{img}{image data}

\item{version}{for api version}

\item{...}{additional arguments to passed to request}

\item{api_key}{your personal indico API key}

\item{cloud}{subdomain for indico private cloud}
}
\value{
List with face emotions probability pairs
}
\description{
Given a grayscale input image of a face, returns a probability distribution over emotional state.
Input should be a two-dimensional structure (like data.frame or matrix), resizing will be attempted internally but for best
performance, images should be already sized at 48x48 pixels.
}
\examples{
## Example 1
img <- "../tests/testthat/image/image.png"
emotion <- face_emotion(img)

most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
cat(sprintf("Detected '\%s' emotion with probability \%0.4f.\\n",
            names(most.possible)[1], most.possible[1]))
cat(sprintf("Next possible is '\%s' emotion with probability \%0.4f.",
            names(most.possible)[2], most.possible[2]))


}
\seealso{
\code{\link{facial_features}}, \code{\link{image_features}}
}
\keyword{API}
\keyword{emotions}
\keyword{face}
\keyword{indico.io}
\keyword{learning}
\keyword{machine}
\keyword{recognition}

