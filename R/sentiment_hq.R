#' Returns a scalar estimate of the sentiment of the text
#'
#' Given input text, returns a scalar estimate of the sentiment of that text.
#' @inheritParams political
#' @return Numerical value roughly in the range from 0 to 1 with 0.5 indicating neutral sentiment.
#' For reference, 0 suggests very negative sentiment and 1 suggests very positive sentiment.
#' @keywords indico.io machine learning API sentiment analysis
#' @seealso \code{\link{political}}, \code{\link{language}}
#' @export
#' @import httr rjson stringr
#' @examples
#' emotion <- sentiment_hq("Thanks everyone for the birthday wishes!!
#'                       It was a crazy few days ><")
#' emotion
#' cat(sprintf("This text has %s tonality",
#'              ifelse(emotion > 0.5, "positive", "negative")))
#'
sentiment_hq <- function(text, version=NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, "sentimenthq", version, ...)
}
