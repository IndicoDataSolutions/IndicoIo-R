#' Determines how relevant a query string is to a given document
#'
#' Determines how relevant a query string is to a given document
#' @inheritParams political
#' @return float or array of floats
#' @people indico.io machine learning API people NER
#' @seealso \code{\link{keywords}}
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- relevance("Barack Obama", "president")
#' cat(sprintf("%s is mentioned in the input text", result[0]['text']))
#'
relevance <- function(text, queries, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'relevance', version, queries = queries, ...)
}
