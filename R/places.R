#' Finds the names of places in the input text
#'
#' Finds the names of places in the input text
#' @inheritParams political
#' @return array of dicts, each with 'text', 'confidence', and 'position' fields
#' @people indico.io machine learning API places NER
#' @seealso \code{\link{people}}, \code{\link{organizations}}
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- places("Lets all go to Virginia Beach before it gets too cold to wander outside.)
#' cat(sprintf("%s is mentioned in the input text" % result[0]['text']))
#'
places <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'places', version, ...)
}
