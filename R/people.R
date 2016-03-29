#' Finds the names and titles of people in the input text
#'
#' Finds the names and titles of people in the input text
#' @inheritParams political
#' @return array of dicts, each with 'text', 'confidence', and 'position' fields
#' @people indico.io machine learning API people NER
#' @seealso \code{\link{places}}, \code{\link{organizations}}
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- people("Barack Obama is scheduled to give a talk next Saturday at the White House.")
#' cat(sprintf("%s is mentioned in the input text", result[0]['text']))
#'
people <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'people', version, ...)
}
