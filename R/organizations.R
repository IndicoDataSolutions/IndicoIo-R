#' Finds the names of organizations in the input text
#'
#' Finds the names of organizations in the input text
#' @inheritParams political
#' @return array of dicts, each with 'text', 'confidence', and 'position' fields
#' @people indico.io machine learning API people NER
#' @seealso \code{\link{places}}, \code{\link{organizations}}
#' @export
#' @import httr rjson stringr
#' @examples
#' result <- organizations("A year ago, the New York Times published confidential comments about ISIS' ideology by Major General Michael K. Nagata, then U.S. Special Operations commander in the Middle East.")
#' cat(sprintf("%s is mentioned in the input text", result[[1]]["text"]))
#'
organizations <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'organizations', version, ...)
}
