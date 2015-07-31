#' Tags text with likely keywords
#'
#' Given input text, returns a series of keywords and associated scores
#' @inheritParams political
#' @return List with keyword score pairs
#' @keywords indico.io machine learning API classification tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}, \code{\link{text_tags}}
#' @export
#' @import httr rjson stringr
#' @examples
#' keywords <- keywords("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' keywords
#' most.possible <- sort(unlist(keywords), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected keyword %s with a score of %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is %s with a score %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
keywords <- function(text, api_key = FALSE, cloud = FALSE, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'keywords', api_key, cloud, ...)
}

#'@export
batch_keywords <- function(text, ...) {
    warning("The `batch_keywords` function will be deprecated in the next major upgrade. " +
      "Please call `keywords` instead with the same arguments")
    keywords(text, ...)
}
