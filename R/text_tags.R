#' Tags text with likely categories
#'
#' Given input text, returns a probability distribution over 111 possible topics
#' @inheritParams political
#' @return List with text tag probability pairs
#' @keywords indico.io machine learning API classification tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}
#' @export
#' @import httr rjson stringr
#' @examples
#' categories <- text_tags("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' categories
#' most.possible <- sort(unlist(categories), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected category %s with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is %s with probability %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
text_tags <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'texttags', version, ...)
}
