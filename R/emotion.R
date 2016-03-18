#' Detects emotion of the text
#'
#' Given input text, returns a probability distribution over 33 possible
#' languages of what language the text was written in.
#' @inheritParams political
#' @return List with emotion probability pairs
#' @keywords indico.io machine learning API emotion detection
#' @seealso \code{\link{political}}, \code{\link{sentiment}}
#' @export
#' @import httr rjson stringr
#' @examples
#' emotions <- emotion("Monday: Delightful with mostly sunny skies.
#'                             Highs in the low 70s.")
#' emotions
#' most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected %s emotion with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is %s with probability %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
emotion <- function(text,  api_key = FALSE, cloud = FALSE, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'emotion', api_key, cloud, version, ...)
}
