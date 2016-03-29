#' Predicts the likelyhood the author of the text is 16 different personas
#'
#' Predicts the likelyhood the author of the text is 16 different personas
#' @inheritParams political
#' @return dict with likelihood the author would be catigorized as each myers briggs axis,
#' and the most likley MB type
#' @personality indico.io machine learning API personality tagging
#' @seealso \code{\link{personality}}, \code{\link{sentiment}}, \code{\link{text_tags}}
#' @export
#' @import httr rjson stringr
#' @examples
#' personas <- personas("Monday: Delightful with mostly sunny skies.
#'                    Highs in the low 70s.")
#' most.possible <- sort(unlist(personas), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected %s persona with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#'
personas <- function(text, version = NULL, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'personality', version, persona = TRUE, ...)
}
