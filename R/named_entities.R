#' Named entities with likely categories
#'
#' Given input text, returns named entities and the probabilities of what category they are in
#' @inheritParams political
#' @return List with a list with entitiy category list pairs, the categor list is of category probability pairs
#' @keywords indico.io machine learning API classification tagging
#' @seealso \code{\link{political}}, \code{\link{sentiment}}
#' @export
#' @import httr rjson stringr
#' @examples
#' entities <- named_entities("London Undergroud's boss Mike Brown warned of the strike...")
#' entities

named_entities <- function(text, api_key = FALSE, cloud = FALSE, ...) {

  # Checks parameters
  if (missing(text) || str_trim(text) == "") {
    stop("No text for analysis provided!")
  }

  make_request(text, 'namedentities', api_key, cloud, ...)
}
