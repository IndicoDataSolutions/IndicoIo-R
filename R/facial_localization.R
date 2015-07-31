#' Detects faces
#'
#' Given a grayscale input image, returns a possible locations for faces
#' Input should be a two-dimensional structure (like data.frame or matrix), resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @param img image data
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
#' @return List with face locations
#' @keywords indico.io machine learning API face emotions recognition
#' @seealso \code{\link{facial_features}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr png
#' @examples
#' ## Example 1
#' img <- "../tests/testthat/image/image.png"
#' emotion <- facial_localization(img)
#'
facial_localization <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  options <- list(...)
  img <- format_image(img, FALSE, FALSE)

  make_request(img, 'faciallocalization', api_key, cloud, ...)
}
