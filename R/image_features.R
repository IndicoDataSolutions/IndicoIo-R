#' Detects image features
#'
#' Given an input image, returns a 2048 dimensional sparse feature vector explaining that image.
#' Useful as a form of feature engineering for image oriented tasks.
#'
#' * Input can be either grayscale or rgb color and should either be a numpy array or nested list format.
#'
#' * Input data should be either uint8 0-255 range values or floating point between 0 and 1.
#'
#' * Large images (i.e. 1024x768+) are much bigger than needed, resizing will be done internally to 64x64 if needed.
#'
#' * For ideal performance, images should be square aspect ratio but non-square aspect ratios are supported as well.
#' @inheritParams face_emotion
#' @return List with image features
#' @keywords indico.io machine learning API image features recognition
#' @seealso \code{\link{face_emotion}}, \code{\link{facial_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- "../tests/testthat/image/image.png"
#' features <- image_features(img)
#'
#' length(features)
#' min(unlist(features))
#' max(unlist(features))
#' sum(unlist(features))
#'
image_features <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  img <- format_image(img, 64)
  make_request(img, 'imagefeatures', api_key, cloud, ...)
}

#'@export
batch_image_features <- function(text, ...) {
    warning("The `batch_image_features` function will be deprecated in the next major upgrade. " +
      "Please call `image_features` instead with the same arguments")
    image_features(text, ...)
}
