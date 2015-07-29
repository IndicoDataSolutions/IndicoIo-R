#' Detects NSFW content in an image
#'
#' Given an input image, returns a float probability between 0 and 1 that content is NSFW
#'
#' * Input can be either grayscale or rgb color and should either be a numpy array or nested list format.
#'
#' * Input data should be either uint8 0-255 range values or floating point between 0 and 1.
#'
#' * Large images (i.e. 1024x768+) are much bigger than needed, resizing will be done internally to 64x64 if needed.
#'
#' * For ideal performance, images should be square aspect ratio but non-square aspect ratios are supported as well.
#' @inheritParams face_emotion
#' @return Float with probability of NSFW
#' @keywords indico.io machine learning API image content filtering
#' @seealso \code{\link{face_emotion}}, \code{\link{facial_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img <- matrix(runif(64*64, 0, 1), nrow = 64)
#' probabilities <- content_filtering(img)
#'
content_filtering <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  if (!is.character(img) && length(dim(img)) != 2) {
    stop("Image should be represented by two-dimensional structure!")
  }

  img <- format_image(img, 128, TRUE)
  make_request(img, 'contentfiltering', api_key, cloud, ...)
}

#' Detects NSFW content in a list of mages
#'
#' Given a list of input images,  returns a list of float probabilities between 0 and 1 that content is NSFW
#'
#' * Input can be either grayscale or rgb color and should either be a numpy array or nested list format.
#'
#' * Input data should be either uint8 0-255 range values or floating point between 0 and 1.
#'
#' * Large images (i.e. 1024x768+) are much bigger than needed, resizing will be done internally to 64x64 if needed.
#'
#' * For ideal performance, images should be square aspect ratio but non-square aspect ratios are supported as well.
#' @inheritParams batch_face_emotion
#' @return List of Floats with probabilities of NSFW
#' @keywords indico.io machine learning API image content filtering
#' @seealso \code{\link{face_emotion}}, \code{\link{facial_features}}
#' @export
#' @import httr rjson stringr
#' @examples
#' img_list = list()
#' img_list[[1]] = matrix(runif(64*64, 0, 1), nrow = 64)
#' probabilities <- batch_content_filtering(img_list)
#'
batch_content_filtering <- function(imgs, api_key = FALSE, cloud = FALSE, ...) {
  img_list <- format_images(imgs, 128, TRUE)
  make_request(img_list, 'contentfiltering', api_key, cloud, batch = TRUE, ...)
}
