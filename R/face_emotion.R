#' Detects face emotion
#'
#' Given a grayscale input image of a face, returns a probability distribution over emotional state.
#' Input should be a two-dimensional structure (like data.frame or matrix), resizing will be attempted internally but for best
#' performance, images should be already sized at 48x48 pixels.
#' @param img image data
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param ... additional arguments to passed to request
#' @return List with face emotions probability pairs
#' @keywords indico.io machine learning API face emotions recognition
#' @seealso \code{\link{facial_features}}, \code{\link{image_features}}
#' @export
#' @import httr rjson stringr png
#' @examples
#' ## Example 1
#' img <- "../tests/testthat/image/image.png"
#' emotion <- face_emotion(img)
#'
#' most.possible <- sort(unlist(emotion), decreasing = TRUE)[1:2]
#' cat(sprintf("Detected '%s' emotion with probability %0.4f.\n",
#'             names(most.possible)[1], most.possible[1]))
#' cat(sprintf("Next possible is '%s' emotion with probability %0.4f.",
#'             names(most.possible)[2], most.possible[2]))
#'
#'
face_emotion <- function(img, api_key = FALSE, cloud = FALSE, ...) {
  # Checks parameters
  if (missing(img)) {
    stop("No image for analysis provided!")
  }

  options <- list(...)
  img <- format_image(img,
      if(exists("detect", options) && options["detect"]==TRUE) 48 else FALSE
  )

  make_request(img, 'fer', api_key, cloud, ...)
}

#'@export
batch_face_emotion <- function(text, ...) {
    warning("The `batch_face_emotion` function will be deprecated in the next major upgrade. " +
      "Please call `face_emotion` instead with the same arguments")
    face_emotion(text, ...)
}

fer <- face_emotion
