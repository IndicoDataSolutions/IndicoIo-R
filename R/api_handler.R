#' Returns a response from the indico API endpoint
#'
#' Given input data, return the response from the indico API endpoint specified
#' @param data data for analysis
#' @param api api to call
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param batch send call to batch endpoint
#' @param ... additional arguments to passed to request
#' @return error or response extracted from the indico API response
#' @keywords indico.io machine learning API
#' @import httr rjson stringr
make_request <- function(data, api, api_key = FALSE, cloud = FALSE, ...) {
  # default to env variables and config file settings
  if (!is.character(cloud) && (cloud == FALSE)) {
    cloud <- .indicoio$cloud
  }
  if (!is.character(api_key) && (api_key == FALSE)) {
    api_key <- .indicoio$api_key
  }

  batch <- typeof(data) == "list" || length(data) > 1

  # compose the proper request url
  url <- request_url(cloud, api, batch, api_key, ...)

  # configure request headers + body
  headers <- add_headers(.indicoio$header)
  body <- toJSON(list(data = data, ...))

  response <- POST(url, accept_json(), headers, body = body)

  stop_for_status(response)

  # Returns results
  answer <- content(response, as = "parsed", type = "application/json")
  if ("error" %in% names(answer)) {
    stop(answer[["error"]])
  }

  answer[["results"]]
}

#' Returns the url for the proper indico API
#'
#' Produces the proper url to query with a given request
#' @param cloud (optional) indico subdomain for private cloud
#' @param api name of API
#' @param batch (logical) does the request contain more than one example?
#' @param api_key your personal indico API key
#' @param apis possible list of apis url paramater for multi qpi requests
#' @param ... additional possible arguments to passed as url parameters
#' @return url for API request
#' @import httr rjson stringr
request_url <- function(cloud, api, batch, api_key, apis=NULL, ...) {
  # compose the proper request url
  if (cloud != FALSE && cloud != "") {
    private_cloud <- sprintf("https://%s.indico.domains/", cloud)
    base_url <- private_cloud
  } else {
    base_url <- .indicoio$remote_api
  }

  if (!is.character(api_key) && (api_key == FALSE)) {
    stop("Please provide an api key.")
  }
  url <- str_c(base_url, api)
  url <- ifelse(batch, str_c(url, '/batch'), url)
  url <- str_c(url, '?key=', api_key)

  if (!is.null(apis)) {
      url <- str_c(url, '&apis=')
      url <- str_c(url, paste(apis, collapse=","))
  }

  url
}

#' Returns a response from the indico API endpoint
#'
#' Given an input image, returns a data.frame obj
#' @param img Image to convert to a data.frame
#' @param size Size of image to resize to
#' @return string base64 encoding of resized image
#' @import httr rjson stringr base64enc EBImage
format_image <- function(img, size) {
  # Converts to anonymous data.frame
  if (is.character(img)) {
    if (file.exists(img)) {
      img <- readPNG(img)
    } else { # is already base64
      img <- base64decode(img)
      img <- readPNG(img)
    }
  } else if (typeof(img) == "list" || length(img) > 1) {
      return(format_images(img, size));
  } else {
      stop("Only base64 encoded strings and filepaths are supported for image input.")
  }

  if (size && nrow(img) > size && ncol(img) > size) {
    img <- resize(img, size, size)
  }

  if (any(is.na(img))) {
      stop("Invalid input!")
  }
  vector <- writePNG(img)
  b64 <- base64encode(vector)
  b64
}

#' Returns a list of `data.frame`s given a list of input images
#'
#' Given a list of input images, returns a list of `string`s
#' @param imgs List of images to convert to a `string`s
#' @param size Size of image to resize to
#' @return `String`s constructed from list of images as base64 encoded
#' @import httr rjson stringr base64enc EBImage
format_images <- function(imgs, size) {
  img_list = list()
  for (i in 1:length(imgs)) {
    img <- format_image(imgs[[i]], size)
    img_list[[i]] = img
  }
  img_list
}
