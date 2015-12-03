#' Returns a response from the indico API endpoint
#'
#' Given input data, return the response from the indico API endpoint specified
#' @param data data for analysis
#' @param api api to call
#' @param api_key your personal indico API key
#' @param cloud subdomain for indico private cloud
#' @param version for api version
#' @param ... additional arguments to passed to request
#' @return error or response extracted from the indico API response
#' @keywords indico.io machine learning API
#' @import httr rjson stringr
make_request <- function(data, api, api_key = FALSE, cloud = FALSE, version = NULL, ...) {
  # default to env variables and config file settings
  if (!is.character(cloud) && (cloud == FALSE)) {
    cloud <- .indicoio$cloud
  }
  if (!is.character(api_key) && (api_key == FALSE)) {
    api_key <- .indicoio$api_key
  }

  batch <- typeof(data) == "list" || length(data) > 1

  kwargs <- list(...)
  if (api == "custom" && kwargs[['method']] == "add_data") {
    batch <- typeof(data[[1]]) == "list" || length(data[[1]]) > 1
  }

  # compose the proper request url
  url <- request_url(cloud, api, batch, api_key, version, ...)

  # configure request headers + body
  headers <- add_headers(.indicoio$header)

  kwargs[["apis"]] <- NULL
  kwargs[["data"]] <- data
  body <- toJSON(kwargs)



  response <- POST(url, accept_json(), headers, body = body)

  x_warning <- response[["headers"]][["x-warning"]]
  if (!is.null(x_warning)) {
    warning(x_warning)
  }

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
#' @param version for api version
#' @param api_key your personal indico API key
#' @param apis possible list of apis url paramater for multi qpi requests
#' @param ... additional possible arguments to passed as url parameters
#' @return url for API request
#' @import httr rjson stringr
request_url <- function(cloud, api, batch, api_key, version=NULL, apis=NULL, method=NULL, ...) {
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
  url <- ifelse(is.null(method), url, str_c(url, '/', method))
  url <- str_c(url, '?key=', api_key)

  if (!is.null(apis)) {
      url <- str_c(url, '&apis=')
      url <- str_c(url, paste(apis, collapse=","))
  }

  if (!is.null(version)) {
      url <- str_c(url, '&version=', paste(version))
  }

  url
}

#' Returns a response from the indico API endpoint
#'
#' Given an input image, returns a data.frame obj
#' @param img Image to convert to a data.frame
#' @param size Size of image to resize to
#' @param min_axis Whether or not to keep aspect ratio '
#' @return string base64 encoding of resized image
#' @import httr rjson stringr base64enc EBImage
format_image <- function(img, size, min_axis=FALSE) {
  # Converts to anonymous data.frame
  if (typeof(img) == "list" || length(img) > 1) {
      return(format_images(img, size));
  }

  if (is.character(img)) {
    if (file.exists(img)) {
      img <- readPNG(img)
    } else { # is already base64
      result <- try(readPNG(base64decode(img)), silent=TRUE)
      if (class(result) == "try-error") {
        return(img) # likely a url
      } else {
        img <- result;
      }
    }
  } else {
      stop("Only base64 encoded strings and filepaths are supported for image input.")
  }

  # Check Aspect Ratio
  width = ncol(img);
  height = nrow(img);
  ratio = width / height;
  if (ratio >= 10 || ratio <= .1) {
      warning("For best performance, we recommend images of apsect ratio less than 10:1");
  }

  if (size && width > size && height > size) {
    if (min_axis) {
        if (height > width) {
            new_height <- size;
            new_width <- ratio * size;
        } else {
            new_height <- 1 / ratio * size;
            new_width <- size;
        }
        img <- resize(img, new_height, new_width)
    } else {
        img <- resize(img, size, size)
    }
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
#' @param min_axis Whether or not to keep aspect ratio
#' @return `String`s constructed from list of images as base64 encoded
#' @import httr rjson stringr base64enc EBImage
format_images <- function(imgs, size, min_axis=FALSE) {
  img_list = list()
  for (i in 1:length(imgs)) {
    img <- format_image(imgs[[i]], size, min_axis)
    img_list[[i]] = img
  }
  img_list
}
