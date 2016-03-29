context("Utility function tests")

test_that("request_url functions as expected", {
  url <- request_url(cloud = FALSE, api = "sentiment", batch = FALSE, api_key = "key")
  expect_equal(url, "https://apiv2.indico.io/sentiment")

  url <- request_url(cloud = FALSE, api = "sentiment", batch = TRUE, api_key = "key")
  expect_equal(url, "https://apiv2.indico.io/sentiment/batch")

  url <- request_url(cloud = FALSE, api = "political", batch = TRUE, api_key = "key")
  expect_equal(url, "https://apiv2.indico.io/political/batch")

  url <- request_url(cloud = "testing", api = "political", batch = TRUE, api_key = "key")
  expect_equal(url, "https://testing.indico.domains/political/batch")

  .indicoio$private_cloud = "https://%s.indico.com/"
  url <- request_url(cloud = "testing", api = "political", batch = TRUE, api_key = "key")
  expect_equal(url, "https://testing.indico.domains/political/batch")
})

test_that("format_image functions as expected", {
  img <- paste(readLines("image/base64.txt"), collapse=" ")
  result <- format_image(img, 48)
  expect_is(result, "character")
});

test_that("format_images functions as expected", {
  img_list = list()
  img_list[[1]] = paste(readLines("image/base64.txt"), collapse=" ")
  result <- format_images(img_list, 48)
  expect_is(result, "list")
  expect_equal(length(result), 1)
  expect_is(result[[1]], "character")
});

test_that("min_resize functions as expected", {
    img <- "image/not_square.png"
    result <- format_image(img, 48, TRUE)

    decoded_img <- base64decode(result)
    decoded_img <- readPNG(decoded_img)
    width <- ncol(decoded_img)
    height <- nrow(decoded_img)

    expect_equal(height > 27, TRUE)
    expect_equal(height < 30, TRUE)
    expect_equal(width, 48)
});

test_that("auth configuration is loaded from environment variables", {
  # store previous config
  prev_api_key <- Sys.getenv("INDICO_API_KEY", unset = FALSE)

  api_key <- "env-api-key"
  Sys.setenv('INDICO_API_KEY' = api_key)
  loadEnvironmentVars()
  expect_equal(.indicoio$api_key, api_key)

  # restore after testing
  Sys.setenv('INDICO_API_KEY' = prev_api_key)
  loadEnvironmentVars()
})

test_that("cloud configuration is loaded from environment variables", {
  # store previous config
  prev_cloud <- Sys.getenv("INDICO_CLOUD", unset = FALSE)

  cloud <- "env-cloud"
  Sys.setenv('INDICO_CLOUD' = cloud)
  loadEnvironmentVars()
  expect_equal(.indicoio$cloud, cloud)

  # restore after testing
  Sys.setenv('INDICO_CLOUD' = prev_cloud)
  loadEnvironmentVars()
})

test_that("auth configuration is loaded from the configuration files", {
  api_key <- "file-api-key"
  content <- sprintf(
    "[auth]\napi_key = %s",
    api_key
  )
  loadConfigFile(content)
  expect_equal(.indicoio$api_key, api_key)

  # reset to defaults
  .indicoio$api_key <- FALSE
  .indicoio$cloud <- FALSE
  loadConfiguration()
})

test_that("cloud configuration is loaded from the configuration files", {
  cloud <- "file-cloud"
  content <- sprintf("[private_cloud]\ncloud = %s", cloud)
  loadConfigFile(content)
  expect_equal(.indicoio$cloud, cloud)

  # reset to defaults
  .indicoio$auth <- FALSE
  .indicoio$cloud <- FALSE
  loadConfiguration()
})

test_that("env variables take precedence over config files", {
  prev_cloud <- Sys.getenv("INDICO_CLOUD", unset = FALSE)

  file_cloud <- "file-cloud"
  env_cloud <- "env-cloud"
  content <- sprintf("[private_cloud]\ncloud = %s", file_cloud)
  Sys.setenv('INDICO_CLOUD' = env_cloud)
  loadConfigFile(content)
  loadEnvironmentVars()
  expect_equal(.indicoio$cloud, env_cloud)

  # reset to defaults
  .indicoio$auth <- FALSE
  .indicoio$cloud <- FALSE
  Sys.setenv('INDICO_CLOUD' = prev_cloud)
  loadConfiguration()
})
