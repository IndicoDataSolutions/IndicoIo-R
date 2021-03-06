context("Using API Versions")

test_that("Specifying a version still works", {
  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  faces <- facial_localization(test_image, version="1")

  expect_is(faces, "list")
  expect_true(length(faces[[1]][["top_left_corner"]]) > 1)
})

test_that("Specifying a version for image features v2", {
  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  features <- image_features(test_image, version="2")
  expect_is(features, "list")
  expect_true(length(features) == 4096)
})
