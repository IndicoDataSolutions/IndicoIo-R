context("Face Localization Detection")

test_that("Face Emotion Detection API returns proper answer", {
  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  faces <- facial_localization(test_image)

  expect_is(faces, "list")
  expect_true(length(faces[[1]][["top_left_corner"]]) > 1)
})

test_that("Throws error on empty or wrong image", {
  expect_error(facial_localization())
  expect_error(facial_localization(array(10)))
})

test_that("Batch Face Emotion Detection API returns proper answer", {
  test_image_list = list()
  test_image_list[[1]] <- paste(readLines("image/base64.txt"), collapse=" ")
  faces <- facial_localization(test_image_list)

  expect_is(faces, "list")
  expect_is(faces[[1]], "list")
  expect_true(length(faces[[1]][[1]][["top_left_corner"]]) > 1)
})
