context("Face Emotion Detection")

test_that("Face Emotion Detection API returns proper answer", {
  fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  emotion <- face_emotion(test_image)

  expect_is(emotion, "list")
  expect_true(all(names(emotion) %in% fer_set))
  expect_equal(sum(unlist(emotion)), 1)
})

test_that("FER alias returns proper answer", {
  fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
  test_image <- paste(readLines("image/base64.txt"), collapse=" ")
  emotion <- face_emotion(test_image)

  expect_is(emotion, "list")
  expect_true(all(names(emotion) %in% fer_set))
  expect_equal(sum(unlist(emotion)), 1)
})

test_that("Throws error on empty or wrong image", {
  expect_error(face_emotion())
  expect_error(face_emotion(array(10)))
})

test_that("Batch Face Emotion Detection API returns proper answer", {
  fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
  test_image_list = list()
  test_image_list[[1]] <- paste(readLines("image/base64.txt"), collapse=" ")
  emotion <- face_emotion(test_image_list)

  expect_is(emotion, "list")
  expect_is(emotion[[1]], "list")
  expect_true(all(names(emotion[[1]]) %in% fer_set))
  expect_equal(sum(unlist(emotion[[1]])), 1)
})

test_that("Batch Face Emotion Detection API returns proper answer", {
  fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
  test_image_list = list()
  test_image_list[[1]] <-  system.file("extdata", "face1.png", package = "indicoio")
  emotion <- face_emotion(test_image_list)
  expect_is(emotion, "list")
  expect_is(emotion[[1]], "list")
  expect_more_than(emotion[[1]][["Happy"]], 0.49)
  expect_true(all(names(emotion[[1]]) %in% fer_set))
  expect_equal(sum(unlist(emotion[[1]])), 1)
})

test_that("Batch Face Emotion Detection API with Base64 returns proper answer", {
  fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
  test_image_list = list()
  test_image_list[[1]] <-  readFile(system.file("extdata", "base64.txt", package = "indicoio"))
  emotion <- face_emotion(test_image_list)
  expect_is(emotion, "list")
  expect_is(emotion[[1]], "list")
  expect_more_than(emotion[[1]][["Happy"]], 0.49)
  expect_true(all(names(emotion[[1]]) %in% fer_set))
  expect_equal(sum(unlist(emotion[[1]])), 1)
})
