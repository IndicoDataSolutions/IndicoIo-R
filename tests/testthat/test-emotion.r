context("Emotion Detection")

emotion_set <- c(
  "anger",
  "sadness",
  "joy",
  "fear",
  "surprise"
)

test_that("Emotion API returns list of probabilities for the list of emotions", {
  test_string <- "I am sad. So sad. I am crying :["
  emotions <- emotion(test_string)

  expect_is(emotions, "list")
  expect_true(all(names(emotions) %in% emotion_set))
  expect_equal(sum(unlist(emotions)), 1)
})

test_that("Throws error on empty text", {
  expect_error(emotion())
  expect_error(emotion(""))
  expect_error(emotion(" "))
})

test_that("Batch Emotion API returns list of list of probabilities for the list of emotions", {
  test_string_arr <- c("I am sad. So sad. I am crying :[", "I am sad. So sad. I am crying :[")
  emotions <- emotion(test_string_arr)

  expect_is(emotions, "list")
  expect_is(emotions[[1]], "list")
  expect_true(all(names(emotions[[1]]) %in% emotion_set))
  expect_equal(sum(unlist(emotions[[1]])), 1)
})
