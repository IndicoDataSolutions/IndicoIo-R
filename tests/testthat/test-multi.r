context("Multi API")

test_that("Predict Text returns appropriate response", {
    test_string <- "Clearly an English sentence."
    results <- predict_text(test_string, apis=c("sentiment", "language"))

    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
    expect_true("language" %in% names(results))
    expect_true("Romanian" %in% names(results[["language"]]))
})

test_that("Throws error on empty text", {
    expect_error(predict_text())
    expect_error(predict_text(""))
    expect_error(predict_text(" "))
})

test_that("Batch Predict Text returns appropriate response", {
    test_string_arr <- c("Clearly an English sentence.", "Clearly not an English sentence.")
    results <- predict_text(test_string_arr, apis=c("sentiment", "language"))

    expect_is(results, "list")
    expect_true("sentiment" %in% names(results))
    expect_true("language" %in% names(results))
    expect_equal(length(results[["sentiment"]]), 2)
    expect_equal(length(results[["language"]]), 2)
})

test_that("Predict Image returns appropriate response", {
    fer_set <- c("Angry", "Sad", "Neutral", "Surprise", "Fear", "Happy")
    test_image <- paste(readLines("image/base64.txt"), collapse=" ")
    results <- predict_image(test_image, apis=c("facial_features", "face_emotion"))

    expect_is(results, "list")
    expect_true("facial_features" %in% names(results))
    expect_true("face_emotion" %in% names(results))
    expect_true(all(names(results[["face_emotion"]]) %in% fer_set))
})

test_that("Throws error on empty or wrong image", {
  expect_error(predict_image())
  expect_error(predict_image(array(10)))
})

test_that("Batch Predict Image returns appropriate response", {
    test_image_list = list()
    test_image_list[[1]] <- paste(readLines("image/base64.txt"), collapse=" ")
    test_image_list[[2]] <- paste(readLines("image/base64.txt"), collapse=" ")
    results <- predict_image(test_image_list, apis=c("facial_features", "face_emotion"))

    expect_is(results, "list")
    expect_true("facial_features" %in% names(results))
    expect_true("face_emotion" %in% names(results))
    expect_equal(length(results[["facial_features"]]), 2)
    expect_equal(length(results[["face_emotion"]]), 2)
})
