context("Twitter Engagement Analysis")

test_that("Twitter Engagement API returns float engagement score", {
  test_string <- "Guns don't kill people, people kill people."
  engagement <- twitter_engagement(test_string)

  expect_is(engagement, "numeric")
  expect_true(engagement <= 1)
  expect_true(engagement >= 0)
})

test_that("Throws error on empty text", {
  expect_error(twitter_engagement())
  expect_error(twitter_engagement(""))
  expect_error(twitter_engagement(" "))
})

test_that("Batch Twitter Engagement API returns list of float engagement scores", {
  test_string_arr <- c("Guns don't kill people, people kill people.", "Let's all be friends.")
  engagement <- batch_twitter_engagement(test_string_arr)
  expect_is(engagement, "list")
  expect_is(engagement[[1]], "numeric")
  expect_true(engagement[[1]] <= 1)
  expect_true(engagement[[2]] >= 0)
})
