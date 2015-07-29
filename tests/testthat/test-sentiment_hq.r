# context("Positive/Negative Sentiment Analysis: HQ")

# test_that("Sentiment_HQ API returns numerical value", {
#   test_string <- "Worst song ever."
#   emotion <- sentiment_hq(test_string)

#   expect_is(emotion, "numeric")
# })

# test_that("Throws error on empty text", {
#   expect_error(sentiment_hq())
#   expect_error(sentiment_hq(""))
#   expect_error(sentiment_hq(" "))
# })

# test_that("Batch Sentiment API returns a list of numerical value", {
#   test_string_arr <- c("Worst song ever.", "Best song ever.")
#   emotion <- sentiment_hq(test_string_arr)

#   expect_is(emotion, "list")
#   expect_equal(length(emotion), length(test_string_arr))
#   expect_is(emotion[[1]], "numeric")
# })
