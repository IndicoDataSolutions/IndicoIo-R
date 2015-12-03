context("personality")

categories <-c(
  "openness",
  "extraversion",
  "conscientiousness",
  "agreeableness"
)
test_that("personality API returns dict with correct categories", {
  test_string <- "I love my friends!"
  personality <- personality(test_string)

  expect_is(personality, "list")
  expect_true(all(names(personality) %in% categories))
})

test_that("Throws error on empty text", {
  expect_error(personality())
  expect_error(personality(""))
  expect_error(personality(" "))
})

test_that("Batch personality API returns list of list of scores for the different personality axis", {
  test_string_arr <- c("I love my friends!", "I like to be alone.")
  personality <- personality(test_string_arr)

  expect_is(personality, "list")
  expect_is(personality[[1]], "list")
  expect_true(all(names(personality[[1]]) %in% categories))
})
