context("keywords")

test_that("Keywords API returns list of scores for the list of keywords", {
  test_string <- "Hoping for sunshine tomorrow."
  keywords <- keywords(test_string, top_n = 1)

  expect_is(keywords, "list")
  expect_equal(length(keywords), 1)
})

test_that("Multilingual Autodetect Keywords API returns list of scores for the list of keywords", {
  test_string <- "La semaine suivante, il remporte sa premiere victoire, dans la descente de Val Gardena en Italie, près de cinq ans après la dernière victoire en Coupe du monde d'un Français dans cette discipline, avec le succès de Nicolas Burtin à Kvitfjell."
  keywords <- keywords(test_string, language="detect")
  expect_is(keywords, "list")
  expect_equal(length(keywords), 3)
})

test_that("Multilingual Keywords API returns list of scores for the list of keywords", {
  test_string <- "La semaine suivante, il remporte sa premiere victoire, dans la descente de Val Gardena en Italie, près de cinq ans après la dernière victoire en Coupe du monde d'un Français dans cette discipline, avec le succès de Nicolas Burtin à Kvitfjell."
  keywords <- keywords(test_string, language="French")

  expect_is(keywords, "list")
  expect_equal(length(keywords), 3)
})

test_that("Throws error on empty text", {
  expect_error(keywords())
  expect_error(keywords(""))
  expect_error(keywords(" "))
})

test_that("Batch Keywords API returns list of list of probabilities for the list of topics", {
  test_string_arr <- c("Hoping for sunshine tomorrow.", "Hoping for rain tomorrow.")
  keywords <- keywords(test_string_arr)

  expect_is(keywords, "list")
  expect_is(keywords[[1]], "list")
  expect_equal(length(keywords), 2)
  expect_equal(length(keywords[[1]]), 3)
})
