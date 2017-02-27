context("PDF Extraction API")

test_that("PDF Extraction API extracts plain text from urls", {
  pdf_url <- "http://www.cbu.edu.zm/downloads/pdf-sample.pdf"
  result <- pdf_extraction(pdf_url)
  result_keys <- "text metadata"
  for (key in names(result)) {
    expect_true(grepl(key, result_keys))
  }
})

test_that("PDF Extraction API extracts plain text from local PDF documents", {
  pdf_path <- system.file("tests", "testthat", "pdf", "test.pdf", package="indicoio")
  result <- pdf_extraction(pdf_path)
  result_keys <- "text metadata"
  for (key in names(result)) {
    expect_true(grepl(key, result_keys))
  }
})
