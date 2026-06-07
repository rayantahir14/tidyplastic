test_that("clean_plastic_data works", {
  dat <- load_data()
  result <- clean_plastic_data(dat)

  expect_s3_class(result, "tbl_df")
  expect_true("region" %in% names(result))
  expect_true("country_clean" %in% names(result))
})
