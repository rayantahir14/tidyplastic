test_that("compute_cleanup_efficiency works", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  result <- compute_cleanup_efficiency(cleaned)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("country", "year", "region", "total_plastic", "total_volunteers", "avg_efficiency"))
  expect_equal(result$country[1], "Argentina")
  expect_equal(result$total_plastic[1], 5336)
  expect_equal(result$total_volunteers[1], 66825)
  expect_equal(result$avg_efficiency[1], 5336 / 66825)
})
