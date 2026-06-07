test_that("join_gdp works", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  efficiency <- compute_cleanup_efficiency(cleaned)
  result <- join_gdp(efficiency)

  expect_s3_class(result, "tbl_df")
  expect_true("gdp_per_capita_nominal" %in% names(result))
  expect_true("iso" %in% names(result))
  expect_equal(result$country[1], "Argentina")
})
