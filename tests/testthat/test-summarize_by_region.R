test_that("summarize_by_region works", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  result <- summarize_by_region(cleaned)

  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("region", "total_plastic", "total_events", "avg_efficiency"))
  expect_equal(result$total_plastic[result$region == "Africa"], 176684)
  expect_equal(result$total_events[result$region == "Africa"], 11830)
  expect_equal(result$total_plastic[result$region == "Asia"], 514339)
})
