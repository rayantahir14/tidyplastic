test_that("plot_cleanup_efficiency returns a ggplot", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  efficiency <- compute_cleanup_efficiency(cleaned)
  p <- plot_cleanup_efficiency(efficiency)
  expect_s3_class(p, "ggplot")
})
