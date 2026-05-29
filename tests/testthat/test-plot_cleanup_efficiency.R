test_that("plot_cleanup_efficiency returns a ggplot", {
  dat <- load_data()
  efficiency <- compute_cleanup_efficiency(dat)
  p <- plot_cleanup_efficiency(efficiency)
  expect_s3_class(p, "ggplot")
})
