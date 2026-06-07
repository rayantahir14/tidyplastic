test_that("plot_gdp_efficiency returns a plotly object", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  efficiency <- compute_cleanup_efficiency(cleaned)
  enriched <- join_gdp(efficiency)
  p <- plot_gdp_efficiency(enriched)

  expect_s3_class(p, "plotly")
})
