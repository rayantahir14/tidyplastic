test_that("test_gdp_plastic_association works", {
  dat <- load_data()
  cleaned <- clean_plastic_data(dat)
  efficiency <- compute_cleanup_efficiency(cleaned)
  enriched <- join_gdp(efficiency)

  expect_error(test_gdp_plastic_association(enriched))
})
