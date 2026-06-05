#' Plot Cleanup Efficiency vs Volunteers
#'
#' Produces a ggplot2 scatter plot of cleanup efficiency vs total volunteers,
#' with a linear smoother on log-log axes.
#'
#' @param data A tibble returned by `compute_cleanup_efficiency()`.
#'
#' @return A ggplot object.
#'
#' @importFrom ggplot2 ggplot aes geom_bin2d geom_smooth scale_x_log10 scale_y_log10 labs theme_minimal
#' @export
plot_cleanup_efficiency <- function(data = compute_cleanup_efficiency()) {
  validate_data_input(data, c("total_volunteers", "avg_efficiency"),
                      call_name = "plot_cleanup_efficiency()")
  ggplot(data, aes(x = total_volunteers, y = avg_efficiency)) +
    geom_bin2d() +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "red",
      linewidth = 1
    ) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      title = "Cleanup Efficiency vs Volunteers",
      x = "Total volunteers (log scale)",
      y = "Cleanup efficiency (log scale)",
      fill = "Count"
    ) +
    theme_minimal()
}
