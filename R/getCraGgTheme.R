#' Add Project-Specific ggplot Theme
#'
#' This function applies a custom ggplot2 theme with clean, minimal styling
#' suitable for professional reports and presentations. It builds on ggplot2::theme_minimal()
#' with customized legend positioning, axis styling, and improved readability.
#'
#' @param base.size Base font size for all text elements (default: 11).
#' @param base.family Base font family for all text elements (default: "").
#' @param legend.position Position of the legend: "bottom", "top", "left", "right", or "none" (default: "bottom").
#' @param grid.lines Logical indicating whether to show grid lines (default: TRUE).
#' @param axis.titles Logical indicating whether to show axis titles (default: FALSE).
#'
#' @return A ggplot2 theme object that can be added to plots using the + operator.
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   getCraGgTheme()
getCraGgTheme <- function(base.size = 11, base.family = "", legend.position = "bottom",
                          grid.lines = TRUE, axis.titles = FALSE) {

  # Input validation
  if (!is.numeric(base.size) || length(base.size) != 1 || base.size <= 0) {
    stop("Argument 'base.size' must be a single positive number")
  }

  if (!is.character(base.family) || length(base.family) != 1) {
    stop("Argument 'base.family' must be a single character string")
  }

  valid.positions <- c("bottom", "top", "left", "right", "none")
  if (!legend.position %in% valid.positions) {
    stop("Argument 'legend.position' must be one of: ", paste(valid.positions, collapse = ", "))
  }

  if (!is.logical(grid.lines) || length(grid.lines) != 1) {
    stop("Argument 'grid.lines' must be a single logical value")
  }

  if (!is.logical(axis.titles) || length(axis.titles) != 1) {
    stop("Argument 'axis.titles' must be a single logical value")
  }

  # Base theme
  base.theme <- ggplot2::theme_minimal(base_size = base.size, base_family = base.family)

  # Custom theme elements
  custom.theme <- ggplot2::theme(
    # Legend styling
    legend.position = legend.position,
    legend.key.size = grid::unit(0.4, "cm"),
    legend.key.width = grid::unit(0.5, "cm"),
    legend.spacing.x = grid::unit(0.2, "cm"),
    legend.spacing.y = grid::unit(0.1, "cm"),
    legend.margin = ggplot2::margin(t = 0.3, unit = "cm"),
    legend.title = ggplot2::element_text(size = ggplot2::rel(0.9), face = "bold"),
    legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),

    # Axis styling
    axis.title = if (axis.titles) ggplot2::element_text(size = ggplot2::rel(0.9), face = "bold") else ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size = ggplot2::rel(0.8), color = "grey30"),
    axis.ticks = ggplot2::element_line(color = "grey80", linewidth = 0.3),
    axis.ticks.length = grid::unit(0.15, "cm"),

    # Panel styling
    panel.border = ggplot2::element_rect(color = "grey90", fill = NA, linewidth = 0.5),
    panel.grid.major = if (grid.lines) ggplot2::element_line(color = "grey95", linewidth = 0.3) else ggplot2::element_blank(),
    panel.grid.minor = if (grid.lines) ggplot2::element_line(color = "grey98", linewidth = 0.2) else ggplot2::element_blank(),

    # Plot styling
    plot.title = ggplot2::element_text(size = ggplot2::rel(1.2), face = "bold", hjust = 0,
                                       margin = ggplot2::margin(b = 0.5, unit = "cm")),
    plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.0), color = "grey40", hjust = 0,
                                          margin = ggplot2::margin(b = 0.3, unit = "cm")),
    plot.caption = ggplot2::element_text(size = ggplot2::rel(0.7), color = "grey50", hjust = 1,
                                         margin = ggplot2::margin(t = 0.3, unit = "cm")),
    plot.margin = ggplot2::margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),

    # Strip text for faceted plots
    strip.text = ggplot2::element_text(size = ggplot2::rel(0.9), face = "bold",
                                       margin = ggplot2::margin(0.3, 0.3, 0.3, 0.3, unit = "cm")),
    strip.background = ggplot2::element_rect(fill = "grey95", color = "grey90", linewidth = 0.3)
  )

  return(base.theme + custom.theme)
}
