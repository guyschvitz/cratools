#' Add Project-Specific ggplot Theme
#'
#' This function applies a custom ggplot2 theme with clean, minimal styling
#' suitable for professional reports and presentations. It builds on theme_minimal()
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
#'
#' # Basic usage
#' ggplot(mtcars, aes(x = mpg, y = hp)) +
#'   geom_point() +
#'   getCraGgTheme()
#'
#' # With custom parameters
#' ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
#'   geom_boxplot() +
#'   getCraGgTheme(base.size = 12, legend.position = "right")
#'
#' # With axis titles enabled
#' ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
#'   geom_point() +
#'   labs(x = "Miles per Gallon", y = "Horsepower", color = "Cylinders") +
#'   getCraGgTheme(axis.titles = TRUE)
#'
#' # Without grid lines for cleaner look
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   getCraGgTheme(grid.lines = FALSE, legend.position = "none")
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
  base.theme <- theme_minimal(base_size = base.size, base_family = base.family)

  # Custom theme elements
  custom.theme <- theme(
    # Legend styling
    legend.position = legend.position,
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.spacing.x = unit(0.2, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.margin = margin(t = 0.3, unit = "cm"),
    legend.title = element_text(size = rel(0.9), face = "bold"),
    legend.text = element_text(size = rel(0.8)),

    # Axis styling
    axis.title = if (axis.titles) element_text(size = rel(0.9), face = "bold") else element_blank(),
    axis.text = element_text(size = rel(0.8), color = "grey30"),
    axis.ticks = element_line(color = "grey80", linewidth = 0.3),
    axis.ticks.length = unit(0.15, "cm"),

    # Panel styling
    panel.border = element_rect(color = "grey90", fill = NA, linewidth = 0.5),
    panel.grid.major = if (grid.lines) element_line(color = "grey95", linewidth = 0.3) else element_blank(),
    panel.grid.minor = if (grid.lines) element_line(color = "grey98", linewidth = 0.2) else element_blank(),

    # Plot styling
    plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0,
                              margin = margin(b = 0.5, unit = "cm")),
    plot.subtitle = element_text(size = rel(1.0), color = "grey40", hjust = 0,
                                 margin = margin(b = 0.3, unit = "cm")),
    plot.caption = element_text(size = rel(0.7), color = "grey50", hjust = 1,
                                margin = margin(t = 0.3, unit = "cm")),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),

    # Strip text for faceted plots
    strip.text = element_text(size = rel(0.9), face = "bold",
                              margin = margin(0.3, 0.3, 0.3, 0.3, unit = "cm")),
    strip.background = element_rect(fill = "grey95", color = "grey90", linewidth = 0.3)
  )

  return(base.theme + custom.theme)
}

