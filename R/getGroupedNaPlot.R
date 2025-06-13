#' getGroupedNaPlot: Plot Missing Data by Group
#'
#' Creates a heatmap visualizing the proportion of missing values for each variable,
#' broken down by a specified grouping variable.
#'
#' @param data       data.frame Input dataset.
#' @param group.var  character   Name of the column in \code{data} to group by.
#' @param viridis.pal character   Viridis palette option (A–H). Default is "B".
#'
#' @return A \code{ggplot2} object: heatmap of missing‐value shares.
#'
#' @importFrom dplyr group_by summarise across everything
#' @importFrom tidyr pivot_longer
#' @importFrom rlang sym !!
#' @importFrom ggplot2 ggplot geom_tile scale_fill_gradientn theme_classic theme element_text element_blank labs
#' @importFrom scales viridis_pal
#'
#' @examples
#' # 1) Create sample data with missing values
#' set.seed(2025)
#' df <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 10),
#'   var1  = c(rnorm(10), rep(NA, 10), rnorm(10)),
#'   var2  = c(rnorm(5), rep(NA, 20), rnorm(5))
#' )
#'
#' # 2) Plot missing‐value proportions by group
#' getGroupedNaPlot(df, "group", viridis.pal = "C")
#'
#' @export
getGroupedNaPlot <- function(data, group.var, viridis.pal = "B") {
  ## Input data checks
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.")
  }
  if (!is.character(group.var) || length(group.var) != 1) {
    stop("`group.var` must be a single character string.")
  }
  if (!group.var %in% names(data)) {
    stop(sprintf("Column '%s' not found in `data`.", group.var))
  }
  valid.pals <- toupper(LETTERS[1:8])
  if (!toupper(viridis.pal) %in% valid.pals) {
    stop("`viridis.pal` must be one of 'A' through 'H'.")
  }

  ## Prepare grouping symbol and ensure factor for consistent ordering
  group.sym <- rlang::sym(group.var)
  data <- data |>
    dplyr::mutate(
      !!group.sym := as.factor(!!group.sym)
    )

  ## Compute share of missing values by group
  na.share.df <- data |>
    dplyr::group_by(!!group.sym) |>
    dplyr::summarise(
      dplyr::across(
        .cols  = dplyr::everything(),
        .fns   = function(x) { sum(is.na(x)) / length(x) },
        .names = "{col}"
      ),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(
      cols      = -!!group.sym,
      names_to  = "variable",
      values_to = "share_missing"
    )

  ## Build the heatmap
  plot.obj <- ggplot2::ggplot(na.share.df,
    ggplot2::aes(
      x    = !!group.sym,
      y    = variable,
      fill = share_missing
    )
  ) +
    ggplot2::geom_tile(alpha = 0.9) +
    ggplot2::scale_fill_gradientn(
      colors = scales::viridis_pal(option = toupper(viridis.pal))(10),
      name   = "Share missing",
      limits = c(0, 1)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = 90),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = group.var,
      y = "Variable"
    )

  return(plot.obj)
}
