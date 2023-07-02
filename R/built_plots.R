

#' Built waterfall plot using ggplot2.
#'
#' @param data_frame Data frame.
#' @param x_axis X axis title.
#' @param y_axis Y axis title.
#'
#' @return Waterfall plot.
#'
built_waterfall_plot <- function(data_frame, x_axis, y_axis) {

  data_frame$desc <- factor(data_frame$desc, levels = data_frame$desc)
  data_frame$id <- seq_along(data_frame$amount)
  data_frame$type <- ifelse(data_frame$amount > 0, "in", "out")
  data_frame[data_frame$desc %in% c("Balance"), "type"] <- "net"

  data_frame$end <- cumsum(data_frame$amount)
  data_frame$end <- c(head(data_frame$end, -1), 0)
  data_frame$start <- c(0, head(data_frame$end, -1))
  data_frame <- data_frame[, c(3, 1, 4, 6, 5, 2)]

  data_frame$type <- factor(data_frame$type, levels = c("out", "in", "net"))

  strwr <- function(str) gsub(" ", "\n", str)

  waterfall_plot <- ggplot2::ggplot(data_frame, ggplot2::aes(fill = type)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_rect(ggplot2::aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) +
    ggplot2::scale_y_continuous(" ") +
    ggplot2::scale_x_discrete(" ", breaks = levels(data_frame$desc), labels = strwr(levels(data_frame$desc))) +
    ggplot2::theme(legend.position = "none",
                   title = ggplot2::element_text(size = 9),
                   axis.text.y = ggplot2::element_text(size = 8),
                   axis.title.x = ggplot2::element_text(size = 8),
                   axis.title.y = ggplot2::element_text(size = 8),
                   strip.text.x = ggplot2::element_text(size = 8),
                   strip.text.y = ggplot2::element_text(size = 8),
                   axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1, size = 8)) +
    ggplot2::geom_text(ggplot2::aes(x = desc, y = end, label = round(amount, 1)), position = ggplot2::position_dodge(2), size = 3) +
    ggplot2::xlab(x_axis) + ggplot2::ylab(y_axis) + ggplot2::ggtitle(y_axis)

  return(waterfall_plot)

}

#' Built waterfall plot using plotly.
#'
#' @param data_frame Data frame.
#' @param nutrient_balance Nutrient balance.
#' @param x_axis_title X axis title.
#'
#' @return Waterfall plot Plotly.

built_waterfall_plot_plotly <- function(data_frame, nutrient_balance, x_axis_title) {

  fig <- plotly::plot_ly(data_frame,
                         x = ~x,
                         y = ~y,
                         measure = ~measure,
                         text = ~ x,
                         type = "waterfall",
                         orientation = "h",
                         totals = list(marker = list(color = ifelse(nutrient_balance < 0, "blue", "green"))),
                         connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0)))
  fig <- fig %>%
    plotly::layout(
      xaxis = list(title = x_axis_title, tickfont = "16", ticks = "outside"),
      yaxis = list(title = "", type = "category", autorange = "reversed"),
      xaxis = list(title = "", type = "linear"),
      showlegend = FALSE) %>%
    plotly::config(displayModeBar = FALSE)

  return(fig)

}

#' Built gauge using plotly.
#'
#' @param carbon_equivalent Carbon equivalent.
#'
#' @return Gauge.

built_gauge_plotly <- function(carbon_equivalent) {

  fig <- plotly::plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = carbon_equivalent,
    title = list(text = "CO2eq/kgMilk"),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = 0.99,
                 increasing = list(color = "red"),
                 decreasing = list(color = "green")),
    gauge = list(
      axis = list(range = list(NULL, 2)),
      bar = list(color = "darkblue"),
      steps = list(
        list(range = c(0, 0.76),   color = "lightgreen"),
        list(range = c(0.76, 1.37), color = "orange"),
        list(range = c(1.37, 2), color = "red")),
      threshold = list(
        line = list(color = "black", width = 6),
        thickness = 0.75,
        value = 0.99))) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::layout(showlegend = FALSE)

  return(fig)

}

#' Built ggplot2 plot for variables over the lactation.
#'
#' @param data_frame Data frame.
#' @param x_variable X axis variable.
#' @param y_variable Y axis variable.
#' @param col_variable Color variable.
#' @param y_title Y axis title.
#'
#' @return Ggplot2 plot.

built_variable_over_lactation_plot <- function(data_frame, x_variable, y_variable, col_variable, y_title) {

  plot <- ggplot2::ggplot(data_frame, ggplot2::aes(x = .data[[x_variable]], y = .data[[y_variable]], col = .data[[col_variable]])) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::xlab("Months in lactation") + ggplot2::ylab(" ") + ggplot2::ggtitle(y_title) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10))

  return(plot)

}

#' Built ggplot2 over the year.
#'
#' @param data_frame Data frame.
#' @param x_variable X axis variable.
#' @param y_variable Y xis variable.
#' @param y_title Y axis title.
#' @param plot_title Plot title.
#'
#' @return Ggplor2 plot.

built_variable_over_year_plot <- function(data_frame, x_variable, y_variable, y_title, plot_title) {

  ggplot2::ggplot( data_frame, ggplot2::aes( x = .data[[x_variable]], y = .data[[y_variable]]) ) +
    ggplot2::theme_bw() +
    ggplot2::geom_point(col = "blue") +
    ggplot2::geom_line(ggplot2::aes( x = .data[[x_variable]], y = .data[[y_variable]]), col = "black") +
    ggplot2::xlab("Year days") + ggplot2::ylab(y_title) + ggplot2::ggtitle(plot_title) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10))

}





