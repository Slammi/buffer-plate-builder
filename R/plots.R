# Plate-map heatmaps.

#' Values that can be mapped: each source, fixed mix, water and fraction used.
plate_map_choices <- function(result) {
  c(setdiff(names(result$volumes), c("run", "replicate", "plate", "well", "row", "col",
                                     "fixed", "total", "fraction_used", "status",
                                     "largest_component", "below_min_sources")),
    if (result$settings$fixed_volume > 0) "fixed",
    "fraction_used")
}

#' Heatmap of one value per well (µL, or % for fraction_used). Failing wells
#' are outlined red and below-minimum wells orange; unused wells are blank.
plot_plate_map <- function(result, value, plates = NULL) {
  s <- result$settings
  d <- plate_dims(s$plate_format)
  vol <- result$volumes
  if (!value %in% names(vol)) stop("Unknown plate map value: ", value)
  if (is.null(plates)) plates <- sort(unique(vol$plate))

  grid <- expand.grid(col = seq_len(d[["cols"]]), row = seq_len(d[["rows"]]), plate = plates)
  dat <- merge(grid, vol[, c("plate", "row", "col", "run", "status", value)],
               by = c("plate", "row", "col"), all.x = TRUE)
  dat$value <- dat[[value]]
  if (value == "fraction_used") dat$value <- 100 * dat$value
  dat$plate <- factor(paste("Plate", dat$plate), levels = paste("Plate", plates))
  flagged <- dat[!is.na(dat$status) & dat$status != "OK", ]

  label <- switch(value, fraction_used = "% used\nbefore water", water = "Water (µL)",
                  fixed = "Fixed (µL)", paste0(value, " (µL)"))
  p <- ggplot2::ggplot(dat, ggplot2::aes(col, row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "grey85", linewidth = 0.3) +
    ggplot2::geom_tile(data = flagged, ggplot2::aes(colour = status), fill = NA, linewidth = 1.1,
                       width = 0.86, height = 0.86) +
    ggplot2::scale_fill_viridis_c(name = label, na.value = "grey97", option = "D") +
    ggplot2::scale_colour_manual(name = NULL, values = c("OVER VOLUME" = "red", "BELOW MIN" = "orange"),
                                 drop = FALSE, limits = c("OVER VOLUME", "BELOW MIN")) +
    ggplot2::scale_x_continuous(breaks = seq_len(d[["cols"]]), expand = c(0, 0), position = "top") +
    ggplot2::scale_y_reverse(breaks = seq_len(d[["rows"]]), labels = LETTERS[seq_len(d[["rows"]])],
                             expand = c(0, 0)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, title = label) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right",
                   plot.title = ggplot2::element_blank())
  if (s$plate_format == 96) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(value), "", formatC(value, format = "g", digits = 3))),
                                size = 2.8, colour = ifelse(!is.na(dat$value) & dat$value > stats::median(dat$value, na.rm = TRUE), "black", "white"))
  }
  if (length(plates) > 1) p <- p + ggplot2::facet_wrap(~plate, ncol = 1)
  p
}
