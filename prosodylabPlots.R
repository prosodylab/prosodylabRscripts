

plotLogodds <- function(data,
                         xVar,
                         yVar,
                         facetVariables = character(0),
                         xLab = "",
                         yLab = "",
                         use_repel = FALSE,
                         point_size = 3,
                         errorbar_width = 0.15,
                         label_hjust = -0.25,
                         label_vjust = 0,
                         base_size = 14,
                         plotTitle = "") {

  
  # exclude NAs of indepdent variables:
  data <- data %>%
    dplyr::filter(
      !is.na({{ xVar }}),
      dplyr::if_all(dplyr::all_of(facetVariables), ~ !is.na(.x))
    )
    
  # Require core packages and load if not already loaded
  if (!require(dplyr, quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!require(ggplot2, quietly = TRUE)) stop("Package 'ggplot2' is required.")
  
  # Load ggrepel only if requested
  repel_available <- FALSE
  if (isTRUE(use_repel)) {
    repel_available <- require(ggrepel, quietly = TRUE)
    if (!repel_available) {
      message("Requested use_repel=TRUE but 'ggrepel' is not installed; falling back to geom_text().")
    }
  }
  
  # Build facet layer (0, 1, or 2 vars)
  facet_layer <- NULL
  if (length(facetVariables) == 1) {
    facet_layer <- ggplot2::facet_grid(as.formula(paste0("~", facetVariables[1])))
  } else if (length(facetVariables) >= 2) {
    facet_layer <- ggplot2::facet_grid(as.formula(paste0(facetVariables[1], " ~ ", facetVariables[2])))
  }
  
  # Labels
  yLab_base <- if (nzchar(yLab)) yLab else deparse(substitute(yVar))
  y_main    <- paste0("Log odds of ", yLab_base)
  y_sec     <- paste0("% ", yLab_base)
  
  # Summaries per x × (facets)
  sum_df <- data %>%
    dplyr::filter(!is.na({{ yVar }})) %>%
    dplyr::group_by({{ xVar }}, dplyr::across(dplyr::all_of(facetVariables))) %>%
    dplyr::summarise(
      n   = dplyr::n(),
      k   = sum({{ yVar }} == 1, na.rm = TRUE),
      p   = k / n,
      perc = 100 * p,
      mean_logodds = log((k + 0.5) / (n - k + 0.5)),                    # empirical logit
      se_logodds   = sqrt(1/(k + 0.5) + 1/(n - k + 0.5)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ymin = mean_logodds - 1.96 * se_logodds,
      ymax = mean_logodds + 1.96 * se_logodds
    )
  
  # Base plot (points & error bars on log-odds scale)
  p <- ggplot2::ggplot(sum_df, ggplot2::aes(x = {{ xVar }}, y = mean_logodds)) +
    ggplot2::geom_point(size = point_size, shape = 1) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax), width = errorbar_width, size = 0.6)
  
  # Label layer at the mean log-odds (shows n and %)
  if (isTRUE(use_repel) && repel_available) {
    p <- p + ggrepel::geom_text_repel(
      ggplot2::aes(label = paste0(n, "/", round(perc, 0), "%")),
      size = 3
    )
  } else {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = paste0(round(perc, 0), "% of ",n)),
      size = 3,
      hjust = label_hjust,
      vjust = label_vjust
    )
  }
  
  # Add facets if specified
  if (!is.null(facet_layer)) p <- p + facet_layer
  
  # Scales, theme, labels
  p +
    ggplot2::scale_y_continuous(
      name = y_main,
      sec.axis = ggplot2::sec_axis(~ plogis(.) * 100, name = y_sec)
    ) +
    ggplot2::xlab(xLab) +
    coord_cartesian(clip = "off") +
    scale_x_discrete(expand = expansion(mult = c(0.2, 0.2))) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 20, r = 60, b = 20, l = 20),
      plot.title = element_text(hjust = 0.5,margin = margin(b = 20),face = "bold")
    ) + # to give more space for secondary y-axis label and title
    theme(
      axis.title.y.right = element_text(margin = margin(l = 10)),
      axis.text.y.right  = element_text(margin = margin(l = 10))
    ) + 
    ggtitle(plotTitle)
}


plotAcoustics <- function(data,
                          xVar,
                          yVar = c("rIntensity","rDuration","rPitch"),
                          facetVariables = character(0),
                          xLab = "",
                          yLabs = c("intensity","log duration","pitch (s.t.)"),
                          use_repel = FALSE,
                          point_size = 3,
                          errorbar_width = 0.15,
                          xnudge = 0.25,
                          label_vjust = 0.5,
                          base_size = 14,
                          plotTitle = "",
                          orientation = c("horizontal","vertical")) {
  
  orientation <- match.arg(orientation)
  
  # ---- requirements ----
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE))  stop("Package 'dplyr' is required.")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Package 'patchwork' is required.")
  
  repel_available <- FALSE
  if (isTRUE(use_repel)) {
    repel_available <- requireNamespace("ggrepel", quietly = TRUE)
    if (!repel_available) message("use_repel=TRUE but 'ggrepel' not installed; falling back to geom_text().")
  }
  
  # ---- helpers ----
  make_facet <- function(vars) {
    if (length(vars) == 0) return(NULL)
    if (length(vars) == 1) return(ggplot2::facet_grid(stats::as.formula(paste0("~", vars[1]))))
    ggplot2::facet_grid(stats::as.formula(paste0(vars[1], " ~ ", vars[2])))
  }
  facet_layer <- make_facet(facetVariables)
  
  delta_label <- function(lbl_string) bquote(Delta ~ .(lbl_string))
  
  x_quo <- rlang::enquo(xVar)
  
  base_data <- data |>
    dplyr::filter(
      !is.na(!!x_quo),
      dplyr::if_all(dplyr::all_of(facetVariables), ~ !is.na(.x))
    )
  
  # one panel; show_x controls whether x-axis title/ticks/labels are displayed
  make_panel <- function(y_name, ylab_text, show_x = TRUE) {
    df <- base_data |>
      dplyr::filter(!is.na(.data[[y_name]]))
    
    lab_df <- df |>
      dplyr::group_by(!!x_quo, dplyr::across(dplyr::all_of(facetVariables))) |>
      dplyr::summarise(
        m = round(mean(.data[[y_name]], na.rm = TRUE), 1),
        n = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(label = paste0(m, "\n", "n=", n))
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = !!x_quo, y = .data[[y_name]])) +
      ggplot2::stat_summary(fun = mean, geom = "point", size = point_size, shape = 1) +
      ggplot2::stat_summary(fun.data = "mean_cl_boot", geom = "errorbar",
                            linewidth = 0.6, width = errorbar_width)
    
    if (isTRUE(use_repel) && repel_available) {
      p <- p +
        ggrepel::geom_text_repel(
          data = lab_df,
          ggplot2::aes(x = !!x_quo, y = m, label = label),
          inherit.aes = FALSE,
          hjust = 0.5,
          vjust = label_vjust,
          size = 3,
          direction = "y",
          max.overlaps = Inf,
          nudge_x = xnudge
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = lab_df,
          ggplot2::aes(x = !!x_quo, y = m, label = label),
          inherit.aes = FALSE,
          hjust = 0.5,
          vjust = label_vjust,
          size = 3,
          position = ggplot2::position_nudge(x = xnudge)
        )
    }
    
    if (!is.null(facet_layer)) p <- p + facet_layer
    
    p <- p +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::ylab(delta_label(ylab_text)) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 10, r = 10, b = 5, l = 10)
      )
    
    if (isTRUE(show_x)) {
      p <- p +
        ggplot2::xlab(xLab) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      p <- p +
        ggplot2::xlab(NULL) +
        ggplot2::theme(
          axis.text.x  = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()
        )
    }
    
    p
  }
  
  if (length(yVar) != 3L || length(yLabs) != 3L) {
    stop("yVar and yLabs must both be length 3 (e.g., c('rIntensity','rDuration','rPitch') and matching labels).")
  }
  
  # Build panels with x-axis visibility depending on orientation
  if (orientation == "vertical") {
    p1 <- make_panel(yVar[1], yLabs[1], show_x = FALSE)
    p2 <- make_panel(yVar[2], yLabs[2], show_x = FALSE)
    p3 <- make_panel(yVar[3], yLabs[3], show_x = TRUE)
    combined <- (p1 / p2 / p3)
  } else { # horizontal
    p1 <- make_panel(yVar[1], yLabs[1], show_x = TRUE)
    p2 <- make_panel(yVar[2], yLabs[2], show_x = TRUE)
    p3 <- make_panel(yVar[3], yLabs[3], show_x = TRUE)
    combined <- (p1 | p2 | p3)
  }
  
  combined <- combined +
    patchwork::plot_annotation(title = plotTitle) &
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", margin = ggplot2::margin(b = 12))
    )
  
  return(combined)
}