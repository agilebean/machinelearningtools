
group_high_low <- function(data, indiff_labels) {

  data %>%
    # add suffix ".score" to indiff variables
    rename_with(.cols = indiff_labels,
                .fn = ~ paste0(.x, ".score")
    ) %>%
    # classify each indiff into high/low > mean(indiff)
    mutate(across(
      ends_with(".score"),
      .fns = list(group = ~ ifelse(.x > median(.x), "high", "low")),
      .names = "{.col}.{.fn}"
    ),
    # keep all original variables Except the ones mutated
    .keep = "unused",
    # insert the new variables after last column
    .after = last_col()
    ) %>%
    # name the new vars as the original indiff vars
    rename_with(
      .cols = ends_with(".score.group"),
      ~ gsub(".score.group", "", .x)
    )
}

################################################################################
# plot.2AFC
# Input:
#   data_2afc_indiff:     the raw survey data including indiff groups
#   comparisons_function: creates the long table of pairwise comparisons
################################################################################
plot.2AFC <- function(
  data_2afc_indiff,
  comparisons_function,
  indiff_group = NULL,
  param_var = "parameter", param_select = NULL,
  grouping = NULL, max_wins = 3,
  x_group = NULL,
  x_variable,
  color_label = NULL,
  palette = "Set1",
  lined = TRUE,
  save_label = "", dpi = 450, width = 7, height = 3.5) {

  data.comparisons <- data_2afc_indiff %>%
    comparisons_function(indiff_group = indiff_group) %>% print

  data.2afc <- summarise_wins(
    data.comparisons, max_wins = max_wins,
    grouping = grouping) %>% print

  # create axis tick labels
  x.labels <- data.2afc %>%
    ungroup() %>%
    select(x_variable) %>%
    distinct() %>%
    unlist() %>%
    as.vector()

  nrow <- 1
  if (!is.null(indiff_group) & length(grouping) > 3) {
    nrow <- nrow + 2
  }

  # display different colors if more than 2 conditions
  no.conditions <- data.2afc[[x_group]] %>% as.factor %>% nlevels

  # show only one parameter
  if (!is.null(param_select)) {
    # tricky: filter does not work if argument is called parameter
    data.2afc %<>% filter(!!sym(param_var) == param_select)
  }

  # for no grouping, use the default x variable
  if (is.null(x_group)) {
    x_group <- x_variable
  }

  # for geom_line, convert x_variable to numeric
  if (lined == TRUE) {
    if (!is.numeric(data.2afc[[x_variable]])) {
      # conversion from factor only works if x_variable is char
      data.2afc %<>% mutate(
        across(x_variable,
               ~as.numeric(as.character(.x)))
      )
    }

  } else {
    data.2afc %<>% mutate(across(x_variable, as.factor))
  }

  # create base plot
  plot.base <- data.2afc %>%
    ggplot(data = ., aes(x = !!sym(x_variable), y = mean)) +
    theme_minimal(base_size = 11) +
    {
      # show all parameters
      if (is.null(param_select)) {

        # scales = "free_x" repeats x-axis tick labels for each facet
        facet_wrap(as.formula(paste("~ ", param_var)),
                   scales = "free_x",
                   nrow = nrow
        )
      }
    } +
    {
      if (no.conditions > 2) {
        scale_color_manual(
          values = colorRampPalette(RColorBrewer::brewer.pal(8, palette))(no.conditions)
        )
      } else {
        scale_color_brewer(palette = palette)
      }
    } +
    labs(
      x = x_variable,
      y = "probability chosen",
      color = color_label
    ) +
    theme(
      # legend.position = "top"
      legend.position = "bottom"
    )

  if (lined == TRUE) {

    x_group_sym <- rlang::sym(x_group)
    color = "darkblue"

    # tricky: The longer form && evaluates left to right until result
    if (!is.null(indiff_group) && (x_variable == x_group)) {

      plot.result <- plot.base +
        geom_line(size = 0.05, color = color) +
        geom_point() +
        geom_errorbar(
          aes(
            ymin = mean - se, ymax = mean + se,
            width = 2
          ),
          color = color,
          size = 0.15 # line thickness
        ) +
        # replace facewrap lavels by variable names
        scale_x_continuous(
          breaks = x.labels,
          labels = x.labels
        ) +
        # let y-axis start from 0
        scale_y_continuous(
          limits = c(0, NA),
          expand = expansion(mult = c(0, 0.1))
        )

    } else { # no indiff group

      plot.result <- plot.base +
        geom_point(aes(color = !!sym(x_group))) +
        geom_line(
          aes(color = !!sym(x_group)),
          size = 0.05
        ) +
        geom_errorbar(
          aes(
            ymin = mean - se, ymax = mean + se,
            color = !!sym(x_group),
            width = 2,
          ),
          # line thickness
          size = 0.15
        ) +
        # replace facewrap lavels by variable names
        scale_x_continuous(
          breaks = x.labels,
          labels = x.labels
        ) +
        # let y-axis start from 0
        scale_y_continuous(
          limits = c(0, NA),
          expand = expansion(mult = c(0, 0.1))
        )
    }

  } else {

    plot.result <- plot.base +
      geom_point(aes(color = !!sym(x_group))) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se,
            width = 0.2,
            color = !!sym(x_group)),
        size = 0.25
      )
  }

  if (save_label != "") {

    filelabel <- paste0("figures/", save_label, ".png") %T>% print

    ggsave(file = filelabel,
           dpi = dpi, width = width, height = height)
  }

  return(list(
    table = data.2afc,
    plot = plot.result
  ))
}


