
# plot 2AFC analysis
plot.2AFC <- function(
  data_2afc,
  param_label,
  param_var = "parameter",
  condition = NULL,
  indiff_group = NULL,
  x_group,
  x_variable,
  color_label = NULL,
  palette = "Set1",
  lined = TRUE, nrow = 1,
  save_label = "", dpi = 450, width = 7, height = 3.5) {

  print(x_group)
  # create axis tick labels
  x.labels <- data_2afc %>%
    ungroup() %>%
    select(x_variable) %>%
    distinct() %>%
    unlist() %>%
    as.vector()

  nrow <- 1
  if (!is.null(indiff_group) & !is.null(condition)) {
    nrow <- nrow + 2
  }

  # display different colors if more than 2 conditions
  no.conditions <- data_2afc[[x_group]] %>% nlevels

  # show only one parameter
  if (!missing(param_label)) {
    # tricky: filter does not work if argument is called parameter
    data_2afc %<>% filter(!!sym(param_var) == param_label)
  }

  # for no grouping, use the default x variable
  if (missing(x_group)) {
    x_group <- x_variable
  }

  # for geom_line, convert x_variable to numeric
  if (lined == TRUE) {
    if (!is.numeric(data_2afc[[x_variable]])) {
      # conversion from factor only works if x_variable is char
      data_2afc %<>% mutate(
        across(x_variable,
               ~as.numeric(as.character(.x)))
      )
    }

  } else {
    data_2afc %<>% mutate(across(x_variable, as.factor))
  }

  formula_string <- paste("~ ", param_var)
  if (!is.null(condition) & !is.null(indiff_group)) {
    formula_string <- paste(formula_string, "~", condition)
  }
  formula1 <- as.formula(formula_string)

  print(data_2afc)

  # create base plot
  plot.base <- data_2afc %>%
    ggplot(data = ., aes(x = !!sym(x_variable), y = mean)) +
    theme_minimal(base_size = 11) +
    {
      # show all parameters
      if (missing(param_label)) {

        # scales = "free_x" repeats x-axis tick labels for each facet
        facet_wrap(formula1,
                   scales = "free_x",
                   nrow = nrow
        )
      }
    } +
    {
      if (no.conditions > 2) {
        scale_color_manual(
          values = colorRampPalette(brewer.pal(8, palette))(no.conditions)
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

    x_group_sym <- sym(x_group)
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
        geom_line(
          aes(color = !!x_group_sym),
          size = 0.05
        ) +
        geom_point(aes(color = !!x_group_sym)) +
        geom_errorbar(
          aes(
            ymin = mean - se, ymax = mean + se,
            color = !!x_group_sym,
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

  return(plot.result)
}



plot_grouped_2AFC_data <- function(
  data_preprocessed,
  param_var = "parameter",
  indiff_group = NULL,
  condition = NULL,
  x_variable = "angle",
  save_label = "",
  ...) {

  # analysis per condition & indiff_group
  if (!is.null(condition) & !is.null(indiff_group)) {
    grouping <- c(param_var, condition, indiff_group, x_variable)
    x_group <- indiff_group

  } else if (!is.null(condition)) { # analysis per condition
    grouping <- c(param_var, condition, x_variable)
    x_group <- condition

  } else if (!is.null(indiff_group)) { # analysis per indiff_group
    grouping <- c(param_var, indiff_group, x_variable)
    x_group <- indiff_group

  } else { # analysis across all x_variable without group/condition
    grouping <- c(param_var, x_variable)
    x_group <- x_variable
  }

  data.comparisons <- data_preprocessed %>%
    create_comparisons_data(indiff_group)

  data.2afc <- data.comparisons %>%
    summarise_wins(max_wins = 3, grouping)

  plot.indiff <- plot.2AFC(
    data_2afc = data.2afc,
    param_var = param_var,
    condition = condition,
    indiff_group = indiff_group,
    x_group = x_group,
    x_variable = x_variable,
    nrow = nrow,
    save_label = save_label,
    ...
  )

  return(list(
    comp  = data.comparisons,
    table = data.2afc,
    plot  = plot.indiff))
}


