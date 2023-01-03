
group_high_low <- function(
  data, indiff_labels,
  split_fct = NULL, quantiles = c(0.33, 0.66),
  error_fct = NULL) {

  if (is.null(split_fct)) {
    cutoff.high <- function(x) {
      quantile(x, probs = quantiles[2], na.rm = TRUE)
    }
    cutoff.low <- function(x) {
      quantile(x, probs = quantiles[1],  na.rm = TRUE)
    }
  } else { # split_fct == mean or median
    if (is.null(error_fct)) {
      cutoff.high <- function(x) { (exec(split_fct, x)) }
      cutoff.low <- cutoff.high
    } else {
      cutoff.high <- function(x) {
        (exec(split_fct, x)) +  (exec(error_fct, x))
      }
      cutoff.low <- function(x) {
        (exec(split_fct, x)) - (exec(error_fct, x))
      }
    }
  }

  data %>%
    # add suffix ".score" to indiff variables
    rename_with(.cols = indiff_labels,
                .fn = ~ paste0(.x, ".score!")
    ) %>%
    # classify each indiff into high/low > mean(indiff)
    dplyr::mutate(across(
      ends_with(".score!"),
      .fns = list(group = ~ case_when(
        .x >  cutoff.high(.x) ~ "high",
        .x <  cutoff.low(.x) ~ "low",
        TRUE ~ "NA"
      ) %>%
        factor(levels = c("low", "high"))),
      .names = "{.col}.{.fn}"
    ),
    # keep all original variables Except the ones mutated
    .keep = "unused",
    # insert the new variables after last column
    .after = last_col()
    ) %>%
    # name the new vars as the original indiff vars
    rename_with(
      .cols = ends_with(".score!.group"),
      ~ gsub(".score!.group", "", .x)
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
  comparisons_function = NULL,
  indiff_group = NULL,
  stimulus_group = NULL,
  param_var = "parameter", param_select = NULL, facet_wrap = TRUE,
  grouping = NULL, max_wins = 3,
  x_variable,
  color_label = NULL,
  palette = "Set1",
  color_scheme_fct = NULL,
  lined = TRUE,
  save_label = "",
  digits = 3, format = "html",
  dpi = 450, width = 7, height = 3.5) {

  # give direct input of summarised 2AFC data
  if (is.null(comparisons_function)) {

    data.2afc <- data_2afc_indiff

  } else {

    data.comparisons <- data_2afc_indiff %>%
      # indiff_group used in group_by, so can be NULL
      comparisons_function(indiff_group = indiff_group) %>% print

    data.2afc <- summarise_wins(
      data.comparisons, max_wins = max_wins,
      grouping = grouping) %>% print
  }

  # determine group for aes(color)
  color.group <- if (!is.null(indiff_group)) {
    indiff_group
  } else if (!is.null(stimulus_group)) {
    stimulus_group
  } else {
    ""
  }

  # display different colors for many conditions
  if (color.group != "") {
    no.colors <- data.2afc[[color.group]] %>% as.factor %>% nlevels
  } else {
    no.colors <- 1
  }

  # show only one parameter
  if (!is.null(param_select)) {
    # tricky: filter does not work if argument is called parameter
    data.2afc %<>% filter(!!sym(param_var) == param_select)
  }

  # for geom_line, convert x_variable to numeric
  if (lined == TRUE) {
    if (!is.numeric(data.2afc[[x_variable]])) {
      # conversion from factor only works if x_variable is char
      data.2afc %<>% dplyr::mutate(
        across(x_variable,
               ~as.numeric(as.character(.x)))
      )
    }

  } else {
    data.2afc %<>% dplyr::mutate(across(x_variable, as.factor))
  }

  # get max probability
  max.prob <- data.2afc$mean %>% max

  # create base plot
  plot.base <- data.2afc %>%
    ggplot(data = ., aes(x = !!sym(x_variable), y = mean)) +
    theme_minimal(base_size = 11) +
    {
      # show all parameters
      if (is.null(param_select) & facet_wrap) {

        # scales = "free_x" repeats x-axis tick labels for each facet
        facet_wrap(as.formula(paste("~ ", param_var)),
                   scales = "free" # repeat y-axis tick labels
        )
      }
    } +
    {
      if (no.colors > 8) {
        scale_color_manual(
          values = colorRampPalette(RColorBrewer::brewer.pal(8, palette))(no.colors)
        )
      } else if (!is.null(color_scheme_fct)) {
        color_scheme_fct()
      } else {
        scale_color_brewer(palette = palette)
      }
    } +
    # repeat y-axis tick labels
    coord_cartesian(ylim = c(0, max.prob)) +
    labs(
      x = x_variable,
      y = "probability chosen",
      color = color_label
    ) +
    theme(legend.position = "bottom")

  # create axis tick labels
  x.labels <- data.2afc %>%
    ungroup() %>%
    select(x_variable) %>%
    distinct() %>%
    unlist() %>%
    as.vector()

  # calculate error bar width ~ 1/11 of one interval
  width.errorbar <- max(x.labels)/11

  # create main plot
  if (lined == TRUE) {

    color = "darkblue"

    plot.result <- plot.base +
      geom_point(aes(color = !!sym(color.group))) +
      geom_line(
        aes(color = !!sym(color.group)),
        size = 0.05
      ) +
      geom_errorbar(
        aes(
          ymin = mean - se, ymax = mean + se,
          color = !!sym(color.group),
          width = width.errorbar,
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

  } else { # no lines

    plot.result <- plot.base +
      geom_point(aes(color = !!sym(color.group))) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se,
            width = 0.2,
            color = !!sym(color.group)),
        size = 0.25
      )
  }

  if (save_label != "") {

    label.plot <- paste0("figures/", save_label, ".png") %T>% print
    label.table <- paste0("tables/", save_label, ".", format) %T>% print

    ggsave(file = label.plot,
           bg = "white",
           dpi = dpi, width = width, height = height)

    data.2afc %>%
      convert_kable(., digits = digits) %>%
      cat(., file = label.table)

  }

  return(list(
    table = data.2afc,
    plot = plot.result
  ))
}


