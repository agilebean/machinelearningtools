################################################################################
#
# XAI Functions
#
################################################################################

######################################################################
# Function get_percentile_from_model()
# INPUT model (caret::train)
# OUTPUT percentile (numeric)
######################################################################
get_percentile_from_model <- function(model, percentile) {

  model$trainingData$.outcome %>%
    quantile(percentile)
}

######################################################################
# Function get_xai_explanations()
# IN:   models_list (list) containing caret models
# OUT:  xai output by DALEX or LIME
######################################################################
get_xai_explanations <- function(
  models_list,
  seed = 171,
  cutoff_greater = 0,
  n_features_lime = 5,
  local_obs = NULL,
  local_min_cutoff = 0.95,
  local_no = 6,
  random_case = NULL,
  save_path = NULL,
  suffix = NULL,
  width = 6, height = 6,
  get_DALEX_explainer = TRUE,
  get_DALEX_residual_plot = TRUE,
  no_permutations = 50,
  get_DALEX_feature_importance = TRUE,
  get_DALEX_feature_importance_plot = TRUE,
  get_DALEX_pdp_plot = TRUE,
  get_DALEX_attribution_plot = TRUE,
  get_DALEX_attribution_text = TRUE,
  get_DALEX_attribution_uncertainty_plot = TRUE,
  get_DALEX_shapley_plot = TRUE,
  get_LIME_explainer = FALSE,
  get_LIME_explanations = FALSE,
  get_LIME_explanations_plot = FALSE,
  get_LIME_features_plot = FALSE
) {

  require(ggplot2) # ggsave
  require(dplyr)
  require(furrr)

  if (get_DALEX_explainer) {
    require(DALEX)
    require(iBreakDown)
    require(ingredients)
  }
  if (get_LIME_explainer) require(lime)

  options(parallelly.fork.enable = TRUE)
  plan(multicore, workers = 8)

  xai.list <- models_list %>%

    future_map(function(model_object) {

      print(paste("*********", model_object$method))
      training.set <- model_object$trainingData %>%
        select(.outcome, everything())

      target <- training.set$.outcome
      print(paste("***target"))

      features <- training.set %>% select(-.outcome)

      # local observations for prediction
      local.obs <- if (!is.null(local_obs)) {
        local_obs
      } else {
        training.set %>%
          filter(
            .outcome >=
              get_percentile_from_model(model_object, local_min_cutoff)) %>%
          sample_n(local_no)
      }

      random.case <- if (!is.null(random_case)) {
        random_case
      } else {
        local.obs %>% sample_n(1)
      }

      DALEX.explainer <- if (get_DALEX_explainer) {

        print("*** DALEX.explainer")

        DALEX::explain(
          model = model_object,
          data = features,
          y = training.set$.outcome >= cutoff_greater,
          label = paste(model_object$method, " model"),
          colorize = TRUE
        )
      } else {
        NULL
      }

      # for residual plots by plot(geom = "histogram")
      DALEX.performance <- DALEX.explainer %>%
        DALEX::model_performance()

      DALEX.residual.plot <- if (get_DALEX_residual_plot) {

          DALEX.performance %>% plot(geom = "histogram")

      } else {
        NULL
      }

      DALEX.feature.importance <- if (
        get_DALEX_feature_importance &
        !is.null(DALEX.explainer)) {

        print("*** DALEX.permutation.feature.importance")

        DALEX.explainer %>%
          model_parts(
            B = no_permutations,
            type = "ratio"
          )

      } else {
        NULL
      }

      DALEX.feature.importance.plot <- if (
        get_DALEX_feature_importance_plot &
        !is.null(DALEX.feature.importance)) {

        print("*** DALEX.feature.importance.plot")

        DALEX.feature.importance %>%
          plot(
            bar_width = 20 / log(ncol(features))
            , show_boxplots = FALSE
            , title = "Permutation Feature importance"
            , subtitle = ""
          ) +
          # reduce space to axis
          scale_y_continuous(expand = expansion()) %T>%
        {
          if (!is.null(save_path)) {
            ggsave(
              width = width, height = height,
              filename = paste(
                c(save_path, "DALEX.feature.importance.plot", model_object$method,
                suffix, "png"),
                collapse = ".")
            )
          }
        }

      } else {
        NULL
      }

      DALEX.pdp.plot <- if (get_DALEX_pdp_plot & !is.null(DALEX.explainer)) {

        print("*** DALEX.pdp.plot")

        DALEX.pdp <- DALEX.explainer %>% ingredients::partial_dependency()

        DALEX.pdp %>% plot %T>%
        {
          if (!is.null(save_path)) {
            ggsave(
              width = width, height = height,
              filename = paste(
                c(save_path, "plot.pdp.DALEX", model_object$method,
                  suffix, "png"),
                collapse = ".")
            )
          }
        }
      } else {
        NULL
      }

      DALEX.attribution <- DALEX.explainer %>%
        iBreakDown::local_attributions(
          local.obs,
          keep_distributions = TRUE
        )
      print("*** DALEX.attribution")

      DALEX.attribution.text <- if(get_DALEX_attribution_text) {

        print("*** DALEX.attribution.text")

        DALEX.attribution %>%
          iBreakDown::describe()
      } else {
        NULL
      }

      DALEX.attribution.plot <- if (get_DALEX_attribution_plot &
                                    !is.null(DALEX.explainer)) {

        print("*** DALEX.attribution.plot")

        DALEX.explainer %>%
          iBreakDown::local_attributions(
            local.obs,
            keep_distributions = TRUE
            ) %>%
          plot(
            shift_contributions = 0.03
          ) %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "DALEX.attribution.plot", model_object$method,
                    suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }

      DALEX.attribution.uncertainty.plot <-
        if (get_DALEX_attribution_uncertainty_plot &
            !is.null(DALEX.explainer)) {

          print("*** DALEX.attribution.uncertainty.plot")
          DALEX.explainer %>%
            iBreakDown::break_down_uncertainty(local.obs) %>%
            plot %T>%
            {
              if (!is.null(save_path)) {
                ggsave(
                  width = width, height = height,
                  filename = paste(
                    c(save_path, "DALEX.attribution.uncertainty.plot",
                      model_object$method, suffix, "png"),
                    collapse = ".")
                )
              }
            }
        } else {
          NULL
        }


      DALEX.distribution.plot <- DALEX.attribution %>%
        plot(plot_distributions = TRUE)

      print("*** DALEX.distribution.plot")


      DALEX.shapley.plot <- if (
        get_DALEX_shapley_plot &
        !is.null(DALEX.explainer) & !is.null(random_case)) {

        print("*** DALEX.shapley.plot")

        DALEX.explainer %>%
          iBreakDown::shap(random_case,
                           B = no_permutations) %>%
          plot()
      }


      LIME.explainer <- if (get_LIME_explainer) {

        print("*** LIME.explainer")
        lime::lime(
          # tricky: features not training.set
          x = features,
          model = model_object
        )
      } else {
        NULL
      }

      LIME.explanations <- if (
        get_LIME_explanations & !is.null(LIME.explainer)) {

        print("***LIME.explanations")
        lime::explain(
          # tricky: features not training.set
          x = local.obs %>% select(-.outcome),
          explainer = LIME.explainer,
          n_features = n_features_lime
        ) %T>% print
      } else {
        NULL
      }

      LIME.explanations.plot <- if (
        get_LIME_explanations_plot & !is.null(LIME.explanations)) {

        print("***LIME.explanations.plot")
        lime::plot_explanations(
          LIME.explanations
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "LIME.explanations.plot",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }

      } else {
        NULL
      }

      LIME.features.plot <- if (
        get_LIME_features_plot & !is.null(LIME.explainer)) {

        print("***LIME.features.plot")
        lime::plot_features(
          LIME.explanations,
          ncol = 2
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "LIME.features.plot",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }

      return(
        list(
          DALEX.explainer = DALEX.explainer
          , DALEX.performance = DALEX.performance
          , DALEX.feature.importance = DALEX.feature.importance
          , DALEX.feature.importance.plot = DALEX.feature.importance.plot
          , DALEX.residual.plot = DALEX.residual.plot
          , DALEX.pdp.plot = DALEX.pdp.plot
          , DALEX.attribution.text = DALEX.attribution.text
          , DALEX.attribution.plot = DALEX.attribution.plot
          , DALEX.attribution.uncertainty.plot = DALEX.attribution.uncertainty.plot
          , DALEX.distribution.plot = DALEX.distribution.plot
          , DALEX.shapley.plot = DALEX.shapley.plot
          , LIME.explainer = LIME.explainer
          , LIME.explanations = LIME.explanations
          , LIME.explanations.plot = LIME.explanations.plot
          , LIME.features.plot = LIME.features.plot
        )
      )
    },
    .options = furrr_options(
      seed = seed
      , packages = c("DALEX", "iBreakDown", "ingredients", "lime")
    ))
}

################################################################################
# get permutation feature importance (pimp) for models.list
################################################################################
get_pimp_for_models_list <- function(
  models_list, no_permutations = 50, seed = 171) {

  require(dplyr)
  require(DALEX)

  fi.list.DALEX <- models_list %>%

    imap(function(model_object, model_name) {

      print(paste("*********", model_object$method))

      training.set <- model_object$trainingData %>%
        select(.outcome, everything())

      target <- training.set$.outcome
      print(paste("***target"))

      features <- training.set %>% select(-.outcome)

      DALEX.explainer <- DALEX::explain(
        model = model_object,
        data = features,
        y = target,
        label = model_name, # .y in imap() is list element name
        colorize = TRUE
      )
      print("*** DALEX.explainer")

      set.seed(seed)
      system.time(
        DALEX.permutation.fi <- model_parts(
          explainer = DALEX.explainer,
          loss_function = loss_root_mean_square,
          B = no_permutations,
          type = "ratio"
        )
      ) # 78-84s for rf

      print("*** DALEX.permutation.fi")

      # bar_width = 5.9 items / 12.4 composites
      ( bar.width <- 20 / log(ncol(features)) )

      DALEX.permutation.fi.plot <- DALEX.permutation.fi %>%
        plot(
          bar_width = bar.width
          , show_boxplots = FALSE
          , title = "Permutation Feature importance"
          , subtitle = ""
        ) +
        scale_y_continuous(expand = expansion()) # reduce space to axis

      print("*** DALEX.permutation.fi.plot")

      return(
        list(
          DALEX.explainer = DALEX.explainer
          , DALEX.permutation.fi = DALEX.permutation.fi
          , DALEX.permutation.fi.plot = DALEX.permutation.fi.plot
        )
      )
    })
  # 44s B=10, 220s B=50, 408s B=100

  return(fi.list.DALEX)
}

#####################################################
# calculate permutation feature importance
#####################################################
get_pimp_range_List_models_list <- function(
  list_models_list, model_string = "", value_function = max) {

  list_models_list %>%
    {
      if (model_string != "") {

        get_list_elements_by_string(., model_string)

      } else {
        .
      }
    } %>%
    {
      # TRICKY: temporary assignment works only for . not names(.) bec it
      # is piped through, so call models.selected %>% names later
      {
        . -> models.selected
      } %>%
        imap_dfr(function(models_list, models_list_label) {
          models_list %>%
            imap(function(model, model_label) {
              model$DALEX.permutation.fi %>%
                # remove the baseline model (seems without any predictor)
                filter(variable != "_baseline_") %>%
                group_by(variable) %>%
                summarise(mean_dropout_loss = mean(dropout_loss)) %>%
                .$mean_dropout_loss %>%
                value_function
            })
        }) %>%
        as.data.frame %>%
        set_rownames(models.selected %>% names)
    }
}

################################################################################
# save feature importance plots
################################################################################
save_pimp_plots <- function(
  datasets_pimp_lists,
  model_string = "",
  scaling_factor = 1,
  width = "automatic",
  height = "automatic",
  axis_tick_labels = NULL) {

  pimp.max <- datasets_pimp_lists %>%
    get_pimp_range_List_models_list(model_string) %>%
    max %>%
    print

  pimp.min <- datasets_pimp_lists %>%
    get_pimp_range_List_models_list(model_string, min) %>%
    min %>%
    print

  no.features <- function(model) {
    model %>%
      pluck("DALEX.explainer") %>%
      pluck("data") %>%
      ncol %>% print
  }

  datasets_pimp_lists %>%
    get_list_elements_by_string(model_string) %>%
    imap(function(model, model_label) {
      model %>%
        imap(
          ~ .x %>%
            {
              .$DALEX.permutation.fi.plot +
                # same scale on flipped x-asis for same datasets_pimp_lists
                scale_y_continuous(limits = c(pimp.min, pimp.max)) +
                # next layer must be added within code block of plot object
                {
                  if (!is.null(axis_tick_labels)) {
                    scale_x_discrete(labels = axis_tick_labels)
                  }
                }
            } %>%
            ggsave(
              filename = paste0(c("figures/pimp", model_label, .y, "png"), collapse = "."),
              plot = .,
              scale = scaling_factor,
              width = ifelse(is.numeric(width), width, log(no.features(.x)) * 3),
              height = ifelse(is.numeric(height), height, log(no.features(.x)) * 2.2),
              dpi = 150
            )
        )
    })
}

################################################################################
# List variable importance scores
# input caret::train object
################################################################################
list_variable_importance <- function(model_object) {

  require(dplyr)
  require(caret)

  model_object$importance %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    mutate(Importance = round(IncNodePurity * 100/max(IncNodePurity), digits =2)) %>%
    arrange(-IncNodePurity)

}

################################################################################
# visualize feature importance by caret::varImp
# input caret::train object
################################################################################
visualize_varImp <- function (
  model_object,
  x_label = "",
  y_label = "feature importance",
  fill_color = "#114151") {

  require(dplyr)
  require(caret)

  importance_object <- model_object %>% varImp()

  if (class(importance_object) == "varImp.train") {
    importance_object %<>% .$importance
  }
  if (!hasName(importance_object, "rowname")) {
    importance_object %<>% rownames_to_column()
  }

  importance_object %>%
    setNames(c("variable", "Importance")) %>%
    ggplot(data = ., aes(x = reorder(variable, Importance), y = Importance)) +
    theme_minimal() +
    geom_bar(stat = "identity", fill = fill_color) +
    coord_flip() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 102)) +
    xlab(x_label) + ylab(y_label)
}


################################################################################
# Visualize variable imporance for randomForests objects
# input randomForest object
################################################################################
visualize_variable_importance_rf <- function(rf_object) {

  require(dplyr)

  rf_object$importance %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    mutate(Importance = round(IncNodePurity * 100/max(IncNodePurity), digits =2)) %>%
    arrange(-IncNodePurity) %>%
    ggplot(data = ., aes(x = reorder(rowname, Importance), y = Importance)) +
    theme_minimal() +
    geom_bar(stat="identity", fill = "#114151") +
    coord_flip() +
    theme(axis.title = element_text(size = 12)
          , axis.text = element_text(size = 12)
          # , panel.grid.major.y = element_blank() # remove horizontal grid lines
    ) +
    scale_y_continuous(expand = c(0,0), limits = c(0,102)) +
    xlab("item") + ylab("variable importance")
}


