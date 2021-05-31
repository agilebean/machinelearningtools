################################################################################
#
# XAI Functions
#
################################################################################


######################################################################
# Function get_xai_explanations()
# IN:   models_list (list) containing caret models
# OUT:  xai output by DALEX or LIME
######################################################################
get_xai_explanations <- function(
  models_list,
  cutoff_greater = 0,
  n_features_lime = 5,
  local_obs = NULL,
  local_no = 6,
  save_path = NULL,
  suffix = NULL,
  width = 6, height = 6,
  get_explainer_DALEX = TRUE,
  get_varImp_DALEX = FALSE,
  get_plot_varImp_DALEX = FALSE,
  get_pdp_plot_DALEX = FALSE,
  get_plot_attribution_DALEX = FALSE,
  get_plot_attribution_uncertainty_DALEX = FALSE,
  get_explainer_LIME = TRUE,
  get_explanation_LIME = FALSE,
  get_plot_features_LIME = FALSE,
  get_plot_explanations_LIME = FALSE
) {

  require(ggplot2) # ggsave

  if (get_explainer_DALEX) {
    require(DALEX)
    require(iBreakDown)
    require(ingredients)
  }
  if (get_explainer_LIME) require(lime)

  xai.list <- models_list %>%

    map(function(model_object) {

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
        training.set %>% sample_n(local_no)
      }

      explainer.DALEX <- if (get_explainer_DALEX) {

        print("*** explainer.DALEX")

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

      varImp.DALEX <- if (get_varImp_DALEX &
                          !is.null(explainer.DALEX)) {

        explainer.DALEX %>% variable_importance()

      } else {
        NULL
      }

      plot.varImp.DALEX <- if (get_plot_varImp_DALEX &
                               !is.null(varImp.DALEX)) {

        print("*** plot.varImp.DALEX")

        varImp.DALEX %>% plot %T>%
        {
          if (!is.null(save_path)) {
            ggsave(
              width = width, height = height,
              filename = paste(
                c(save_path, "plot.varImp.DALEX", model_object$method,
                suffix, "png"),
                collapse = ".")
            )
          }
        }
      } else {
        NULL
      }

      plot.pdp.DALEX <- if (get_pdp_plot_DALEX & !is.null(explainer.DALEX)) {

        print("*** plot.pdp.DALEX")

        pdp.DALEX <- explainer.DALEX %>% ingredients::partial_dependency()
        pdp.DALEX %>% plot %T>%
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

      plot.attribution.DALEX <- if (get_plot_attribution_DALEX &
                                    !is.null(explainer.DALEX)) {

        print("*** plot.attribution.DALEX")

        explainer.DALEX %>%
          iBreakDown::local_attributions(local.obs) %>%
          plot %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "plot.attribution.DALEX", model_object$method,
                    suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }

      plot.attribution.uncertainty.DALEX <- if (

        get_plot_attribution_uncertainty_DALEX &
        !is.null(explainer.DALEX)) {

        print("*** plot.attribution.uncertainty.DALEX")
        explainer.DALEX %>%
          iBreakDown::break_down_uncertainty(local.obs) %>%
          plot %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "plot.attribution.uncertainty.DALEX",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }

      explainer.LIME <- if (get_explainer_LIME) {

        print("*** explainer.LIME")
        lime::lime(
          # tricky: features not training.set
          x = features,
          model = model_object
        )
      } else {
        NULL
      }

      explanation.LIME <- if (
        get_explanation_LIME & !is.null(explainer.LIME)) {

        print("***explanation.LIME")
        lime::explain(
          # tricky: features not training.set
          x = local.obs %>% select(-.outcome),
          explainer = explainer.LIME,
          n_features = n_features_lime
        ) %T>% print
      } else {
        NULL
      }

      plot.features.LIME <- if (
        get_plot_features_LIME & !is.null(explainer.LIME)) {

        print("***plot.features.LIME")
        lime::plot_features(
          explanation.LIME,
          ncol = 2
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "plot.features.LIME",
                    model_object$method, suffix, "png"),
                  collapse = ".")
              )
            }
          }
      } else {
        NULL
      }

      plot.explanations.LIME <- if (
        get_plot_explanations_LIME & !is.null(explanation.LIME)) {

        print("***plot.explanations.LIME")
        lime::plot_explanations(
          explanation.LIME
        ) + ggtitle(model_object$method)  %T>%
          {
            if (!is.null(save_path)) {
              ggsave(
                width = width, height = height,
                filename = paste(
                  c(save_path, "plot.explanations.LIME",
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
          explainer.DALEX = explainer.DALEX
          , varImp.DALEX = varImp.DALEX
          , plot.varImp.DALEX = plot.varImp.DALEX
          , plot.pdp.DALEX = plot.pdp.DALEX
          , plot.attribution.DALEX = plot.attribution.DALEX
          , plot.attribution.uncertainty.DALEX = plot.attribution.uncertainty.DALEX
          , explainer.LIME = explainer.LIME
          , explanation.LIME = explanation.LIME
          , plot.features.LIME = plot.features.LIME
          , plot.explanations.LIME = plot.explanations.LIME
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
