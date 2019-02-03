################################################################################
#
# Package: machinelearningtools
# Purpose: Provide convenience functions for machine learning with caret
#
################################################################################


################################################################################
# set model input > formula
################################################################################
set_formula <- function(target_label, features) {

  features %>%
    paste(collapse = " + ") %>%
    paste(target_label, "~", .) %>%
    as.formula(env = .GlobalEnv)

}

################################################################################
# turn on cluster for parallel processing
################################################################################
clusterOn <- function() {

  require(doParallel) # loads parallel library for makeCluster
  cluster.new <- makeCluster(spec = { detectCores() - 1 }, type = "FORK")
  registerDoParallel(cluster.new)

  return(cluster.new)

}

################################################################################
# get_model_metrics:
#   calculate training set performance:
#   mean & sd for all model objects in model_list
#
# set color by
##
##  palette:
##    models.list %>% get_model_metrics(palette = "Dark2")
##
##  color codes:
##    models.list %>% get_model_metrics(
##      colors = c("#4DAF4A", "#E41A1C", "#FF7F00", "#377EB8"))
##
##  colors: "#4DAF4A" green "#377EB8" blue "#E41A1C" red "#FF7F00" orange
##
################################################################################
get_model_metrics <- function(model_list,
                              target_label = target.label,
                              testing_set = testing.set,
                              palette = "Set1", direction = 1,
                              colors = NULL,
                              boxplot_color = "grey95") {
  require(dplyr)
  require(purrr)
  require(ggplot2)
  require(RColorBrewer)

  transpose_table <- function(metric_table, metric, desc = FALSE) {

    suffix <- paste0("~", metric)
    mean <- paste0(metric,".training")

    metric_table %>%
      dplyr::select(ends_with(suffix)) %>%
      rename_all(funs(gsub(suffix, "", .))) %>%
      t %>% as.data.frame %>%
      rename(mean = V1, sd = V2) %>%
      round(digits = 3) %>%
      rownames_to_column(var = "model") %>%
      arrange( {if (desc) desc(mean) else mean } )

  }

  ### get metrics from original resamples' folds
  resamples.values <- model_list %>% resamples %>% .$values %>%
    select_if(is.numeric) %>%
    # retrieve RMSE, Rsquared but not MAE
    ## tricky: select without dplyr:: prefix does NOT work
    dplyr::select(ends_with("RMSE"), ends_with("Rsquared"))

  ### calculate mean and sd for each metric
  metric_table <- resamples.values %>%
    map_df(function(variable) {
      ## tricky: dplyr::mutate doesn't work here
      c(mean = mean(variable), sd = sd(variable))
    }) %>% print

  RMSE.training <- metric_table %>% transpose_table("RMSE")

  Rsquared.training <- metric_table %>% transpose_table("Rsquared", desc = TRUE)

  ### visualize the resampling distribution from cross-validation
  resamples.boxplots <-
    resamples.values %>%
    dplyr::select(ends_with("~RMSE")) %>%
    set_names(~gsub("~RMSE","",.)) %>%
    drop_na() %>%
    gather(key = model, value = RMSE) %>%
    ggplot(aes(x = reorder(model, RMSE, median), y = RMSE, color = model)) +
    theme_minimal() +
    geom_boxplot(width = 0.7, fill=boxplot_color) +
    geom_jitter() +
    coord_flip() +
    labs(x = "model") +
    theme(legend.position = "none", # removes all legends
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14)) +
    scale_color_brewer(palette = palette, direction = direction)

  if (!is.null(colors)) {
    resamples.boxplots <-
      resamples.boxplots +
      scale_color_manual(values = colors)
  }

  # RMSE for all models on testing set
  RMSE.testing <- get_rmse_testing(target_label, model_list, testing_set)

  benchmark.all <- merge(RMSE.training, RMSE.testing, by = "model") %>%
    mutate(delta = mean - RMSE.testing) %>%
    arrange(RMSE.testing)

  return(list(RMSE.training = RMSE.training,
              Rsquared.training = Rsquared.training,
              RMSE.testing = RMSE.testing,
              RMSE.all = benchmark.all,
              RMSE.boxplots = resamples.boxplots
  ))
}



################################################################################
# Get Testing Set Performance
# calculate RMSE for all model objects in model_list
################################################################################
get_rmse_testing <- function(target_label, models_list, testing_set) {

  models_list %>%
    # caret::predict() can take a list of train objects as input
    predict(testing.set) %>%
    map_df(~sqrt(mean( (testing.set[[target_label]]-.)^2) ) ) %>%
    # simpler than: mutate_if(is.numeric, funs(round(., digits = 3)))
    round(digits = 3) %>%
    t %>% as.data.frame %>%
    select(RMSE.testing = V1) %>%
    rownames_to_column(var = "model") %>%
    arrange(RMSE.testing)
}

################################################################################
# List variable importance
# input caret::train object
################################################################################
list_variable_importance <- function(train_model) {

  train_model$importance %>%
    as.data.frame %>%
    tibble::rownames_to_column() %>%
    mutate(Importance = round(IncNodePurity * 100/max(IncNodePurity), digits =2)) %>%
    arrange(-IncNodePurity)

}

################################################################################
# Visualize variable imporance for randomForests objects
# input randomForests object
################################################################################
visualize_variable_importance_rf <- function(rf_object) {
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

