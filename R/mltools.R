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
##  colors: "#4DAF4A" green "#377EB8" blue "#E41A1C" red "#FF7F00" orange
##  order: 2, 4, 3, 1
################################################################################
get_model_metrics <- function(model_list,
                              colors = c("#4DAF4A", "#E41A1C", "#FF7F00", "#377EB8"),
                              boxplot_color = "grey95") {
  require(dplyr)
  require(purrr)
  require(ggplot2)
  require(RColorBrewer)

  model_list <- models.list

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
    }) %>%
    t %>% as.data.frame %>%
    rename(mean = V1, sd = V2) %>%
    round(digits = 3)

  # # visualize model_metrics
  # resamples.barcharts <- metric_table %>%
  #   rownames_to_column() %>%
  #   mutate(model = rowname) %>%
  #   # remove "~RMSE" suffix from model names
  #   mutate_at(.vars = "model", .funs = function(x) gsub("~RMSE", "", x) ) %>%
  #   ggplot(data = .[1:4,], mapping = aes(x = reorder(model, mean), y = mean, color=model)) +
  #   geom_bar(stat = "identity") +
  #   geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width=.2,
  #                 position = position_dodge(.9)) +
  #   coord_flip() +
  #   theme(legend.position = "") +
  #   theme(axis.text.x = element_text(size = 12)) +
  #   theme(axis.text.y = element_text(size = 12 )) +
  #   xlab("Model") + ylab("RMSE")

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
    # scale_color_brewer(values = RColorBrewer::brewer.pal(4,"Set1"))
    scale_color_manual(values = colors)

  return(list(RMSE.training = metric_table,
              # RMSE.barcharts = resamples.barcharts,
              RMSE.boxplots = resamples.boxplots
  ))
}


################################################################################
# Get Testing Set Performance
# calculate RMSE for all model objects in model_list
################################################################################
get_rmse_testing <- function(models_list, testing_set) {

  models_list %>%
    # caret::predict() can take a list of train objects as input
    predict(testing.set) %>%
    map_df(~sqrt(mean((testing.set[[target.label]]-.)^2)) ) %>%
    round(digits = 3) %>%
    t %>%
    as.data.frame %>%
    select(RMSE.testing = V1)
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
