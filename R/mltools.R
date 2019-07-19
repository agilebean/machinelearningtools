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
clusterOn <- function(no_cores = NULL) {

  require(doParallel) # loads parallel library for makeCluster
  cluster.new <- makeCluster(spec = if (!is.null(no_cores)) no_cores else { detectCores() - 1 },
                             type = "FORK",
                             outfile = "" # verbose
  )
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
get_model_metrics <- function(models_list,
                              target_label = NULL,
                              testing_set = NULL,
                              palette = "Set1", direction = 1,
                              colors = NULL,
                              boxplot_fill = "grey95",
                              boxplot_color = "grey25") {
  require(dplyr)
  require(purrr)
  require(ggplot2)
  require(RColorBrewer)

  # retrieve target.label & testing.set from models_list
  target.label <- if (!is.null(target_label)) target_label else models_list$target.label
  testing.set <- if (!is.null(testing_set)) testing_set else models_list$testing.set

  if (is.factor(testing.set[[target.label]])) {
    metric1 = "Accuracy"
    metric2 = "Kappa"
  } else if (is.numeric(testing.set[[target.label]])) {
    metric1 = "RMSE"
    metric2 = "Rsquared"
  }

  # remove target.label + testing.set from models.list
  if (!is.null(models_list$target.label) & !is.null(models_list$testing.set)) {
    models_list %<>% head(-2)
  }

  ### get metrics from original resamples' folds
  resamples.values <- models_list %>% resamples %>% .$values %>%
    # select_if(is.numeric) %>%
    # retrieve RMSE, Rsquared but not MAE
    ## tricky: select without dplyr:: prefix does NOT work
    # dplyr::select(ends_with("RMSE"), ends_with("Rsquared"))
    dplyr::select(ends_with(metric1), ends_with(metric2))

  ### calculate mean and sd for each metric
  metric1.training <- get_metric_from_resamples(resamples.values, metric1)
  metric2.training <- get_metric_from_resamples(resamples.values, metric2)

  ### visualize the resampling distribution from cross-validation
  metric1.resamples.boxplots <- visualize_resamples_boxplots(
    resamples.values, metric1)
  metric2.resamples.boxplots <- visualize_resamples_boxplots(
    resamples.values, metric2)

  metric1.testing <- get_testingset_performance(
    # tricky: target.label & testing.set NOT target_label & testing_set
    target.label, models_list, testing.set)

  if (is.factor(testing.set[[target.label]])) {

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
      merge(metric1.testing, by = "model")

  } else if (is.numeric(testing.set[[target.label]])) {

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
      merge(metric1.testing, by = "model") %>%
      mutate(delta = RMSE.testing - RMSE.mean) %>%
      arrange(RMSE.testing) %>%
      as_tibble
  }

  return(list(metric1.training = metric1.training,
              metric2.training = metric2.training,
              metric1.resamples.boxplots = metric1.resamples.boxplots,
              metric2.resamples.boxplots = metric2.resamples.boxplots,
              metric1.testing = metric1.testing,
              benchmark.all = benchmark.all
  ))
}

################################################################################
# get_metrics_from_resamples
# Helper function for get_model_metrics
################################################################################
get_metric_from_resamples <- function(resamples_values, metric) {

  suffix <- paste0("~", metric)
  # tricky: for arrange, convert string column name to symbol, not quosure
  # https://stackoverflow.com/a/26497839/7769076
  metric.mean <- rlang::sym(paste0(metric,".mean"))
  metric.sd <- paste0(metric,".sd")

  metric_table <- resamples_values %>%
    ## tricky: dplyr::mutate doesn't work here
    map_df(~c(mean = mean(.), sd = sd(.) )) %>%
    dplyr::select(ends_with(suffix)) %>%
    rename_all(.funs = funs(gsub(suffix, "",.))) %>%
    t %>% as_tibble(rownames = "model") %>%
    setNames(c("model", metric.mean, metric.sd)) %>%
    # tricky: unquote symbol, not quosure
    arrange(desc(!!metric.mean))
}

################################################################################
# visualize_resamples_boxplots()
# Helper function for get_model_metrics
################################################################################
visualize_resamples_boxplots <- function(resamples_values, METRIC,
                                         palette = "Set1",
                                         colour_count = ncol(resamples_values),
                                         dot_size = 1/logb(nrow(resamples_values), 5),
                                         boxplot_fill = "grey95", boxplot_color = "grey25",
                                         colors = NULL) {

  require(dplyr)
  require(ggplot2)

  # create palette with 8+ colors
  ## Source: http://novyden.blogspot.com/2013/09/how-to-expand-color-palette-with-ggplot.html
  getPalette <- colorRampPalette(brewer.pal(8, palette))(colour_count)

  ### visualize the resampling distribution from cross-validation
  resamples.boxplots <- resamples_values %>%
    dplyr::select(ends_with(METRIC)) %>%
    purrr::set_names(~gsub(paste0("~", METRIC), "", .)) %>%
    drop_na() %>%
    gather(key = model, value = METRIC) %>%
    ggplot(aes(x = reorder(model, METRIC, median),
               y = METRIC, color = model)) +
    theme_minimal() +
    geom_jitter(size = dot_size) +
    geom_boxplot(width = 0.7, fill = boxplot_fill, color = boxplot_color, alpha = 0.3) +
    coord_flip() +
    scale_color_manual(values = if (!is.null(colors)) colors else getPalette) +
    labs(x = "model", y = METRIC) +
    theme(legend.position = "none",
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14))

  return(resamples.boxplots)
}


################################################################################
# Get Models List
# read all model.list files from permutation_list's target & features set
################################################################################
get_models_list <- function(permutation_list, model_index = 1,
                            prefix = "data/models.list",
                            impute_method = "medianImpute",
                            cv_repeats = 100) {

  permutation <- permutation_list %>% map_df(model_index) %>% print

  models.list.name <- paste(prefix,
                            permutation$target_label,
                            permutation$features_set,
                            paste0(cv_repeats, "repeats"),
                            impute_method,
                            "rds",
                            sep = ".") %T>% print

  models.list  <- readRDS(models.list.name)

  return(models.list)
}

################################################################################
# Get Testing Set Performance
# calculate RMSE for all model objects in model_list
################################################################################
get_testingset_performance <- function(target_label, models_list, testing_set) {

  if (is.factor(testing_set[[target_label]])) {

    models_list %>%
      map(
        # estimate target in the testing set
        ~predict(., newdata = testing_set) %>%
          confusionMatrix(., testing_set[[target_label]]) %>%
          .$overall %>%
          # tricky: convert first to dataframe > can select column names
          map_df(1) %>% select(Accuracy, Kappa)
      ) %>%
      bind_rows(.id = "model") %>%
      setNames(c("model", "Acc.testing", "Kappa.testing"))

  } else if (is.numeric(testing_set[[target_label]])) {

    models_list %>%
      # caret::predict() can take a list of train objects as input
      predict(testing_set) %>%
      map_df(~sqrt(mean( (testing_set[[target_label]]-.)^2) ) ) %>%
      # simpler than: mutate_if(is.numeric, funs(round(., digits = 3)))
      t %>%
      as_tibble(rownames = "model") %>%
      rename(RMSE.testing = V1) %>%
      arrange(RMSE.testing)
  }
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

################################################################################
# Send push message to RPushbullet app
# input caret::train object
################################################################################
push_message <- function(time_in_seconds = 60) {

  beepr::beep("facebook")
  RPushbullet::pbPost(type = "note",
                      title = paste("caret training finished after",
                                    round(time_in_seconds/60, digits = 2), "min"),
                      body = paste("The training for models finished"),
                      devices = "ujyr8RSNXs4sjAsoeMFET6")
}

################################################################################
#
# LESSONS LEARNED
#
################################################################################
#
################################################################################
# 1. dplyr: never ever select() but dplyr::select()
#
################################################################################
# 2. unified handling: use passed arguments or default values
#
# target.label <- if (!is.null(target_label)) target_label else models_list$target.label
#
################################################################################
# 3. dplyr: extended dplyr verbs work with .f, FUN, funs()
#
# ... %>% rename_all(funs(gsub(suffix, "", .))) %>% ...
#
################################################################################
# 4. dplyr: conditional sorting with dplyr
#
# arrange( {if (desc) desc(mean) else mean } )
#
################################################################################
# 5. purrr: apply several functions on column values AND bind with c()
#
# metric_table <- resamples.values %>%
#   map_df(function(variable) {
#     ## tricky: dplyr::mutate doesn't work here
#     c(mean = mean(variable), sd = sd(variable))
#   })
#
################################################################################
# 6. purrr: rename column names by set_names() or rename_all()
#
# resamples.values %>%
#   dplyr::select(ends_with("~RMSE")) %>%
#   set_names(~gsub("~RMSE","",.)) %>% ...
# OR
# metric_table %>%
#   dplyr::select(ends_with(suffix)) %>%
#   rename_all(funs(gsub(suffix, "", .))) %>% ...
#
################################################################################
# 7. visualization:   # create palette with 8+ colors
#
# getPalette <- colorRampPalette(brewer.pal(8, palette))(length(models.list))
# ...
# scale_color_manual(values = if (!is.null(colors)) colors else getPalette)
#
################################################################################
# 8. basic: match 2 dataframes by key column
#
# merge(RMSE.training, RMSE.testing, by = "model")
#
################################################################################
# 9. basic: paste0() requires a vector for collapse,
# but paste() can handle a strings separated by comma
#
# paste0(c("analysis", target.label, features.set, "pdf"), collapse = ".")
# paste("analysis", target.label, features.set, "pdf", sep = ".")
#
################################################################################
# 10. purrr: purrr style is shorter than base inline function style: function(x) {}
#
# models_list %>%
#   predict(testing_set) %>%
#   map_df(~sqrt(mean( (testing_set[[target_label]]-.)^2) ) )
#
################################################################################
# 11. dplyr: rename column without mutate/rename by select()
#
# as.data.frame %>%
#   select(RMSE.testing = V1)
#
################################################################################
# 12. dplyr: make row names explicit and rename "rowname"
#
# rownames_to_column(var = "model")
#
################################################################################
# 13. dplyr: transpose as.tibble instead of as.data.frame
#
# instead of:
#   round(digits = 3) %>% t %>% as.data.frame %>%
#     rownames_to_column(var = "model")
# do:
#   t %>% as.tibble(rownames = "model")
#
################################################################################
