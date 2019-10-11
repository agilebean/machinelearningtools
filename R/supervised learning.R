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
clusterOn <- function(no_cores = NULL, outfile_name = "") {

  require(doParallel) # loads parallel library for makeCluster

  cluster.new <- makeCluster(spec = if (!is.null(no_cores)) no_cores else { detectCores() - 1 },
                             type = "FORK",
                             outfile = outfile_name # verbose
  )
  registerDoParallel(cluster.new)

  return(cluster.new)

}

################################################################################
# turn off cluster for parallel processing
################################################################################
clusterOff <- function(cluster_name) {

  require(doParallel)

  if (nrow(showConnections()) !=  0) {

    stopCluster(cluster_name)
    # tricky: insert serial backend after stopping cluster, not before
    ## https://github.com/tobigithub/R-parallel/wiki/R-parallel-Errors
    registerDoSEQ()

  }
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

  # set testing set to argument > from models_list > NULL if empty
  if (!is.null(testing_set)) {
    testing.set <- testing_set
  } else if (is.null(models_list$testing.set)) {
    testing.set <- NULL
  } else if (nrow(models_list$testing.set) != 0) {
    testing.set <- models_list$testing.set
  } else { # e.g. if testingset exits but 0 rows
    testing.set <- NULL
  }

  # remove target.label + testing.set from models.list
  if (!is.null(models_list$target.label)) {
    models_list %<>% purrr::list_modify("target.label" = NULL)
  }
  if (!is.null(models_list$testing.set)) {
    models_list %<>% purrr::list_modify("testing.set" = NULL)
  }

  target <- models_list[[1]]$trainingData$.outcome

  if (is.factor(target)) {
    metric1 = "Accuracy"
    metric2 = "Kappa"
  } else if (is.numeric(target)) {
    metric1 = "RMSE"
    metric2 = "Rsquared"
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
    resamples.values, metric1, palette)
  metric2.resamples.boxplots <- visualize_resamples_boxplots(
    resamples.values, metric2, palette)

  if (!is.null(testing.set)) {
    metrics.testing <- get_testingset_performance(
      models_list, target.label, testing.set
    )
  } else {
    metrics.testing <- NULL
  }

  if (is.factor(target)) {

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
    {
      if (!is.null(metrics.testing)) {
        # tricky: within conditional {} block, must reference to LHS (.)
        merge(., metrics.testing, by = "model")
      } else {
        .
      }
    } %>%
      as_tibble(.)

  } else if (is.numeric(target)) {

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
    {
      if (!is.null(metrics.testing)) {
        # tricky: within conditional {} block, must reference to LHS (.)
        merge(., metrics.testing, by = "model") %>%
          mutate(RMSE.delta = RMSE.testing - RMSE.mean) %>%
          arrange(RMSE.testing)
      } else {
        .
      }
    } %>%
      as_tibble(.)
  }

  return(list(metric1.training = metric1.training,
              metric2.training = metric2.training,
              metric1.resamples.boxplots = metric1.resamples.boxplots,
              metric2.resamples.boxplots = metric2.resamples.boxplots,
              metrics.testing = metrics.testing,
              benchmark.all = benchmark.all
  ))
}


################################################################################
# get_metrics_from_resamples
# Helper function for get_model_metrics
################################################################################
get_metric_from_resamples <- function(resamples_values, metric) {

  require(dplyr)

  suffix <- paste0("~", metric)
  # tricky: for arrange, convert string column name to symbol, not quosure
  # https://stackoverflow.com/a/26497839/7769076
  metric.mean <- rlang::sym(paste0(metric,".mean"))
  metric.sd <- paste0(metric,".sd")

  # metric_table <-
  resamples_values %>%
    ## tricky: dplyr::mutate doesn't work here
    map_df(~c(mean = mean(.), sd = sd(.) )) %>%
    dplyr::select(ends_with(suffix)) %>%
    rename_all(.funs = funs(gsub(suffix, "",.))) %>%
    t %>% as_tibble(rownames = "model") %>%
    setNames(c("model", metric.mean, metric.sd)) %>%
    {
      if (metric == "RMSE") {
        # tricky: unquote symbol, not quosure
        # tricky: must use . inside inline dplyr code {}
        arrange(., !!metric.mean)
      } else {
        # for Accuracy, Kappa AND Rsquared
        arrange(., desc(!!metric.mean))
      }
    }
}

################################################################################
# get_metric_resamples
# Helper function for tidyposterior
################################################################################
get_metric_resamples <- function(resamples_data, metric) {

  resamples_data %>%
    .$values %>%
    as_tibble() %>%
    select(Resample, contains(metric)) %>%
    # tricky: tilde (~) NOT dash (-)
    setNames(gsub(paste0("~", metric), "", names(.))) %>%
    rename(id = Resample)
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

#######################################################################
# define output filename
#######################################################################
output_filename <- function(prefix, target_label, features_set_label,
                            cv_repeats, impute_method) {
  paste0(c(prefix,
           target_label, features_set_label,
           paste0(cv_repeats, "repeats"),
           { if (!is.null(impute_method)) paste(impute_method) else "noimpute"},
           "rds"),
         collapse = ".") %T>% print
}

#######################################################################
# benchmark algorithms with caret::train
#######################################################################
benchmark_algorithms <- function(

  target_label,
  features_labels,
  training_set,
  testing_set,
  formula_input = FALSE,
  preprocess_configuration = c("center", "scale", "nzv"),
  training_configuration,
  impute_method = NULL,
  algorithm_list,
  glm_family = NULL,
  seed = 17, split_ratio = 0.80,
  cv_repeats, try_first = NULL,
  models_list_name = NULL,
  beep = TRUE,
  push = TRUE) {

  ########################################
  ## 2.3 Select the target & features
  ########################################
  target_label %>% print
  features_labels %>% print

  ########################################
  # 3.2: Select the target & features
  ########################################
  target <- training_set[[target_label]]
  # avoid tibble e.g. for svmRadial: "setting rownames on tibble is deprecated"
  features <- training_set %>% select(features_labels) %>% as.data.frame

  ########################################
  # 3.3: Train the models
  ########################################
  models.list <- list()

  if (formula_input) {

    print("******** FORMULA interface")

    # define formula
    formula1 <- set_formula(target_label, features_labels)

    system.time(
      models.list <- algorithm_list %>%

        map(function(algorithm_label) {

          print(paste("***", algorithm_label))

          ############ START new cluster for model training
          cluster.new <- clusterOn()

          if (algorithm_label == "rf") {

            model <- train(
              form = formula1,
              method = "rf",
              data = if (is.null(try_first)) training_set else head(training_set, try_first),
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = TRUE
            )

            # logistic regression
          } else if (algorithm_label == "glm" | algorithm_label == "glmnet") {

            model <- train(
              form = formula1,
              method = algorithm_label,
              family = glm_family,
              data = if (is.null(try_first)) training_set else head(training_set, try_first),
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )
          } else {

            model <- train(
              form = formula1,
              method = algorithm_label,
              data = if (is.null(try_first)) training_set else head(training_set, try_first),
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )
          }
          ############ END model training & STOP cluster
          cluster.Off()

          return(model)
        }) %>%
        setNames(algorithm_list)
    ) %T>% {
      if (beep) beepr::beep()
      if (push) push_message(.["elapsed"], algorithm_list)
    }
    # categorical variables -> x,y interface
  } else {

    print("******** X Y INTERFACE")

    # check if dataset contains categorical features
    contains_factors <- training_set %>%
      select_if(is.factor) %>% names %>% {length(.) > 0}

    # transform categorical features by one-hot-encoding for models except rf, ranger, gbm
    # e.g. glmnet expects features as model.matrix (source: https://stackoverflow.com/a/48230658/7769076)
    if (contains_factors) {

      formula1 <- set_formula(target_label, features_labels)
      features.onehotencoded <- model.matrix(formula1, data = training_set)
      testing.set <- model.matrix(formula1, data = testing.set)
    }

    # models that can handle factors instead of one-hot-encoding
    algorithms.handling.factors <- c("rf", "ranger", "gbm", "nnet")

    system.time(
      models.list <- algorithm_list %>%

        map(function(algorithm_label) {

          print(paste("***", algorithm_label))

          # transform factors by one-hot-encoding for all models except rf, ranger, gbm
          if (contains_factors & (!algorithm_label %in% algorithms.handling.factors)) {

            features <- features.onehotencoded
            print(paste("*** performed one-hot-encoding for model", algorithm_label))

          }
          ############ START new cluster for model training
          cluster.new <- clusterOn()

          if (algorithm_label == "rf") {

            model <- train(
              x = features,
              y = target,
              method = "rf",
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = TRUE
            )

            # logistic regression
          } else if (algorithm_label == "glm" & class(target) == "factor") {

            model <- train(
              x = features,
              y = target,
              method = "glm",
              family = glm_family,
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )

          } else if (algorithm_label == "xgbTree" | algorithm_label == "xgbLinear") {

            model <- train(
              x = features,
              y = target,
              method = algorithm_label,
              nthread = 1,
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )

          } else if (algorithm_label == "svmRadial" | algorithm_label == "svmLinear") {

            # predict() requires kernlab::ksvm object created by formula:
            # https://stackoverflow.com/q/52743663/7769076
            formula.svm <- set_formula(target_label, features_labels)

            model <- train(
              form = formula.svm,
              method = algorithm_label,
              data = if (is.null(try_first)) training_set else head(training_set, try_first),
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )

          } else {

            model <- train(
              x = features,
              y = target,
              method = algorithm_label,
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )
          }
          ############ END model training & STOP cluster
          clusterOff(cluster.new)

          return(model)
        }) %>%
        setNames(algorithm_list)
    ) %T>% {
      if (beep) beepr::beep()
      if (push) push_message(.["elapsed"], algorithm_list)
    }
  }

  # stop cluster if training throws error (https://stackoverflow.com/a/41679580/7769076)
  on.exit(if (exists("cluster.new")) { stopCluster(cluster.new) } )

  ########################################
  # Postprocess the models
  ########################################
  # add target.label & testing.set to models.list
  models.list$target.label <- target_label
  models.list$testing.set <- testing_set

  # save the models.list
  if (!is.null(models_list_name)) {

    models.list %>% saveRDS(models_list_name)

    print(paste("model training results saved in", models_list_name))
  }

  return(models.list)
}

################################################################################
# Dataset contains Factors
# check if dataset contains categorical features
################################################################################
contains_factors <- function(data) {
  data %>%
    select_if(is.factor) %>%
    names %>%
    {length(.) > 0}
}

################################################################################
# Algorithm handles Factors
# Check if algorithm handles categorical features without one-hot-encoding
################################################################################
handles_factors <- function(algorithm_label) {

  # models that can handle factors instead of one-hot-encoding
  algorithms.handling.factors <- c("rf", "ranger", "gbm", "nnet")
  algorithm_label %in% algorithms.handling.factors
}

################################################################################
# Get feature set
# From vector of feature labels, generate feature set
################################################################################
get_featureset <- function(data,
                           target_label = NULL,
                           featureset_labels = NULL,
                           select_starts = NULL) {

  data %>%
    dplyr::select(!!rlang::sym(target_label)) %>%

    {
      if (!is.null(featureset_labels)) {
        cbind(.,
              data %>%
                dplyr::select(!!!rlang::syms(featureset_labels))
        )
      } else { . }
    } %>%
    {
      if (!is.null(select_starts)) {

        cbind(.,
              map_dfc(select_starts, function(start_keyword) {
                data %>%
                  select(starts_with(start_keyword))
              })
        )

      } else { . }
    } %>%
    as_tibble()
}

################################################################################
# Get Testing Set Performance
# calculate RMSE for all model objects in model_list
################################################################################
get_testingset_performance <- function(
  models_list, target_label = NULL, testing_set = NULL) {

  # remove target.label + testing.set from models.list
  if (!is.null(models_list$target.label) & !is.null(models_list$testing.set)) {

    target.label <- models_list$target.label
    testing.set <- models_list$testing.set
    models_list %<>% purrr::list_modify("target.label" = NULL, "testing.set" = NULL)

  } else if (!is.null(target_label) & !is.null(testing_set)) {

    target.label <- target_label
    testing.set <- testing_set
  }

  features.labels <- testing.set %>% select(-target.label) %>% names

  observed <- testing.set[[target.label]]

  if (is.factor(observed)) {

    models_list %>%
      map(
        function(model_object) {

          if (contains_factors(testing.set) & !handles_factors(model_object$method)) {
            formula1 <- set_formula(target.label, features.labels)
            testing.set <- model.matrix(formula1, data = testing.set)
          }
          model_object %>%
            # estimate target in the testing set
            predict(., newdata = testing.set) %>%
            confusionMatrix(., observed) %>%
            .$overall %>%
            # tricky: convert first to dataframe > can select column names
            map_df(1) %>% select(Accuracy, Kappa)
        }
      ) %>%
      bind_rows(.id = "model") %>%
      setNames(c("model", "Acc.testing", "Kappa.testing"))

  } else if (is.numeric(observed)) {

    models_list %>%
      # caret::predict() can take a list of train objects as input
      predict(testing.set) %>%

      map_df(function(predicted) {

        mean.training.set <- models_list[[1]]$trainingData$.outcome %>% mean

        # predicted <- predict(model_object, testing.set) %T>% print
        c(
          # postResample(predicted, observed) %>% .["RMSE"],
          sqrt(mean((observed - predicted)^2)),
          # https://stackoverflow.com/a/36727900/7769076
          sum((predicted - mean.training.set)^2) / sum((observed - mean.training.set)^2),
          # R2 = regression SS / TSS
          ## sum((predicted - mean(predicted))^2) / sum((observed - mean(observed))^2),
          ## the same reference (observed) seems to be better
          sum((predicted - mean(observed))^2) / sum((observed - mean(observed))^2),
          # postResample(predicted, observed) %>% .[("Rsquared")],
          cor(predicted, observed)^2
        )
      }) %>%
      t %>%
      as_tibble(rownames = "model") %>%
      rename(RMSE.testing = V1, R2.testing = V2,
             R2.testing2 = V3,  R2.postResample= V4) %>%
      arrange(RMSE.testing) %>% as.data.frame
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
push_message <- function(time_in_seconds = 60, algorithm_list = "") {

  beepr::beep("facebook")
  RPushbullet::pbPost(type = "note",
                      title = paste("caret training finished after",
                                    round(time_in_seconds/60, digits = 2), "min"),
                      body = paste("The training finished for models:",
                                   paste0(algorithm_list, collapse = ", ")),
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