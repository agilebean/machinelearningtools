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

  cluster.new <- makeCluster(spec = if (!is.null(no_cores)) no_cores else { detectCores() },
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
get_model_metrics <- function(
  models_list, target_label = NULL, testing_set = NULL,
  median_sort = FALSE, reverse = FALSE,
  palette = "Set1", colors = NULL,
  boxplot_fill = "grey95", boxplot_color = "grey25") {

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
    metric3 = NULL
    metric1.descending = FALSE
    metric2.descending = FALSE
    metric3.descending = FALSE

  } else if (is.numeric(target)) {

    metric1 = "RMSE"
    metric2 = "Rsquared"
    metric3 = "R"
    metric1.descending = TRUE
    metric2.descending = FALSE
    metric3.descending = FALSE
  }

  if (reverse) {
    metric1.descending
    metric2.descending
    metric3.descending
  }

  ### get metrics from original resamples' folds
  resamples.values <- models_list %>% caret::resamples() %>% .$values %>%
    # select_if(is.numeric) %>%
    # retrieve RMSE, Rsquared but not MAE
    ## tricky: select without dplyr:: prefix does NOT work
    # dplyr::select(ends_with("RMSE"), ends_with("Rsquared"))
    dplyr::select(ends_with(metric1), ends_with(metric2)) %>%
    # calculate R from R-squared variables
    mutate(
      across(
        .cols = ends_with(metric2), # R-squared
        .fns = sqrt,
        .names = "{.col}.R"
      )
    ) %>%
    set_names(gsub(paste0(metric2, ".R"), "R", names(.)))

  ### calculate mean and sd for each metric
  metric1.training <- get_metric_from_resamples(
    resamples.values, metric1, median_sort)
  metric2.training <- get_metric_from_resamples(
    resamples.values, metric2, median_sort)
  metric3.training <- switch (
    !is.null(metric3), # instead of: is.numeric(target) = test on a vector
    get_metric_from_resamples(resamples.values, metric3, median_sort),
    NULL
  )

  ### visualize the resampling distribution from cross-validation
  metric1.resamples.boxplots <- visualize_resamples_boxplots(
    resamples.values, metric1, palette, colors = colors, metric1.descending)
  metric2.resamples.boxplots <- visualize_resamples_boxplots(
    resamples.values, metric2, palette, colors = colors, metric2.descending)

  metric3.resamples.boxplots <- switch (
    !is.null(metric3),
    visualize_resamples_boxplots(
      resamples.values, metric3, palette, colors = colors, metric3.descending),
    NULL)

  if (!is.null(testing.set)) {
    metrics.testing <- get_testingset_performance(
      models_list, target.label, testing.set
    )
  } else {
    metrics.testing <- NULL
  }

  if (is.factor(target)) { # classification

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
      {
        if (!is.null(metrics.testing)) {
          # tricky: within conditional {} block, must reference to LHS (.)
          merge(., metrics.testing, by = "model") %>%
            arrange(desc(Acc.testing))
        } else {
          .
        }
      } %>%
      as_tibble(.)

  } else if (is.numeric(target)) { # regression

    benchmark.all <- merge(metric1.training, metric2.training, by = "model") %>%
      merge(metric3.training, by = "model") %>%
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

  return(list(metric1 = metric1,
              metric2 = metric2,
              resamples.values = resamples.values,
              metric1.training = metric1.training,
              metric2.training = metric2.training,
              metric3.training = metric3.training,
              metric1.resamples.boxplots = metric1.resamples.boxplots,
              metric2.resamples.boxplots = metric2.resamples.boxplots,
              metric3.resamples.boxplots = metric3.resamples.boxplots,
              metrics.testing = metrics.testing,
              benchmark.all = benchmark.all
  ))
}


################################################################################
# get_metric_from_resamples
# Helper function for get_model_metrics
################################################################################
get_metric_from_resamples <- function(
  resamples_values, metric, median_sort = FALSE) {

  require(dplyr)

  suffix <- paste0("~", metric)
  # tricky: for arrange, convert string column name to symbol, not quosure
  # https://stackoverflow.com/a/26497839/7769076
  metric.mean <- rlang::sym(paste0(metric,".mean"))
  metric.sd <- paste0(metric,".sd")
  metric.median <- rlang::sym(paste0(metric,".median"))

  sort.metric <- ifelse(median_sort, metric.median, metric.mean)

  resamples_values %>%
    dplyr::select(ends_with(suffix)) %>%
    rename_with(~gsub(suffix, "", .)) %>%
    summarize(across(everything(),
                     list(median = median, mean = mean, sd = sd))) %>%
    # genius tip (.value!): https://stackoverflow.com/a/58880309/7769076
    pivot_longer(
      cols = everything(),
      names_to = c("model", ".value"),
      names_pattern =  "(.+)_(.+$)"
    ) %>%
    set_names(c(
      "model",
      as.character(metric.median),
      as.character(metric.mean),
      metric.sd
    )) %>%
    { # first columns mean+sd if not sorted by median
      if (!median_sort) {
        select(., model, ends_with("mean"), ends_with("sd"), ends_with("median"))
      } else { . }
    } %>%
    {
      if (metric == "RMSE") {
        # tricky: unquote symbol, not quosure
        # tricky: must use . inside inline dplyr code {}
        arrange(., !!sort.metric)
      } else { # for Accuracy, Kappa AND Rsquared: sort by descending order

        arrange(., desc(!!sort.metric))
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
visualize_resamples_boxplots <- function(
  resamples_values,
  METRIC,
  palette = "Set1",
  descending = FALSE,
  color_count = NULL,
  dot_size = NULL,
  boxplot_fill = "grey95",
  boxplot_color = "grey25",
  colors = NULL,
  exclude_light_hues = NULL
) {
  require(dplyr)
  require(ggplot2)
  require(RColorBrewer)

  # dot size of resamples distribution is indirectly proportional to their #
  if (is.null(dot_size)) dot_size <- 1/logb(nrow(resamples_values), 5)

  # extract the resamples values for selected METRIC (e.g. "Accuracy" or "RMSE")
  resamples.by.metric <- resamples_values %>%
    dplyr::select(ends_with(METRIC)) %>%
    purrr::set_names(~ gsub(paste0("~", METRIC), "", .)) %>%
    drop_na() %>%
    pivot_longer(
      cols = everything(),
      names_to = "model",
      values_to = METRIC,
      names_transform = list(model = as.factor)
    )

  # create HEX color codes from palette with 8+ colors
  ## Source: http://novyden.blogspot.com/2013/09/how-to-expand-color-palette-with-ggplot.html
  color.codes <- brewer.pal(8, palette)

  # remove the first color codes of palette as they have very light hues
  if (!is.null(exclude_light_hues)) {
    color.codes %<>% .[-c(1:exclude_light_hues)]
  }

  # the # colors needed depends on # extracted resamples for selected METRIC
  if (is.null(color_count)) color_count <- ncol(resamples_values)

  # generate the color palette by extrapolation from color.codes to color_count
  color.palette.generated <- colorRampPalette(color.codes)(color_count)

  resamples.boxplots <- resamples.by.metric %>%
    ggplot(aes(
      {
        if (descending) {
          x = reorder(model, desc(!!sym(METRIC)), median)
        } else {
          x = reorder(model, !!sym(METRIC), median)
        }
      },
      y = !!sym(METRIC),
      color = model
    )) +
    geom_boxplot(
      width = 0.7,
      fill = boxplot_fill,
      color = boxplot_color,
      alpha = 0.3
    ) +
    geom_jitter(size = dot_size) +
    coord_flip() +
    scale_color_manual(
      values = if (!is.null(colors)) colors else color.palette.generated
    ) +
    labs(x = "model", y = METRIC) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14)
    )

  return(resamples.boxplots)
}

#######################################################################
# define output filename
#######################################################################
output_filename <- function(prefix, ..., suffix = "rds") {

  require(dplyr)
  require(purrr)
  # tricky: enable variable names that are not defined (NULL)
  dots <- list(...) %>% discard(is.null)

  paste0(
    # tricky: c(list(of characters)) inserts escape characters (\")
    c(prefix, dots, suffix, recursive = TRUE),
    collapse = "."
  )
}

#######################################################################
# define string in filename
#######################################################################
logical_string <- function(logical_flag, true_string) {

  if (logical_flag) true_string else NULL

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
  preprocess_configuration = c("center", "scale", "zv"),
  training_configuration,
  impute_method = NULL,
  algorithm_list,
  glm_family = NULL,
  seed = 17,
  cv_repeats,
  try_first = NULL,
  models_list_name = NULL,
  cluster_log = "",
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

  if (!is.null(try_first) & is.numeric(try_first)) {

    target %<>% head(try_first)
    features %<>% head(try_first)
    training_set %<>% head(try_first)

  }

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
          cluster.new <- clusterOn(outfile_name = cluster_log)
          # stop cluster if training throws error (https://stackoverflow.com/a/41679580/7769076)
          on.exit(if (exists("cluster.new")) { clusterOff(cluster.new) } )

          if (algorithm_label == "rf") {

            model <- train(
              form = formula1,
              method = algorithm_label,
              data = training_set,
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = TRUE
            )

          } else if (algorithm_label == "ranger") {

            model <- train(
              form = formula1,
              method = algorithm_label,
              data = training_set,
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = "impurity"
            )

          } else if (algorithm_label == "glm" | algorithm_label == "glmnet") {

            model <- train(
              form = formula1,
              method = algorithm_label,
              family = glm_family,
              data = training_set,
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )
          } else {

            model <- train(
              form = formula1,
              method = algorithm_label,
              data = training_set,
              preProcess = preprocess_configuration,
              trControl = training_configuration
            )
          }
          ############ END model training & STOP cluster
          clusterOff(cluster.new)
          stopImplicitCluster()

          return(model)
        }) %>%
        setNames(algorithm_list)
    ) %T>% {
      if (beep) beepr::beep()
      if (push) push_message(
        time_in_seconds = .["elapsed"],
        algorithm_list = algorithm_list,
        models_list_name = models_list_name
      )
    }
    # categorical variables -> x,y interface
  } else {

    print("******** X Y INTERFACE")

    # transform categorical features by one-hot-encoding for models except rf, ranger, gbm
    # e.g. glmnet expects features as model.matrix (source: https://stackoverflow.com/a/48230658/7769076)
    if (contains_factors(training_set)) {

      formula1 <- set_formula(target_label, features_labels)
      features.onehot <- model.matrix(formula1, data = training_set) %>%
        as.data.frame() %>%
        select(-`(Intercept)`)
      # training.set.onehot <- cbind(target, features.onehot)
    }
    # backup original features before loop to avoid overriding
    features.original <- features
    # training.set.original <- training_set

    system.time(
      models.list <- algorithm_list %>%

        map(function(algorithm_label) {

          print(paste("***", algorithm_label))

          # transform factors by one-hot-encoding for all models except rf, ranger, gbm
          if (contains_factors(training_set) &
              !handles_factors(algorithm_label)
              & !algorithm_label %in% c("svmRadial", "svmLinear")
          ) {

            features <- features.onehot
            # training.set <- training.set.onehot
            print(paste("*** performed one-hot-encoding for model", algorithm_label))

          } else { # no onehot-encoding

            features <- features.original
            # training.set <- training.set.original

          }

          ############ START new cluster for model training
          cluster.new <- clusterOn(outfile_name = cluster_log)
          # stop cluster if training throws error (https://stackoverflow.com/a/41679580/7769076)
          on.exit(if (exists("cluster.new")) { clusterOff(cluster.new) } )

          if (algorithm_label == "rf") {

            model <- train(
              x = features,
              y = target,
              method = algorithm_label,
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = TRUE
            )
          } else if (algorithm_label == "ranger") {

            model <- train(
              x = features,
              y = target,
              method = algorithm_label,
              preProcess = preprocess_configuration,
              trControl = training_configuration,
              importance = "impurity"
            )

          } else if (class(target) == "factor" &
                     (algorithm_label == "glm" | algorithm_label == "glmnet")
          ) {

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
              data = training_set,
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
          stopImplicitCluster()

          return(model)
        }) %>%
        setNames(algorithm_list)
    ) %T>% {
      if (beep) beepr::beep()
      if (push) push_message(
        time_in_seconds = .["elapsed"],
        algorithm_list = algorithm_list,
        models_list_name = if (!is.null(models_list_name)) models_list_name else NULL
      )
    }
  }

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
  print(models.list)
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
  algorithms.handling.factors <- c(
    "rf", "ranger", "gbm", "nnet"
  )

  # check whether imput algorithm handles factors
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

  # do onehot encoding for algorithms that cannot handle factors
  if (contains_factors(testing.set)) {

    formula1 <- set_formula(target.label, features.labels)
    testing.set.onehot <- model.matrix(formula1, data = testing.set) %>%
      as_tibble() %>%
      select(-`(Intercept)`)
  }

  if (is.factor(observed)) {

    models_list %>%
      map(
        function(model_object) {

          # set flag for onehot encoding
          onehot <- FALSE
          # do onehot encoding for algorithms that cannot handle factors
          if (contains_factors(testing.set) &
              !handles_factors(model_object$method) &
              !model_object$method %in% c("svmRadial", "svmLinear")) {

            onehot <- TRUE
          }

          model_object %>%
            # estimate target in the testing set
            predict(newdata = if (onehot) testing.set.onehot else testing.set) %>%
            confusionMatrix(., observed) %>%
            .$overall %>%
            # tricky: convert first to dataframe > can select column names
            map_df(1) %>%
            select(Accuracy, Kappa)
        }
      ) %>%
      bind_rows(.id = "model") %>%
      setNames(c("model", "Acc.testing", "Kappa.testing"))

  } else if (is.numeric(observed)) {

    models_list %>%
      map_df(
        function(model_object) {

          # set flag for onehot encoding
          onehot <- FALSE
          # do onehot encoding for algorithms that cannot handle factors
          if (contains_factors(testing.set) &
              !handles_factors(model_object$method) &
              !model_object$method %in% c("svmRadial", "svmLinear")) {

            onehot <- TRUE
          }

          mean.training.set <- models_list[[1]]$trainingData$.outcome %>% mean

          predicted <- model_object %>%
            # estimate target in the testing set
            predict(newdata = if (onehot) testing.set.onehot else testing.set)

          c(
            # postResample(predicted, observed) %>% .["RMSE"],
            sqrt(mean((observed - predicted)^2)),
            # https://stackoverflow.com/a/36727900/7769076
            sum((predicted - mean.training.set)^2) / sum((observed - mean.training.set)^2),
            # R2 = regression SS / TSS
            ## sum((predicted - mean(predicted))^2) / sum((observed - mean(observed))^2),
            ## ?for centering, the same reference (observed) seems to be better?
            sum((predicted - mean(observed))^2) / sum((observed - mean(observed))^2),
            # postResample(predicted, observed) %>% .[("Rsquared")]
            cor(predicted, observed)^2
          )
        }) %>%
      t %>%
      as_tibble(rownames = "model") %>%
      rename(RMSE.testing = V1, R2.testing = V2,
             R2.testing2 = V3,  R2.postResample= V4) %>%
      arrange(RMSE.testing) %>%
      as.data.frame
  }
}

################################################################################
# Visualize variable importance
# input caret::train object
################################################################################
visualize_importance <- function (
  model_object, # caret::train object
  relative = FALSE, # calculate relative importance scores (not normalized)
  axis_label = NULL, # label for vertical axis
  axis_tick_labels = NULL, # labels for items/facets/factors
  text_labels = FALSE, # labels showing numeric scores next to bar
  axis_limit = NULL, # max. axis score displayed
  width = 4, height = 3, dpi = 300, # specs for saved plot
  fill_color = "#114151",
  font_size = 10,
  save_label = "" # filename for saved plot
) {

  require(caret)
  require(gbm)
  require(dplyr)
  require(ggplot2)

  # calculate feature importance
  importance_object <- model_object %>% caret::varImp()

  unit.label <- ifelse(relative, "%RI", "importance")
  unit.variable <- rlang::sym(unit.label)


  if (class(importance_object) == "varImp.train") {
    importance_object %<>% .$importance
  }
  if (!hasName(importance_object, "rowname")) {
    importance_object %<>% rownames_to_column()
  }

  importance.table <- importance_object %>%
    rename(variable = rowname, importance = Overall) %>%
    arrange(desc(importance)) %>%
    {
      if (relative) {
        mutate(., `%RI` = importance/sum(importance)*100) %>%
          select(variable, `%RI`)
      } else {
        .
      }
    }

  importance.plot <- importance.table %>%
    set_names(c("variable", unit.label)) %>%
    ggplot(data = .,
           aes(x = reorder(variable, !!unit.variable), y = !!unit.variable)) +
    theme_minimal() +
    geom_bar(stat = "identity", fill = fill_color) +
    {
      if (text_labels) {
        geom_text(aes(label = round(!!unit.variable, digits = 2)),
                  position = position_dodge(width = 5),
                  hjust = -0.1,
                  check_overlap = TRUE,
                  # tricky: font size must be scaled down by ggplot2:::.pt
                  # https://stackoverflow.com/a/17312440/7769076
                  size = font_size / (ggplot2:::.pt * 1.1)
        )
      }
    } +
    coord_flip() +
    theme(axis.title = element_text(size = font_size),
          axis.text = element_text(size = font_size)) +
    {
      if (!is.null(axis_limit)) {
        scale_y_continuous(expand = c(0, 0),
                           limits = c(0, axis_limit))
      }
    } +
    {
      if (!is.null(axis_tick_labels)) {
        scale_x_discrete(labels = axis_tick_labels)
      }
    } +
    labs(
      x = axis_label,
      y = unit.label
    )

  if (save_label != "") {
    ggsave(
      filename = save_label,
      plot = importance.plot,
      dpi = dpi,
      width = width,
      height = height
    )
  }

  return(
    list(
      importance.table = importance.table,
      importance.plot = importance.plot
    ))
}




################################################################################
# Send push message to RPushbullet app
# input caret::train object
################################################################################
push_message <- function(
  algorithm_list = NULL, models_list_name = NULL,
  time_in_seconds = 60, sound = "classical"
  ) {

  algorithm_list_string <- if (!is.null(algorithm_list)) {
    paste("for machine learning algorithms:", paste0(algorithm_list, collapse = ", "))
  } else {
    ""
  }

  models_list_string <- if (!is.null(models_list_name)) {
    paste(", and is saved under the filename", models_list_name)
  } else {
    ""
  }

  pushoverr::pushover(
    title = paste("ml training finished after",
                  round(time_in_seconds/60, digits = 2), "min"),
    message = paste("Trained models: ",
                    algorithm_list_string, models_list_string),
    sound = sound
  )
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
