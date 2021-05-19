################################################################################
# perform_max_SMOTE
# input:  features  dataframe
#         target    vector
#         max_k     k nearest neighbors for SMOTE
#         algorithm "SMOTE" (default), "ADASYN
#
################################################################################
perform_max_SMOTE <- function(
  features, target, max_k = 30, algorithm = "SMOTE") {

  require(smotefamily)
  for (k in max_k:1) {

    training.set <- NULL
    max_k_found <- FALSE

    tryCatch({

      if (algorithm == "SMOTE") {
        training.set <- smotefamily::SMOTE(features, target, K = k) %>% .$data

      } else if (algorithm == "ADASYN") {
        training.set <- smotefamily::ADAS(features, target, K = k) %>% .$data
      }


      # flag to break out of loop
      if (!is.null(training.set)) max_k_found <- TRUE

    },
    error = function(e) {
      cat("ERROR", conditionMessage(e), "\n")
    },
    warning = function(w) {
      # cat("warning", conditionMessage(w), "\n")
    }
    )
    if(max_k_found) {
      cat("SMOTE: Found maximum k =", k, "\n")
      break
    }
  }
  return(training.set)
}


################################################################################
# print_correlation_table_from_model
# input: caret::train object
################################################################################
print_correlation_table_from_model <- function(
  model_object, target_label, digits = 3) {

  # must set digits = 4 for mean() to return 3 decimals
  options.digits.bak <- getOption("digits")
  options(digits = digits)

  data.input <- model_object$trainingData %>%
    select(!!target_label := .outcome, everything()) %>%
    select(is.numeric) %>%
    as_tibble()

  # 1) for final table, move all variable names to rows
  data.transposed <- data.input %>%
    tibble::rownames_to_column() %>%
    pivot_longer(-rowname) %>%
    pivot_wider(
      # id_cols = name,
      names_from=rowname,
      values_from=value)

  # 2) calc mean+sd on data in rows
  data.stats <- data.transposed %>%
    # move data from original column vectors into row data
    nest(data = -name) %>%
    # create vectors from row data
    mutate(data = map(data, ~t(.x) %>% as.numeric)) %>%
    # calculate summary stats
    mutate(
      mean = map(data, ~mean(.x)),
      sd = map(data, ~sd(.x))
    ) %>%
    unnest(c(mean, sd)) %>%
    select(-data)

  # 3) create correlation matrix
  data.cor <- cor(data.input) %>%
    as.data.frame() %>%
    rownames_to_column()

  # 4) final table: merge desc stats with correlation matrix
  data.table <- merge(data.stats, data.cor,
                      by.x = "name", by.y = "rowname",
                      sort = FALSE
  ) %>% as_tibble()

  # 5) print final table
  html.table <- data.table %>%
    knitr::kable(format = "html", digits = digits) %>%
    kableExtra::kable_styling(bootstrap_options = c("bordered", "hover"))

  options(digits = options.digits.bak)

  return(list(
    html.table = html.table,
    data.table = data.table
  ))
}
