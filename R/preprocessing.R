perform_max_SMOTE <- function(features, target, algorithm = "SMOTE") {

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
