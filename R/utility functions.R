
#######################################################################
# define output path from directories and create if don't exist
#######################################################################
output_dir <- function(..., file = "") {
  # create dir path
  dirs = file.path(...)
  # create dir (and subdirs) if they don't exist
  if(!dir.exists(dirs) & !file.exists(dirs)) {
    dir.create(dirs, recursive = TRUE)
  }
  # return filepath under dirs
  file.path(dirs, file)
}

#######################################################################
# define output filename
#######################################################################
output_filename <- function(prefix = NULL, ..., suffix = "rds") {

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
# get_list_elements_by_string
#######################################################################
get_list_elements_by_string <- function(input_list, search_string) {
  require(purrr)
  input_list %>%
  # keep requires logical vector > imap_lgl
  # .y searches on the name of list element
  keep( imap_lgl(.,  ~ grepl(search_string, .y )))
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
