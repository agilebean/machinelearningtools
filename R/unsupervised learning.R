################################################################################
#
# Topic:   Unsupervised Learning
# Package: machinelearningtools
# Purpose: Provide convenience functions for machine learning with caret
#
################################################################################
iterate_factor_analysis <- function (items, max_factors = 5) {

  require(dplyr)
  require(purrr)
  require(psych)

  no_factors_labels <- 1:max_factors %>% as.character

  result <- map_dfr(no_factors_labels, function(no_factors) {

    pca.cor <- principal(items, nfactors = no_factors, covar = FALSE)
    pca.cov <- principal(items, nfactors = no_factors, covar = TRUE)
    pfa.cor <- fa(items, nfactors = no_factors, fm = "pa", cor = "cor")
    pfa.cov <- fa(items, nfactors = no_factors, fm = "pa", cor = "cov")

    list(pca.cor, pca.cov, pfa.cor, pfa.cov) %>%
      set_names(c("pca.cor", "pca.cov", "pfa.cor", "pfa.cov")) %>%
      map_df(function(EFA) {

        cor.label <- EFA$Call %>% as.character() %>% .[length(.) ]
        if(cor.label != "cor" & cor.label != "cov") {
          cor.label <- ifelse(cor.label == FALSE, "cor", "cov")
        }

        tibble(
          as.character(EFA$factors), EFA$fn, EFA$rms,
          EFA$fit.off, cor.label, EFA$chi,
          list(EFA), list(EFA$loadings), list(EFA$scores))

      }) %>%
      set_names(c("nfactors", "fn",  "RMSR", "fit.off",  "cor", "chi",
                  "model", "loadings", "scores")) %>%
      mutate_if(is.numeric, round, 2)

  })

  return(result)
}
