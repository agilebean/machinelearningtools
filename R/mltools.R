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
