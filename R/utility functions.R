
get_listelements_by_string <- function(input_list, search_string) {

  input_list %>%
    names %>%
    stringr::str_detect(search_string) %>%
    purrr::keep(input_list, .)
}
