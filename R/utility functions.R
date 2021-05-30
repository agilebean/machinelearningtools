
get_list_elements_by_string <- function(input_list, search_string) {

  require(purrr)
  input_list %>%
    # keep requires logical vector > imap_lgl
    # .y searches on the name of list element
    keep( imap_lgl(.,  ~ grepl(search_string, .y )))
}
