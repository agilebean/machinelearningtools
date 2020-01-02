# count word occurences of a free-text field for one predictor label
unnest_freetext <- function(data, predictor, limit = NULL) {

  require(tidytext)
  enquo(predictor)
  unnested <- data %>%
    select(!!predictor) %>%
    unnest_tokens(word, !!predictor) %>%
    filter(!word %in% stop_words$word) %>%
    group_by(word) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  # print only the first <limit> rows
  if (!is.null(limit)) {
    unnested <- unnested[1:limit, ]
  }

  return(unnested)
}

# get sentiment weights of a free-text field for one predictor label
sentiment_freetext <- function(
  data, text_column,
  lexicon = NULL, lexicon_label = "nrc",
  normalize = FALSE, no_rows = NULL
) {
  require(dplyr)
  require(tidyr)
  require(tidytext) # for stopwords, get_sentiments

  if (is.null(lexicon)) {
    lexicon <- get_sentiments(lexicon_label)
  }

  unnested <- data %>%
    select(!!text_column) %>%
    unnest_tokens(word, !!text_column) %>%
    filter(!word %in% stop_words$word) %>%
    group_by(word) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

  # print only the first <limit> rows
  if (!is.null(no_rows)) {
    unnested <- unnested[1:no_rows, ]
  }

  # tricky: right_join (not inner_join) to keep all dict. sentiments
  sentiment.ranking <- right_join(unnested, lexicon)

  # convert sentiment ranking (tibble) into predictors
  result <- sentiment.ranking %>%
    group_by(sentiment) %>%
    summarise(weight = sum(count, na.rm = TRUE)) %>%
    {
      if (normalize) {
        # tricky: division by 0 would lead to NaN
        # mutate(weight = weight/sum(weight)) %>%
        mutate(., weight = ifelse(weight == 0, 0, weight/sum(weight)))
      } else {
        .
      }
    } %>%
    # tricky: spread all sentiment levels into columns
    spread(sentiment, weight) %>% print

  return(result)
}
