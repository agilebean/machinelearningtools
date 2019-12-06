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
  data, predictor, limit = NULL, lexicon_label = "nrc"
) {
  require(dplyr)
  require(tidyr)
  require(tidytext) # for stopwords, get_sentiments

  nrc.dict <- get_sentiments(lexicon_label)

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

  # tricky: right_join (not inner_join) to keep all dict. sentiments
  sentiment.ranking <- right_join(unnested, nrc.dict)

  # convert sentiment ranking (tibble) into predictors
  result <- sentiment.ranking %>%
    group_by(sentiment) %>%
    summarise(weight = sum(count, na.rm = TRUE)) %>%
    # tricky: division by 0 would lead to NaN
    # mutate(weight = weight/sum(weight)) %>%
    mutate(weight = ifelse(weight == 0, 0, weight/sum(weight))) %>%
    # tricky: spread all sentiment levels into columns
    spread(sentiment, weight)

  return(result)
}
