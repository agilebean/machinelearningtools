name_by_item_dict <- function(data, lv_item_dict) {

  names(data) <- lv_item_dict %>%
    map(pluck("item.labels")) %>%
    unlist %>%
    as.vector()

  return(data)
}

encode_by_item_dict <- function(data, lv_item_dict) {

  lv_item_dict %>%
    map( ~ .x %>%
           pluck("item.labels") %>%
           # put all the items into a list of scales
           data[.] %>%
           # tricky: use .x containing encoding defined in item.dict.e1
           encode_scale_labels(.x %>% pluck("encoding"))
    ) %>%
    set_names(lv_item_dict %>% map_chr(~ .x %>% pluck("scale.label")))
}

encode_survey_by_item_dict <- function(survey_data, lv_item_dict) {

  survey_data %>%
    name_by_item_dict(lv_item_dict) %>%
    encode_by_item_dict(lv_item_dict)
}

# FFMQ
scale.truth.rarely.5 <- c(
  "Very rarely true",
  "Rarely true",
  "Sometimes true",
  "Often true",
  "Very often true"
)

# Self-esteem Rosenberg
scale.agreement.strongly.4 <- c(
  "Strongly disagree",
  "Disagree",
  "Agree",
  "Strongly agree"
)

scale.agreement.strongly.5 <- c(
  "Strongly disagree",
  "Disagree",
  "Neither agree nor disagree",
  "Agree",
  "Strongly agree"
)

# SDT, virtualcompanion daily
scale.agreement.little.5 <- c(
  "Not at all",
  "A little",
  "Moderately",
  "A lot",
  "Extremely"
)

# I-PANAS-SF
scale.agreement.slightly.5 <- c(
  "Not at all",
  "Slightly",
  "Moderately",
  "Very",
  "Extremely"
)

# SLWS
scale.agreement.slightly.7 <- c(
  "Strongly disagree",
  "Disagree",
  "Slightly disagree",
  "Neither agree nor disagree",
  "Slightly agree",
  "Agree",
  "Strongly agree"
)

# UCLA Loneliness
scale.frequency.4 <- c(
  "Never",
  "Rarely",
  "Often",
  "Always"
)

# FFMQ
scale.frequency.almost.6 <- c(
  "Almost never",
  "Very infrequently",
  "Somewhat infrequently",
  "Somewhat frequently",
  "Very frequently",
  "Almost always"
)

# SISE
scale.esteem.semdiff.5 <- c(
  "1 Not very true of me",
  "2",
  "3",
  "4",
  "5 Very true of me"
)

# BIS/BAS
scale.truth.4 <- c(
  "Very false for me",
  "Somewhat false for me",
  "Somewhat true for me",
  "Very true for me"
)

scale.truth.4.muris <- c(
  "Not true",
  "Somewhat true",
  "True",
  "Very true"
)

# self-efficacy (Sherer1982)
scale.agreement.7 <- c(
  "Strongly disagree",
  "Disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Agree",
  "Strongly agree"
)

# self-control
scale.control.5 <- c(
  "1 Not at all like me",
  "2",
  "3",
  "4",
  "5 Very much like me"
)

scale.describe.semdiff.5 <- c(
  "1 Does not describe me at all",
  "2",
  "3",
  "4",
  "5 Describes me very well"
)


# Fordyce SISE
scale.happy.11 <- c(
  "0 Extremely unhappy (utterly depressed, completely down)",
  "1 Very unhappy (depressed, spirits very low)",
  "2 Pretty unhappy (somewhat \"blue\", spirits down)",
  "3 Mildly unhappy (just a bit low)",
  "4 Slightly unhappy (just a bit below neutral)",
  "5 Neutral (not particularly happy or unhappy)",
  "6 Slightly happy (just a bit above neutral)",
  "7 Mildly happy (feeling fairly good and somewhat cheerful)",
  "8 Pretty happy (spirits high, feeling good)",
  "9 Very happy (feeling really good, elated!)",
  "10 Extremely happy (feeling ecstatic, joyous, fantastic!)"
)
