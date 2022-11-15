################################################################################
#
# Survey Functions
#
################################################################################
######################################################################
# Function authenticate_googledrive()
######################################################################
authenticate_googledrive <- function(
    email = "chaehan.so@gmail.com", token = NULL) {
  require(googledrive)
  require(googlesheets4)
  googledrive::drive_auth(email)
  if (!is.null(token)) {
    gs4_auth(token = googledrive::drive_token())
  } else {
    googlesheets4::gs4_deauth()
  }
}

######################################################################
# Function get_survey_data()
#
# tidies all column names equivalent to:
# rename_with(~ tolower(.) %>% # removes capital letters
#   gsub("\\(|\\)", "",.) %>% # removes "(" and ")"
#   gsub(" ", "_", .)) # removes spaces by "_"
#
######################################################################
get_survey_data <- function(
  survey_key, col_types = NULL, remove_col = 0, remove_row = 0) {

  data.survey.main <- survey_key %>%
    read_sheet(col_types = col_types, skip = remove_row) %>%
    select(-remove_col)

  data.survey.main
}

######################################################################
# Function label_data_raw()
######################################################################
label_data_raw <- function(data_raw, descstats_dict, attention_dict) {

  attention.cols <- 1:ncol(attention_dict)

  data_raw %>%
    # attention items
    rename_with(
      .cols = attention.cols,
      .fn = ~ paste0("attention", attention.cols)
    ) %>%
    rename_with(.cols = descstats_dict$index, ~ descstats_dict$labels) %>%
    # extract the content between brackets - rename_with doesn't work (duplicate)
    set_names(gsub("\\[(.+)\\].+", "\\1", names(.)))
}

######################################################################
# function check_attention_items()
# attention check
# assumes: attention check item are first items in data
# assumes: attention check item names start with "attention"
######################################################################
check_attention_items <- function(data, attention_dict) {

  no.attention.items <- ncol(attention_dict)
  # add row index
  data.indexed <- data %>% rownames_to_column(var = "row")
  # extract attention items with row index
  attention.items <- data.indexed %>%
    # assumes: attention check item are first items in data
    select(1: (1+no.attention.items) )  %>%
    set_names(c("row", paste0("attention", 1:no.attention.items)))

  ## remove failed attention checks AND attention items
  # remove.rows <- which(aa$attention1 != "Agree" | aa$attention2 != "2")
  # data[-remove.rows, -c(1:2)]

  # VERY TRICKY: extract the attention checks that passed
  # https://stackoverflow.com/a/49381716/7769076
  passed <- attention_dict %>% inner_join(attention.items)

  data.indexed[data.indexed$row %in% passed$row,
               !grepl(c("row|attention"), names(data.indexed))]
}

######################################################################
# Function convert_numeric()
# IN:   userdata (dataframe) containing factors
# OUT:  output (dataframe) factors transformed into numerical
######################################################################
# convert dataframe with factors into dataframe with numeric values
convert_numeric <- function(userdata) {
  numeric_matrix <- lapply(userdata, function(x) if(is.factor(x)) { # lapply not sapply (returns 1 col)!!!
    as.numeric(as.character(x)) } else if (is.character(x))
    { as.numeric(x) }
    else {x})
  numeric_dataframe <- data.frame(numeric_matrix)
  return(numeric_dataframe)
}

######################################################################
# function get_rowwise_stats()
# IN:   data_survey (tibble)
# OUT:  output (tibble)
#
######################################################################
get_rowwise_stats <- function(
  data, id_label = "id", sort = "sd", sort_fct = "desc", print_max = 100) {

  data %>%
    rowwise(!!rlang::sym(id_label)) %>%
    dplyr::mutate(
      sd = sd(c_across(everything())),
      mean = mean(c_across(everything()))) %>%
    {
      if (sort_fct == "desc") {
        arrange(., desc(!!rlang::sym(sort)))
      } else {
        arrange(., !!rlang::sym(sort))
      }
    } %>%
    print(n = print_max)
}

######################################################################
# function plot_density()
# IN:   data (tibble)
# OUT:  output (tibble)
#
######################################################################
plot_density <- function(data, sort_fct = "mean") {

  data %>%
    get_rowwise_stats(sort = sort_fct) %>%
    pluck(sort_fct) %>%
    lattice::densityplot()
}

######################################################################
# function save_desc_stats()
# IN:   data (tibble)
# OUT:  output (tibble)
#
######################################################################
# retrieve & save descriptive statistics
save_desc_stats <- function(
  data_labeled, descstats_labels, data_desc_stats_label) {

  data_labeled %>%
    select(descstats_labels) %T>%
    saveRDS(data_desc_stats_label) %T>%
    print
}

######################################################################
# Function save_kable_table()
# saves dataframe as latex or html
######################################################################
save_kable_table <- function(data, file_name, format = "html", digits = 2) {

  format <- gsub(".+\\.(.+)", "\\1", file_name)

  data %>%
    knitr::kable(digits = digits,
                 format = ifelse(format == "tex", "latex", "html")) %>%
    cat(file = file_name)
}

######################################################################
# Function save_table()
# saves dataframe as csv, latex or html
######################################################################
save_table <- function(dataframe, file_name, digits = 2) {

  data <- dataframe %>%
    mutate(across(where(is.numeric), round, digits))

  format <- gsub(".+\\.(.+)", "\\1", file_name)

  switch(
    format,
    "csv" = write_excel_csv(
      data,
      file = file_name
    ),
    "tex" = save_kable_table(
      data,
      file_name = file_name,
      digits = digits
    ),
    "html" = save_kable_table(
      data,
      file_name = file_name,
      digits = digits
    )
  )
}

################################################################################
# save_sjPlot_corr_csv
# input: sjPlot::tab_corr object
################################################################################
save_sjPlot_corr_csv <- function(
  sjplot_tab_corr, file_label = "") {

  table.corr <- sjplot_tab_corr$page.content %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    pluck(1) %>%
    # remove last row
    head(., -1) %>% print

  if (file_label != "") {
    table.corr %>%
      save_table(paste0(file_label, ".csv"))
  }
}


get_scale_stats <- function(
  item_encoded_list, digits = 2, file_name = "") {

  scale.stats <- item_encoded_list %>%
    map(~ .x %>%
          # tricky: remove id column only if exists
          select(-matches("^id$")) %>%
          psych::alpha(.
                       # , check.keys = TRUE
          )
    ) %>%
    # tricky: in named list, .x is list element, .y is list element name
    imap_dfr(
      ~ .x$total %>%
        select(`Cronbach alpha` = raw_alpha, mean, sd) %>%
        set_rownames(.y) %>%
        round(digits = digits)
    ) %>%
    rownames_to_column("scale")

  if (file_name != "") {
    scale.stats %>% save_table(file_name = file_name, digits = digits)
  }

  scale.stats

}

######################################################################
# Function get_itemscale_stats()
# IN:   item_encoded_list (list from encode_survey_by_item_dict())
# OUT:  -- (tables saved as latex, html, or csv)
#
######################################################################
get_itemscale_stats <- function(
  item_encoded_list,
  save_tables_from = 0,
  digits = 2,
  format = "csv"
) {

  # get Cronbach alphas & item reliabilities
  list.itemscale <- item_encoded_list %>%

    map(~ .x %>%
          # tricky: remove id column only if exists
          select(-matches("^id$")) %>%
          sjPlot::tab_itemscale(show.kurtosis = TRUE) %>%
          .$df.list %>%
          .[[1]] %>% # tricky!
          dplyr::rename(`alpha if deleted` = `&alpha; if deleted`) %>%
          select(-Missings)
    )

  filename <- function(table_nr, scale_label, file_extension) {
    paste0(
      "tables/Table ", table_nr,
      " Item Analysis of Scale ", scale_label,
      ".", file_extension)
  }

  # save tables
  if (save_tables_from > 0) {
    pmap(list(
      list.itemscale, # ..1 item analysis table per scale
      (save_tables_from - 1) + (1:length(scale.result)), # ..2 table number
      names(scale.result) # ..3 scale name
    ),
    ~ save_table(..1, filename(..2, ..3, format), digits)
    )
  }

  list.itemscale
}

######################################################################
# Function determine_factor_extraction_no()
# IN:   items_df (dataframe)
# OUT:  output (text)
#
######################################################################
determine_factor_extraction_no <- function(items_df) {
  # set max.#cores by parallel::detectCores()
  options(mc.cores=4)
  # getOption("mc.cores")
  # Parallel analysis
  parallel <- fa.parallel(items_df)
  # Wayne Velicer's VSS+MAP criterion for number of factor extraction
  vss <- VSS(items_df)
  summary(vss)
}

######################################################################
# Function do_factor_analysis()
# IN:   items_input (dataframe), n_factors(integer), factor_method(string),
#       corr_type(string), cut_off(dbl): below factors loadings suppressed
# OUT:  output (dataframe)
######################################################################
do_factor_analysis <- function(
  items_input,
  n_factors = 3, factor_method = "fa", corr_type = "cor",
  sort = TRUE, cut_off = NULL, digits = 3, save = "") {
  require(dplyr)
  items <- na.omit(items_input)
  items <- data.matrix(items)
  # PCA from psych package - works with small sample size!
  if (factor_method=="pca")
  {
    corr.matrix <- cor(items,items)
    # psych::principal for Principal Components Analysis
    output <- principal(corr.matrix,rotate="oblimin", nfactors=n_factors)
  }
  else # psych:fa for Factor Analysis
  {
    output <- fa(
      items,
      rotate = "oblimin",
      use = "pairwise",
      fm = factor_method,
      nfactors = n_factors,
      cor = corr_type
    )
  }

  if (sort) output %<>% fa.sort()

  # https://www.anthonyschmidt.co/post/2020-09-27-efa-tables-in-r/
  add_info <- cbind(output$communality,
                    output$uniquenesses,
                    output$complexity) %>%
    # make it a data frame
    as.data.frame() %>%
    # column names
    dplyr::rename("communality" = V1,
                  "uniqueness" = V2,
                  "complexity" = V3) %>%
    #get the item names from the vector
    rownames_to_column("item")

  factor.loadings <- output$loadings %>%
    unclass() %>%
    as.data.frame() %>%
    rownames_to_column("item") %>%
    replace(.>-cutoff & .<cutoff, NA)

  output <- factor.loadings %>%
    left_join(add_info) %>%
    dplyr::mutate(across(where(is.numeric), round, digits))

  output <- factor.loadings %>%
    left_join(add_info) %>%
    dplyr::mutate(across(where(is.numeric), round, digits)) %>%
    # print (blank) for values within cutoff range
    dplyr::mutate(across(everything(), as.character)) %>%
    replace(is.na(.), "")

  if (save != "") {
    output %>% save_table(save)
  }

  output
}

######################################################################
# Function get_alpha()
# IN:   data (dataframe)
# OUT:  cronbach alpha & r.drop corrected by smc (list)
######################################################################
get_alpha <- function(data) {
  # cronbach alpha short form
  data <- data.matrix(data)
  # alpha function was overriden, must be called from package!
  alpha_list <- psych::alpha(data, na.rm=TRUE)
  cronbachAlpha <- alpha_list$total$std.alpha
  # extract alpha stats: r.drop
  trennsch.1 <- alpha_list$item.stats["r.drop"]
  trennsch.2 <- alpha_list$item.stats["r.cor"]
  output <- list(cronbachAlpha = cronbachAlpha, r.cor = trennsch.2)
  # Remember: assign to output variable!
  output <- lapply(output, round, digits=2)
  return(output)
}

################################################################################
#
# Scale Labels
# Todo: extend by new labels
#
################################################################################
scale.effective.labels <- c("Not effective at all","Slightly effective","Moderately effective","Highly effective","Extremely effective")
# scale.extent.labels <- c("Not at all","Slightly","Moderately","Highly","Extremely")
scale.extent.labels <- c("not at all","a little","moderately","a lot","extremely")
scale.agreement.labels <- c("Disagree strongly", "Disagree a little", "Neither disagree nor agree", "Agree a little", "Agree strongly")
scale.likeability.labels <- c("Not at all","Slightly","Moderately","Highly","Extremely")
scale.animationlike.labels <- c("Not at all","Not very much","Neutral","Very much","Extremely")
scale.lastwatched.labels <- c("More than 2 years ago",
                              "Between 1 to 2 years",
                              "Between 6 months to 1 year",
                              "In the last 6 months")
scale.numberwatched.labels <- c("0","1 to 2","3 to 4","5 to 6","More than 6")

quartzFonts(gillsans = c("Gill Sans Light", "Gill Sans Light", "Gill Sans Italic", "Gill Sans Bold Italic"))

################################################################################


# create mean scores of a Latent Variable
get_mean_score <- function(data) {
  return(rowMeans(data.matrix(data), na.rm = TRUE))
}


######################################################################
# Function remove_duplicates()
# IN:   file (google sheet)
# OUT:  non_duplicates (dataframe)
######################################################################
remove_duplicates <- function(file)
{
  require(dplyr)
  # remove timestamp (first column) & duplicates
  non_duplicates <- file[-1] %>% file[!duplicated(file),]
  return(non_duplicates)
}

######################################################################
# Function import_file_google()
# IN:   file_name (google sheet)
# OUT:  file (dataframe)
######################################################################
import_file_google <- function(file_name)
{
  require(dplyr)
  # gs_file.dd2() %>% gs_copy(to = filename.dd2)
  file.identifier <- gs_title(file_name)
  ws.identifier <- gs_ws_ls(file.identifier)
  # file.dd2 <- file.identifier %>% gs_read(ws = ws.identifier)
  file <- file.identifier %>% gs_read_csv(ws = ws.identifier)
  # remove duplicates
  output <- remove_duplicates(file)
  return(output)
}

today <- paste(format(Sys.time(), "%Y-%m-%d"))

######################################################################
# Function create_email_list()
# IN:   email_df (dataframe)
# OUT:  result (dataframe)
######################################################################
create_email_list <- function(email_df)
{
  result <- NULL
  for (item in email_df)
  {
    # print(as.character(item))
    result <-paste0(result, item, sep="; ")
  }
  result <- gsub("(.*)(, )", "\\1", result)
  return(result)
}

######################################################################
# Function filename.n
# IN:   filename (string), sample_size (numeric)
# OUT:  filename in data dir with sample size and file extension (string)
######################################################################
filename.n <- function(filename, sample_size) {

  caller <- sys.call(-1)
  caller %>% print
  caller %<>% as.character %>% .[1] %>% print

  if (str_detect(caller, "saveRDS") | str_detect(caller, "readRDS")) {
    file.extension <- ".rds"
    # } else if (caller == "write_csv" | caller == "write.csv") {
  } else if (caller == "standardise_path") {
    file.extension <- ".csv"
  }
  paste0("data/", filename, ".n=", sample_size, file.extension) %T>% print
}

######################################################################
# Function create_survey_data()
# IN:   survey_key (char)
#       input_dir (char)
#       survey_name (char)
# OUT:  survey.raw (dataframe)
######################################################################
get_google_survey_data <- function(survey_key, input_dir, survey_name,
                               start_date = NULL, end_date = NULL,
                               descriptive_columns = NULL)
{
  require(googlesheets)
  # retrieve google spreadsheet data
  ## Prerequisites: 1. File | Publish to the web + 2. Share | Get link
  survey.raw <- survey_key %>% gs_key(lookup = FALSE) %>% gs_read()

  # preprocess survey data
  survey.raw %<>%
    # Remove timestamp
    select(-Timestamp) %>%
    # remove duplicates
    distinct()

  return(survey.raw)
}

######################################################################
# Function remove_descriptive_columns()
# IN:   descriptive_columns_matrix (matrix)
#       survey_raw (dataframe)
# OUT:  out (dataframe)
######################################################################
remove_descriptive_columns <- function(descriptive_columns_matrix, survey_raw)
{
  # set label and name for descriptive.columns
  descriptive.columns <- descriptive_columns_matrix %>%
    as.data.frame %>%
    setNames(c("label","name"))

  # extract descriptive columns (by label) from survey data
  survey.descriptive <- survey_raw[, names(survey_raw) %in% descriptive.columns$label]

  # set item names (by name) for descriptive survey
  survey.descriptive %<>% setNames(descriptive.columns$name)

  # create output object otherwise
  output <- NULL

  output$descriptive <- survey.descriptive

  survey.data  <- survey_raw[, -which(names(survey_raw) %in% descriptive.columns$label)]

  output$survey <- survey.data

  return(output)
}

name_by_item_dict <- function(data, lv_item_dict) {

  names(data) <- lv_item_dict %>%
    map(pluck("item.labels")) %>%
    unlist %>%
    as.vector()

  return(data)
}

######################################################################
# Function encode_scale_labels()
# replace scale responses containing label names with their scale codes
#
# IN:   scale_data_frame (dataframe), scale_labels(vector(char))
# OUT:  encoded_df (dataframe)
#
######################################################################
encode_scale_labels <- function(
  scale_data_frame, scale_labels, reverse_index = NULL) {

  matrix <- as.matrix(scale_data_frame)
  scale.codes <- 1:length(scale_labels)
  matrix[matrix == ""] <- NA

  for (index in scale.codes) {
    matrix[matrix == scale_labels[index]] <- scale.codes[index]
  }

  encoded_df <- matrix %>%
    as.data.frame() %>%
    map_df(as.numeric)

  # reverse-coding
  if (!is.null(reverse_index)) {
    encoded_df[reverse_index] %<>% map(~ max(scale.codes) + 1 - .x)
  }

  return(encoded_df)
}

encode_by_item_dict <- function(data, lv_item_dict, add_id = TRUE) {

  lv_item_dict %>%
    map( ~ .x %>%
           pluck("item.labels") %>%
           # put all the items into a list of scales
           data[.] %>%
           # tricky: use .x containing encoding defined in item.dict
           encode_scale_labels(
             scale_labels = .x %>% pluck("encoding"),
             reverse_index = .x %>% pluck("reverse.index")
           )
    ) %>%
    set_names(lv_item_dict %>% map_chr(~ .x %>% pluck("scale.label"))) %>%
    {
      if (add_id) {
        # add participant id
        map(., ~ .x %>%
              rownames_to_column(var = "id") %>%
              dplyr::mutate(across(id, as.numeric))
        )
      } else {
        .
      }
    }
}

encode_survey_by_item_dict <- function(survey_data, lv_item_dict) {

  survey_data %>%
    name_by_item_dict(lv_item_dict) %>%
    encode_by_item_dict(lv_item_dict)
}



######################################################################
# DEPRECATED
# Function encode_survey_and_scales()
# IN:   survey_data (dataframe)
#       LV_labels (list)
#       LV_scale_list (list)
# OUT:  out (dataframe)
######################################################################
encode_survey_and_scales <- function(survey_data, LV_labels, LV_scale_list) {
  ## Create list of item names for each LV
  # 1. LV.item.list: assign item names to latent variable labels
  LV.item.list <- lapply(LV_labels, function(name) assign(name, eval(parse(text = name))))
  names(LV.item.list) <- LV_labels

  # 2. survey.data: set column names from LV.item.list
  colnames(survey_data) <- LV.item.list %>% unlist %>% as.vector()

  # 3. LV.data.list: extract survey.data columns by item names
  LV.data.list <- lapply(LV.item.list, function(items) survey_data[items] )

  # 4. Encoding: apply scale for each LV in LV.data.list
  survey.factor.list <- mapply(encode_scale_labels, LV.data.list, LV.scale.list)

  survey.numeric.list <- lapply(survey.factor.list, convert_numeric)

  survey.numeric <- survey.numeric.list %>%
    # remove the scale name from item name
    setNames(NULL) %>%  as.data.frame

  output <- NULL
  output$survey <- survey.numeric

  # create scales
  scales <- lapply(survey.numeric.list, rowMeans) %>% as.data.frame()

  output$scales <- scales

  # scales <- scales %>% dplyr::select(-Attention1, -Attention2)

  return(output)
}

######################################################################
# DEPRECATED
# Function remove_failed_attention_checks()
# IN:   attention_items_matrix (matrix)
#       survey_numeric (dataframe)
#       survey_descriptive (dataframe)
# OUT:  survey_cleaned (dataframe)
#
######################################################################
remove_failed_attention_checks <- function(attention_items_matrix,
                                           survey_numeric,
                                           survey_descriptive,
                                           scales_raw) {
  # set label and name for descriptive.columns
  attention_items <- attention_items_matrix %>%
    as.data.frame %>%
    setNames(c("item","target"))

  # as numbers are interpreted as factors, convert to char! then num!!
  attention_items$target %<>% as.character %>% as.numeric

  # remove attention item columns, e.g. dplyr::select(-attention1)
  columns_to_remove <-  attention_items[,"item"] %>% as.vector
  survey_pruned_items <- survey_numeric %>% dplyr::select(-columns_to_remove)
  message(paste("removed columns", paste(columns_to_remove, collapse = " ")))

  # get false answers for all attention_items
  wrong_rows_list <- apply(attention_items, 1, function(row) {
    which(survey.numeric[as.character(row["item"])] != row["target"]) %>% as.list
  })

  # find the wrong answers' common set among all attention items
  wrong_rows <- Reduce(intersect, wrong_rows_list) %>% unlist

  # remove wrong answers
  survey_numeric_cleaned      <- survey_pruned_items[-wrong_rows,]
  survey_descriptive_cleaned  <- survey_descriptive[-wrong_rows,]
  scales_raw_cleaned          <- scales_raw[-wrong_rows,]

  message(paste("removed", length(wrong_rows), "rows of failed attention checks!"))

  output <- list(survey_numeric_cleaned,
                 survey_descriptive_cleaned,
                 scales_raw_cleaned)
  return(output)
}

######################################################################
# Function mediate_each()
# IN:   vars (vector)
#       data (dataframe)
#       no_simulations (numerical)
#       IDE (boolean)
# OUT:  best.mediators.ACME (default)
#       best.mediators.IDE (IDE=TRUE)
#
######################################################################
mediate_each <- function(vars, data, no_simulations=10, IDE = FALSE) {
  require(dplyr)
  require(mediation)
  predictor <- vars["predictor"]
  mediator  <- vars["mediator"]
  outcome   <- vars["outcome"]

  model.M <- lm(get(mediator) ~ get(predictor), data, na.action = na.omit)

  results.M <- model.M %>% summary %>%
    # 2nd row: get(predictor), 1st col: Estimate, 4th col: p-value
    .$coefficients %>% .[2,c(1,4)] %>%
    as.list %>% as.data.frame %>%
    # rename columns
    dplyr::rename(effect_size_mx = Estimate, p_value_mx = "Pr...t..") %>%
    # add predictor name
    dplyr::mutate(predictor_mx=predictor, mediator_mx=mediator)

  model.Y <- lm(get(outcome) ~ get(predictor) + get(mediator), data)
  # model.Y %>% summary
  results.Y <- model.Y %>% summary %>%
    .$coefficients %>% .[2,c(1,4)] %>%
    as.list %>% data.frame %>%
    # rename columns
    dplyr::rename(effect_size_ym = Estimate, p_value_ym = "Pr...t..") %>%
    # add predictor name
    dplyr::mutate(outcome = outcome,
                  mediator = mediator,
                  predictor = predictor)

  if (IDE)
  {
    result <- cbind(results.Y, results.M)

  } else { # IDE==FALSE

    model.mediation <- mediate(model.m = model.M, model.y = model.Y,
                               treat="get(predictor)", mediator = "get(mediator)",
                               boot = FALSE,
                               sims = no_simulations)

    best.mediators.ACME <- summary(model.mediation) %>%
      .[c("d0", "d0.p", "z0", "z0.p", "n0", "n0.p")] %>%
      lapply(., function(x) round(x, digits = 2)) %>%
      as.data.frame() %>%
      dplyr::mutate(predictor = predictor,
                    mediator = mediator,
                    outcome = outcome)

    result <- best.mediators.ACME

  }
  print(result)
  return(result)
}

######################################################################
# Function eval_mediations_detailed()
# IN:   mediation_vars (vector)
#       data (dataframe)
#       p_cutoff (numerical)
# OUT:  best.models.detailed
#
# SAVE: best.models.IDE.rds
#
######################################################################
eval_mediations_detailed <- function(mediation_vars, data, p_cutoff) {
  # estimate all mediation models
  mediation.models <- mediation_vars %>%
    apply(., 1, mediate_each, data, IDE = TRUE) %>%
    # convert list with same columns into dataframe
    do.call(rbind.data.frame, .) %T>% print

  best.models <- mediation.models %>%
    arrange(desc(effect_size_ym)) %>%
    # extract only models with significant mediator relationships (y-m and m-x)
    filter(abs(p_value_ym) <= p_cutoff & abs(p_value_mx) <= p_cutoff) %>%
    # round only numeric columns
    lapply(., function(x) if (is.numeric(x)) round(x, 2) else x) %>%
    as.data.frame

  best.models.IDE <- best.models %>%
    # estimate indirect effect b*a
    dplyr::mutate(IDE = effect_size_ym * effect_size_mx,
           p_ave = (p_value_ym + p_value_mx)/2) %>%
    # dplyr::select(ACME, p_ave, outcome, mediator, predictor) %>%
    dplyr::select(IDE, predictor, mediator, outcome, effect_size_mx, effect_size_ym) %>%
    dplyr::filter(!(predictor=="CSE_SSCS" & mediator=="CSE_Tierney")) %>%
    arrange(desc(IDE)) %>%
    do(rbind(head(.,10), tail(.,10)))

  setwd(outputTimex)
  saveRDS(best.models.IDE, "best.models.IDE.rds")
  result <- best.models.detailed

  return(result)
}

######################################################################
# Function eval_mediations_ACME()
# IN:   mediation_vars (vector)
#       data (dataframe)
#       no_simulations (numerical)
#       p_cutoff (numerical)
#       output_dir (char)
# OUT:  best.mediators.ACME
#
# SAVE: best.mediators.ACME.n=<no_simulations>.rds
#
######################################################################
eval_mediations_ACME <- function(mediation_vars, data, no_simulations,
                                 p_cutoff, output_dir) {
  # enable parallel processing with cluster
  require(parallel)
  # Calculate the number of cores
  no_cores <- detectCores() - 1
  # Initiate cluster
  cluster <- makeCluster(no_cores, type = "FORK")

  best.mediators.ACME <-  mediation_vars %>%
    parApply(cluster, ., 1, mediate_each, # replaces: apply(., 1, mediate_each,
          data = data, IDE=FALSE, no_simulations) %>%
    do.call(rbind.data.frame, .) %>%
    filter(d0.p < p_cutoff) %>%
    arrange(desc(d0),desc(n0), z0) %T>% print

  stopCluster(cluster)

  setwd(output_dir)
  filename <- paste0("best.mediators.ACME.n=", dim(data)[1],
                     ".sim=", no_simulations,".rds")
  saveRDS(best.mediators.ACME, filename)

  return(best.mediators.ACME)
}
