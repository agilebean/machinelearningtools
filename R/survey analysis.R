################################################################################
#
# Survey Functions
#
################################################################################

######################################################################
# Function convert_numeric()
# IN:   userdata (dataframe) containing factors
# OUT:  output (dataframe) factors transformed into numerical
######################################################################
# convert dataframe with factors into dataframe with numeric values
convert_numeric <- function(userdata)
{
  numeric_matrix <- lapply(userdata, function(x) if(is.factor(x)) { # lapply not sapply (returns 1 col)!!!
    as.numeric(as.character(x)) } else if (is.character(x))
    { as.numeric(x) }
    else {x})
  numeric_dataframe <- data.frame(numeric_matrix)
  return(numeric_dataframe)
}

######################################################################
# Function determine_factor_extraction_no()
# IN:   items_df (dataframe)
# OUT:  output (text)
#
######################################################################
determine_factor_extraction_no <- function(items_df)
{
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
do_factor_analysis <- function(items_input, n_factors = 3, factor_method = "fa",
                               corr_type = "cor", cut_off = NULL)
{
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
    output <- fa(items, rotate="oblimin", use="pairwise",
                 fm=factor_method, nfactors=n_factors, cor=corr_type)
  }
  # print (blank) for values within cutoff range

  cutoff <- ifelse(cut_off, cut_off, 0)
  output$weights %<>% # updates input after feeding into pipe
    replace(., .>-cutoff & .<cutoff, NA) %>%
    round(., digits = 2) %>%
    print(., na.print="")

  return(output)
}


######################################################################
# Function get_alpha()
# IN:   data (dataframe)
# OUT:  cronbach alpha & r.drop corrected by smc (list)
######################################################################
get_alpha <- function(data)
{
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


######################################################################
# Function encode_scale_labels()
# replace scale responses containing label names with their scale codes
#
# IN:   scale_data_frame (dataframe), scale_labels(vector(char))
# OUT:  encoded_df (dataframe)
#
######################################################################
encode_scale_labels <- function (scale_data_frame, scale_labels)
{
  matrix <- as.matrix(scale_data_frame)
  scale.codes <- 1:length(scale_labels)
  matrix[matrix == ""] <- NA

  for (index in scale.codes)
  {
    matrix[matrix == scale_labels[index]] <- scale.codes[index]
  }

  # retain the colum names in list
  encoded_df <- as.data.frame(matrix)
  return(encoded_df)
}

# create mean scores of a Latent Variable
get_mean_score <- function(data)
{
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

######################################################################
# Function encode_survey_and_scales()
# IN:   survey_data (dataframe)
#       LV_labels (list)
#       LV_scale_list (list)
# OUT:  out (dataframe)
######################################################################
encode_survey_and_scales <- function(survey_data, LV_labels, LV_scale_list)
{
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
                                           scales_raw)
{
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
mediate_each <- function(vars, data, no_simulations=10, IDE = FALSE)
{
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
    rename(effect_size_mx = Estimate, p_value_mx = "Pr...t..") %>%
    # add predictor name
    mutate(predictor_mx=predictor, mediator_mx=mediator)

  model.Y <- lm(get(outcome) ~ get(predictor) + get(mediator), data)
  # model.Y %>% summary
  results.Y <- model.Y %>% summary %>%
    .$coefficients %>% .[2,c(1,4)] %>%
    as.list %>% data.frame %>%
    # rename columns
    rename(effect_size_ym = Estimate, p_value_ym = "Pr...t..") %>%
    # add predictor name
    mutate(outcome=outcome, mediator=mediator, predictor=predictor)

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
      mutate(predictor=predictor, mediator=mediator, outcome=outcome)

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
eval_mediations_detailed <- function(mediation_vars, data, p_cutoff)
{
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
    mutate(IDE = effect_size_ym * effect_size_mx,
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
                                 p_cutoff, output_dir)
{
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
