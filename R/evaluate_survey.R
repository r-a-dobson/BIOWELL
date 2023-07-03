#' Evaluate BIO-WELL survey
#'
#'Function to calculate key survey evaluation statistics on BIO-WELL survey
#'responses.
#'
#'@param data a data frame containing the combined response data frames from a
#'  `BIOWELL` package survey. See `download_data()` to obtain this.
#'@details
#'Once you have combined all responses to your BIO-WELL survey into a single
#'data frame using the `download_data()` function, this data frame can be input
#'into `evaluate_survey()`.
#'
#'`evaluate_survey()` returns six outputs in a list object:
#'
#'# Stem question statistics:
#'
#'The first three components are derived from the BIO-WELL score for each stem
#'question, calculated by averaging slider responses across the five well-being
#'domains.
#'
#'1) A processed data frame containing the average BIO-WELL scores for each
#'participant for each stem question within your custom `BIOWELL` package survey.
#'
#'2) Cronbach's Alpha: the computed Cronbach's Alpha statistic measures the
#'reliability, or internal consistency, of the five wellbeing sliders . This can
#'be calculated for each biodiversity statement and will help understand the
#'strength or consistency with which the sliders collectively measure a
#'biopsychosocial-spiritual model of wellbeing. The resulting alpha statistic
#'can be compared to those obtained in the original Irvine et al. (2023)
#'article. This is calculated using `cronbach.alpha()` function from the `ltm` R
#'package (Rizopoulos et al., 2007).
#'
#'3) Item-total correlation and Cronbach's Alpha-without: the computed
#'item-total statistic for each BIO-WELL question  within your survey (Cronbach
#'et al., 1951). This assesses how strongly scores on one wellbeing slider are
#'related to scores on all the other wellbeing sliders. It also contributes to
#'understanding the reliability of the wellbeing sliders as a measure of a
#'biopsychosocial-spiritual model of wellbeing. Alpha without is the Cronbach
#'Alpha reliability estimate of the scale without the variable. This is
#'calculated using the `item.total()` function from the `multilevel` [R
#'package](https://CRAN.R-project.org/package=multilevel).
#'
#'# Wellbeing domain x question:
#'
#'The latter three components are derived from the slider responses for each of the
#'five wellbeing domains within each stem question (total stem questions x five).
#'
#'4) A processed data frame containing the score for each wellbeing domain slider
#'within each stem question given by each participant in your custom `BIOWELL`
#'package survey dataset.
#'
#'5) Cronbach's alpha (see 2 for description)
#'
#'6) Item-total correlation and Cronbach's Alpha-without (see 3 for description)
#'
#'@returns Returns a list containing a data frame of processed BIO-WELL scores,
#'  Cronbach's alpha statistic and item-total.
#'@export
#'@references
#'Cronbach, L. J. (1951) Coefficient alpha and the internal structure of tests.
#'Psychometrika, 16, 297–334.
#'
#'Rizopoulos, D., 2007. ltm: An R package for latent variable modeling and item
#'response analysis. Journal of statistical software, 17, pp.1-25.
#'@examples
#'# Read in sample BIO-WELL survey data
#'data("sample_BW_data")
#'
#'results<-evaluate_survey(sample_BW_data)
#'
#'# Data frame containing average BIO-WELL scores for each participant x
#'# stem question
#'
#'head(results[[1]])
#'
#'# Statistics calculated using the average BIO-WELL score for each stem question:
#'# Cronbach's alpha
#'results[[2]]
#'
#'# Item total and Cronbach's alpha without
#'results[[3]]
#'
#'# Data frame containing BIO-WELL score for each participant x
#'# wellbeing domain question
#'
#'head(results[[4]])
#'
#'# Statistics calculated using the BIO-WELL score for each wellbeing domain:
#'
#'#'# Cronbach's alpha
#'results[[5]]
#'
#'# Item total and Cronbach's alpha without
#'results[[6]]
#'
#'

evaluate_survey <- function(data) {

  if (!inherits(data, "data.frame")) {
    stop("Please provide data as object of class data.frame")
  }

  if (length(grep("Social_INVERTED", colnames(data))) == 0) {
    stop(
      "Columns missing: were these data collected using the BIOWELL package?"
    )
  }

  if (nrow(data) == 1) {
    stop("Only one response in data. Cannot compute survey statistics.")
  }

  # Extract BIO-WELL scores for each wellbeing domain
  df1 <- data[, grep("Social_INVERTED", colnames(data))]
  df2 <- data[, grep("Spiritual_INVERTED", colnames(data))]
  df3 <- data[, grep("Physical_INVERTED", colnames(data))]
  df4 <- data[, grep("Emotional_INVERTED", colnames(data))]
  df5 <- data[, grep("Cognitive_INVERTED", colnames(data))]

  # Take the average BIO-WELL score across each domain for each question
  average_biowell <- (df1 + df2 + df3 + df4 + df5) / 5

  # Change column names
  column_names <- gsub("Social_INVERTED", "", colnames(average_biowell))

  df <- do.call(rbind.data.frame, strsplit(column_names, "_"))

  colnames(df) <- c("Setting", "Question")

  # Extract average BIO-WELL scores for each question for each participant.
  raw <- as.data.frame(t(average_biowell))

  # Remove rownames
  rownames(raw) <- NULL

  colnames(raw) <- paste0("participant_", 1:ncol(raw))

  stem_df <- cbind(df, raw)

  # Calculate Cronbach's Alpha using ltm package

  stem_only <- t(raw)

  stem_cronback_alpha <- ltm::cronbach.alpha(stem_only)

  stem_item_total <- multilevel::item.total(stem_only)



  # Calculate item total using ltm package
  all_sliders <- data[,grepl("INVERTED",colnames(data))]
  all_sliders <- all_sliders[,1:(ncol(all_sliders)-1)]


  # Change column names
  column_names <- gsub("_INVERTED", "", colnames(all_sliders))

  df <- do.call(rbind.data.frame, strsplit(column_names, "_"))

  colnames(df) <- c("Setting", "Question", "Wellbeing domain")

  raw <- as.data.frame(t(all_sliders))

  # Remove rownames
  rownames(raw) <- NULL

  colnames(raw) <- paste0("participant_", 1:ncol(raw))


  sliders_df <- cbind(df, raw)


  sliders_cronback_alpha <- ltm::cronbach.alpha(all_sliders)

  sliders_item_total <- multilevel::item.total(all_sliders)



  # Create list containing data frame and statistics
  results_list <- list(stem_data_frame = stem_df ,
                       stem_cronback_alpha = stem_cronback_alpha,
                       stem_item_total = stem_item_total,
                       slider_data_frame = sliders_df,
                       sliders_cronback_alpha = sliders_cronback_alpha,
                       sliders_item_total = sliders_item_total)

  # Return list to user
  return(results_list)

}



