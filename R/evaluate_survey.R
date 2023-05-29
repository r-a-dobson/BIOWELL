#' Evaluate BIO-WELL survey
#'
#'Function to calculate key survey evaluation statistics on BIO-WELL survey
#'responses.
#'
#'@param data a data frame containing the combined response data frames from a
#'  `BIOWELL` package survey. See `download_data()` to obtain this.
#'@details
#'
#'Once you have combined all responses to your BIO-WELL survey (created using
#'the `build_survey()` function) into a single data frame using the
#'`download_data()` function, this data frame can be input into
#'`evaluate_survey()`.
#'
#'`evaluate_survey()` returns three outputs in a list object:
#'
#'1) A processed data frame containing the average BIO-WELL scores for each
#'participant for each questions within your custom `BIOWELL` package survey.
#'
#'2) Cronbach's Alpha: the computed Cronbach's Alpha statistic for your survey,
#'which measures the internal consistency of responses. This calculated using
#'`cronbach.alpha()` function from the `ltm` R package (Rizopoulos et al.,
#'2007).
#'
#'3) Item-total correlation: the computed item-total statistic for each BIO-WELL
#'question within your survey (Cronbach et al., 1951). This is calculated using
#'the `item.total()` function from the `multilevel` [R
#'package](https://CRAN.R-project.org/package=multilevel).
#'
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
#'question
#'
#'head(results[[1]])
#'
#'# Cronbach's alpha
#'results[[2]]
#'
#'# Item total
#'results[[3]]

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

  colnames(df) <- c("Situation", "Question")

  # Extract average BIO-WELL scores for each question for each participant.
  raw <- as.data.frame(t(average_biowell))

  # Remove rownames
  rownames(raw) <- NULL

  colnames(raw) <- paste0("participant_", 1:ncol(raw))

  df <- cbind(df, raw)

  # Calculate Cronbach's Alpha using ltm package

  cronback_alpha <- ltm::cronbach.alpha(t(raw))

  # Calculate item total using ltm package
  item_total <- multilevel::item.total(t(raw))

  # Create list containing data frame and statistics
  results_list <- list(data_frame = df ,
                       cronback_alpha = cronback_alpha,
                       item_total = item_total)

  # Return list to user
  return(results_list)

}



