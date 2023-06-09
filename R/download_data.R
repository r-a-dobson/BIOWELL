#'Combine BIO-WELL survey responses into a data frame
#'
#'Function downloads survey responses from a Dropbox App folder and combines
#'them into a single data frame.
#'
#'@param Dropbox_App_folder path to the folder in your Dropbox App contain
#'  survey responses.
#'@param BW_app_path a character string, the path to your BIO-WELL Shiny App
#'  folder.
#'@details
#'Each set of participant responses to your BIO-WELL survey is stored as a
#'single "csv" file within your Dropbox App folder.
#'
#'`download_data()` downloads all participant responses and combines them into a
#'single data frame. Each row represents responses from a participant.
#'
#'# BIO-WELL score columns
#'
#'For each BIO-WELL question, the scores for each of the five scales are given
#'(Physical, Emotional, Cognitive, Social and Spiritual) in RAW and INVERTED
#'format.
#'
#' + The "_RAW" value is the actual value between 0 and 100 that users specify with
#'the sliders, whereby 0 is a positive wellbeing response to biodiversity and
#'100 is a negative response.
#'
#'+ The "_INVERTED" value is calculated as 100 minus the RAW value. This is to
#'improve interpretability  of the BIO-WELL scores: 0 is a negative wellbeing
#'response and 100 is a positive wellbeing response.
#'
#'The final columns are overall inverted and raw BIOWELL scores. This is the
#'average across all five scales across all BIO-WELL questions.
#'
#'Please see Irvine et al., (2023) for more details on the BIO-WELL score.
#'
#'#'# Extra variables recorded
#'
#'In addition to the participants responses to survey questions, the downloaded
#'data will contain columns for the following variables:
#'
#'+ `survey_duration` - the time in seconds between the participants starting
#'the survey and submitting their responses.
#'+ `start_date` - the date that participants began the survey in YYYY-MM-DD
#'format.
#'+ `start_time` - the time that participants began the survey in HH-MM-SS
#'format.
#'+ `end_date` - the date that participants submitted their responses to the
#'survey in YYYY-MM-DD format.
#'+ `end_time`  - the time that participants submitted their responses to
#'the survey in HH-MM-SS format.
#'+ `timezone` - the time zone of participants.
#'+ `timezone_location` - the broad location of the participant's time zone.
#'+ `server_submit_time` - the time in the Shiny Server's time zone that
#'participant responses were submitted.
#'
#'@references Irvine, K.N., Fisher, J.C., Bentley, P.R., Nawrath, M., Dallimer,
#'M., Austen, G.E., Fish, R. and Davies, Z.G., 2023. BIO-WELL: The development
#'and validation of a human wellbeing scale that measures responses to
#'biodiversity. Journal of Environmental Psychology, 85, p.101921. doi:10.1016/j.jenvp.2022.101921.
#'
#'@returns Data frame containing combined responses to BIO-WELL survey
#'@export
#'@examplesIf file.exists(".secrets/dropbox_token.rds")
#'
#'  data <- download_data(Dropbox_App_folder = "survey_responses",
#'                        BW_app_path = paste0(tempdir(), "/my_app_name_1"))
#'
#'# See below a sample of downloaded survey participant data
#'
#'data(sample_BW_data)
#'
#'head(sample_BW_data)
#'

download_data <- function(Dropbox_App_folder,
                          BW_app_path) {


  if(!dir.exists(BW_app_path)){stop("Cannot find BIO-WELL app folder.")}

  files <- list.files(paste0(BW_app_path,"/.secrets"))

  # Check for Dropbox token in .secrets folder

  if (!"dropbox_token.rds" %in% files) {
    stop("No dropbox_token.rds in BIO-WELL .secrets. See `activate_dropbox()`.")
  }

  # Get Dropbox access token
  token <- readRDS(paste0(BW_app_path, "/.secrets/dropbox_token.rds"))

  # Refresh Dropbox access token
  rdrop2::drop_dir(dtoken = token)

  # Read all the files in Dropbox App folder into list
  filesInfo <- rdrop2::drop_dir(Dropbox_App_folder, dtoken = token)

  filePaths <- filesInfo$path_display

  # Remove the running average file from the list
  filePaths <- filePaths[!grepl("BIOWELL_RUNNING_AVERAGE.csv" , filePaths)]

  # Read in all survey response data frames
  data <- lapply(filePaths,
           rdrop2::drop_read_csv,
           dtoken = token,
           stringsAsFactors = FALSE)

  # Combine all responses into one data frame

  message1 <- "Have you changed the no. of questions/answers between responses?"
  message2 <- "Are there any erroneous files in your Dropbox App folder?"

  tryCatch({
    data <- do.call(rbind, data)
  },
  error = function (e) {
    message(paste0(e, "\n", message1,"\n", message2))
  })

  return(data)
}

