#'Create BIO-WELL survey in Shiny
#'
#'Function to generate custom BIO-WELL survey with R Shiny
#'
#'@param survey_title a character string, the title of the survey to display on
#'  each page.
#'@param sidepanel_message a character string, the text to display on the side
#'  panel throughout the survey.
#'@param organisation a character string, the name of the organisation running
#'  the survey.
#'@param organisation_website a character string, the web address for the
#'  organisation running the survey.
#'@param screen_message a character string, the text to display above the
#'  screening questions.
#'@param screen_questions a character string or vector, the text for each
#'  screening question.
#'@param screen_questions_ID a character string or vector equal to the length of
#'  `screen_questions`, providing short IDs for each screening question to use
#'  in output survey response data frame.
#'@param screen_response_options a list the length of `screen_questions` with
#'  each element containing a vector of choices for responding to each screening
#'  question in-turn. To specify which answers to screen by add ":SCREEN" to the
#'  option. See details for an example.
#'@param start_message a character string, the text to display above the "start"
#'  questions.
#'@param start_questions a character string or vector, the "start" questions to
#'  ask participants prior to the BIO-WELL questions.
#'@param start_questions_ID a character string or vector equal to the length of
#'  `start_questions`, providing short IDs for each "start" question to use in
#'  output survey response data frame.
#'@param start_questions_type a character string or vector, the type
#'  of response item for each "start" question. Length must be equal to the
#'  length of `start_questions`. One of; `text`, `checkbox`, `selectbox`,
#'  `likert_five` and `likert_seven`. See details for more information.
#'@param start_response_options a list of character strings or
#'  vectors, the options to offer for each `start_questions` of type `checkbox`
#'  or `selectbox`. Provide these in the same order as they appear in
#'  `start_questions`.
#'@param biowell_situations a character string or vector, describing the
#'  environmental space setting(s) for BIO-WELL questions.
#'@param biowell_situations_ID a character string or vector the length of
#'  `biowell_situations`, the short IDs for each BIO-WELL situation to use in
#'  output survey response data frame.
#'@param biowell_questions a list of character strings or vectors, the questions
#'  for each separate `biowell_situations`. Each will be accompanied by the
#'  BIO-WELL response item comprising five sliders.
#'@param biowell_questions_ID a character string or vector equal to the length
#'  of `biowell_questions`, the short IDs for each BIO-WELL question to use in
#'  output survey response data frame.
#'@param end_message a character string, the text to display above the "end"
#'  questions.
#'@param end_questions a character string or vector, the questions to
#'  ask participants following the BIO-WELL questions.
#'@param end_questions_type a character string or vector, the type of
#'  response item for each "end" question. Length must be equal to the length of
#'  `start_questions`. One of; `text`, `checkbox`, `selectbox`, `likert_five`
#'  and `likert_seven`. See details for more information.
#'@param end_questions_ID a character string or vector equal to the length of
#'  `end_questions`, providing short IDs for each "end" question to use in
#'  output survey response data frame.
#'@param end_response_options  a list of character strings or vectors,
#'  the options to offer for each `end_questions` of type `checkbox` or
#'  `selectbox`. Provide these in the same order as they appear in
#'  `end_questions`.
#'@param all_questions a logical, indicating whether all "start" and "end"
#'  questions must be answered to continue with the survey.
#'@param all_sliders a logical, indicating whether all BIO-WELL sliders must be
#'  moved at least once to continue with the survey.
#'@param language a character string, the language for the BIO-WELL sliders. See
#'  details for all 31 language options available. Default; `english`.
#'@param user_report a logical, indicating whether to generate a BIO-WELL score
#'  report for participants. See details for more information.
#'@param offline_mode Optional; a logical, indicating whether to upload survey
#'  response data to Dropbox.
#'@param return optional; whether to run the survey Shiny app or return app
#'  component. One of; `run`, `ui`, `server`. Default: `run`.
#'@param Dropbox_App_folder a character string, the path to the folder in your
#'  Dropbox App to save survey responses to.
#'@param BW_app_path a character string, the path to your BIO-WELL survey Shiny
#'  App folder. See details for more information.
#'@details
#'
#'# BIO-WELL survey structure
#'
#' + Screening: This section allows you to set questions where certain responses
#' will prevent users from completing the survey. For instance, if they are not
#'of a certain age, or do not consent to their involvement.
#'
#' + Start: This section allows you to set questions that come before the BIO-WELL
#' questions. For example, these may be generic questions, such as gaining
#' information on a participant's location, gender or employment.
#'
#'+ BIO-WELL: The questions in this section are all accompanied by the five
#'sliders that comprise the BIO-WELL scale. For each situation given, a new page is added
#'to the survey. See Irvine et al., (2023) for more details on the BIO-WELL
#'scale and survey set-up, including the BIO-WELL stem questions.
#'
#'  1) `biowell_situations`: Text for describing an introductory scenario to
#'cognitively situate the participant within a particular natural environment
#'(i.e. a nearby forest).
#'
#'2) `biowell_questions`: A list the length of `biowell_situations`, in which
#'each element contains a vector of the questions to be asked for each
#'situation. These stem questions are asked about biodiversity metrics (e.g.
#'abundance, species diversity), as well as biodiversity attributes (e.g.
#'smells, colours, shapes) within each `biowell_situation` given.
#'
#'+ End: This section allows you to set questions that follow the BIO-WELL
#'questions. This may include asking participants for their contact details or
#'questions that may help understand patterns in BIO-WELL scores, such as
#'whether individuals spend a lot of time outdoors.
#'
#'+ Report: This section will calculate the participant's average BIO-WELL score
#'across the five wellbeing domains in the BIO-WELL scale (physical, emotional,
#'cognitive, social and spiritual) and provide users with an interpretation of
#'their BIO-WELL score, including a comparison to previous survey participants.
#'
#'# Structure: messages, questions, IDs, types and options.
#'
#'Each section can be specified using arguments relating to the following
#'aspects:
#'
#'## Messages
#'Text that will accompany each section or display throughout the survey.
#'
#'## Questions and IDs
#'
#'Questions provide the exact text for each question to ask participants.
#'
#'IDs provide shorted versions or codes for each question within the survey.
#'These are then used in the output survey response data frame to prevent
#'lengthy column names. For example, the question "The variety of textures in
#'this forest makes me feel..." could be given the ID "Variety of textures".
#'
#'## Response types and options
#'
#'There are five response type options to accompany non-BIO-WELL questions.
#'These include:
#'
#'1) `text` - A simple text box that the participant can type their answer
#'into.
#'
#'2) `selectbox`* - A drop-down box that the participant can choose one
#'answer from.
#'
#'3) `checkbox`* - A group of checkboxes that the participant can choose multiple
#'or none of. Be aware that if `all_questions = TRUE`, participants will have to
#'select one of the check boxes, and so it is best to include an option for
#'selecting none of the other options.
#'
#'4) `likert_five` - A five point Likert scale going from "strongly agree" to
#'"strongly disagree". Find out more information on the Likert scale
#'[here](https://dictionary.apa.org/likert-scales).
#'
#'5) `likert_seven` - A seven point Likert scale that also includes "somewhat
#'agree" and "somewhat disagree". Find out more information on the Likert scale
#'[here](https://dictionary.apa.org/likert-scales).
#'
#' *For `selectbox` and `checkbox` response types, you will need to set the options
#' that participants will have. This can be done using the "...response_options"
#' arguments.
#'
#' To specify this, for each `selectbox` and `checkbox` response type in each
#' section, create a list of equal length to the number of these response types.
#' Each element within the list should contain a character vector of the options.
#'
#' For example:
#'
#' start_questions <- c("How old are you?",
#'                      "What is your current employment?",
#'                      "Which of the following pets do you own?")
#'
#' start_questions_ID <- c("Age","Employment","Pets")
#'
#' start_questions_type <- c("selectbox", "text", "checkbox")
#'
#' start_response_options <- list(c("0-17", "18-49", "51 +"),
#'                                c("dog", "cat", "rabbit", "fish", "snake"))
#'
#'# Screening questions
#'
#'Screening questions offer you the opportunity to remove participants from
#'taking part in the survey if they do not meet certain criteria. These will all
#'be `selectbox` response types. To specify how to screen participants, add
#'":SCREEN" to the response options for which, if chosen, the participant should
#'be removed.
#'
#'For example, if participants do not consent or are under 18, they will be
#'removed from the following survey:
#'
#' screen_questions <- c("Do you consent to this study?","How old are you?")
#'
#' screen_questions_ID <- c("Consent","Age")
#'
#' screen_response_options <- list(c("No:SCREEN", "Yes"),
#'                                 c("0-17:SCREEN", "18-49", "51+"))
#'
#'# Happy with your BIO-WELL survey design?
#'
#'Once you have customised your survey, you need to save the R script as "app.R"
#'within a designated folder for your BIOWELL survey Shiny App.
#'
#'This R script should contain nothing other than "BIOWELL::build_survey()" with
#'the arguments to the function in-filled between the brackets. See Vignette 3
#'for more details.
#'
#'You can then deploy your survey onto the Shiny Server using the `create_URL()`
#'function.
#'
#'# Language options
#'
#' An exciting feature of the `BIOWELL` package is the ability to
#'specify the language for the BIO-WELL scale. Both the biodiversity stem
#'statements and the wellbeing sliders have been professionally translated into 32
#'languages which includes consultation with native speakers. Select a language
#'from the below list.
#'+ `arabic` + `bengali_india` + `chinese_hong_kong` +
#'`chinese_taiwan` + `danish` + `dutch` + `edo` + `english` + `finnish` +
#'`french` + `german` + `gujarati` + `indonesian` + `irish` + `italian` +
#'`japanese` + `korean` + `malay` + `norwegian` + `polish` + `portuguese` +
#'`portuguese_brasil` + `punjabi_india` + `punjabi_pakistan` + `russian` +
#'`scottish_gaelic` + `spanish` + `swedish` + `urdu_india` + `urdu_pakistan` +
#'`welsh`.
#'#'
#'Please see Vignette 2 in the `BIOWELL` package for full guidance on this
#'function.
#'
#'# Extra variables recorded
#'
#'In addition to the participants responses to survey questions, the survey
#'records the following variables with each submitted response:
#'
#'+ `survey_duration` - the time in seconds between the participants starting
#'the survey and submitting their responses.
#'+ `start_date` - the date that participants began the survey in YYYY-MM-DD
#'format.
#'+ `start_time` - the time that participants began the survey in HH-MM-SS
#'format.
#'+ `end_date` - the date that participants submitted their responses to the
#'survey in YYYY-MM-DD format.
#'+ `end_time`  - the time that participants submitted their responses to the
#'the survey in HH-MM-SS format.
#'+ `timezone` - the time zone of participants.
#'+ `timezone_location` - the broad location of the participant's time zone.
#'+ `server_submit_time` - the time in the Shiny Server's time zone that
#'participant responses were submitted.
#'
#'@returns Generates a custom BIO-WELL survey in Shiny.
#'@export
#'@references Irvine, K.N., Fisher, J.C., Bentley, P.R., Nawrath, M., Dallimer,
#'M., Austen, G.E., Fish, R. and Davies, Z.G., 2023. BIO-WELL: The development
#'and validation of a human wellbeing scale that measures responses to
#'biodiversity. Journal of Environmental Psychology, 85, p.101921. doi:10.1016/j.jenvp.2022.101921.
#'@examplesIf interactive()
#'build_survey(
#' survey_title = "Insert survey title here",
#'  sidepanel_message = "Insert sidepanel message here.",
#'  start_message = "Insert start message here",
#'  screen_message = "Insert screen message here",
#'  screen_questions = c("Insert participant screening question here"),
#'  screen_questions_ID = c("Screening_Q1"),
#'  screen_response_options = list(c(
#'    "You will be removed:SCREEN",
#'    "You will not be removed"
#'  )),
#'  start_questions = c(
#'    "Insert start question: text",
#'    "Insert start question: selectbox",
#'    "Insert start question: checkbox",
#'    "Insert start question: likert_five",
#'    "Insert start question: likert_seven"
#'  ),
#'  start_questions_ID = c(
#'    "text_SQ1",
#'    "selectbox_SQ2",
#'    "checkbox_SQ3",
#'    "likert_five_SQ4",
#'    "likert_seven_SQ5"
#'  ),
#'  start_questions_type = c("text",
#'                           "selectbox",
#'                           "checkbox",
#'                           "likert_five",
#'                           "likert_seven"),
#'  start_response_options = list(
#'    c('Choose',  'one',  'of', 'these'),
#'    c('Choose',  'multiple',  'of', 'these')
#'  ),
#' biowell_situations = c(
#'   "Insert description of biowell situation 1.",
#'    "Insert description of biowell situation 2."
#'  ),
#' biowell_situations_ID = c("situation1", "situation2"),
#'  biowell_questions = list(
#'    c("Insert biodiversity stem question here BW1"),
#'    c(
#'      "Insert biodiversity stem question here - BW2",
#'      "Insert biodiversity stem question here - BW3"
#'    )
#'  ),
#'  biowell_questions_ID = list(c("BW.S1.Q1"),
#'                              c("BW.S2.Q1",
#'                                "BW.S2.Q2")),
#'  end_questions = c(
#'    "Insert end question: text",
#'    "Insert end question: selectbox",
#'    "Insert end question: checkbox",
#'   "Insert end question: likert_five",
#'    "Insert end question: likert_seven"
#'  ),
#'  end_questions_ID = c(
#'    "text_EQ1",
#'    "selectbox_EQ2",
#'    "checkbox_EQ3",
#'    "likert_five_EQ4",
#'    "likert_seven_EQ5"
#'  ),
#'  end_questions_type = c("text",
#'                         "selectbox",
#'                         "checkbox",
#'                         "likert_five",
#'                         "likert_seven"),
#'  end_response_options = list(
#'    c('Choose',  'one',  'of', 'these'),
#'   c('Choose',  'multiple',  'of', 'these')
#' ),
#'  end_message = "Insert end message here.",
#'  Dropbox_App_folder = "insert_Dropbox_App_folder_name",
#'  BW_app_path = tempdir(),
#'  organisation = "BIOWELL package example",
#'  organisation_website = "https://github.com/r-a-dobson/BIOWELL",
#'  all_questions = TRUE,
#'  all_sliders = FALSE,
#'  user_report = TRUE,
#'  offline_mode = TRUE
#')


build_survey <- function(survey_title = NULL,
                         sidepanel_message = NULL,
                         organisation,
                         organisation_website = NULL,
                         screen_message = NULL,
                         screen_questions,
                         screen_questions_ID,
                         screen_response_options=NULL,
                         start_message = NULL,
                         start_questions,
                         start_questions_ID,
                         start_questions_type,
                         start_response_options=NULL,
                         biowell_situations,
                         biowell_situations_ID,
                         biowell_questions,
                         biowell_questions_ID,
                         end_message = NULL,
                         end_questions,
                         end_questions_type,
                         end_questions_ID,
                         end_response_options = NULL,
                         all_questions = TRUE,
                         all_sliders = TRUE,
                         user_report = TRUE,
                         language = "english",
                         offline_mode = FALSE,
                         return = "run",
                         Dropbox_App_folder,
                         BW_app_path) {

  #----------------------------------------------------------------------------
  # Catch errors and set defaults
  #----------------------------------------------------------------------------

  # Check for Dropbox token
  if(!offline_mode) {

    if (!file.exists(".secrets/dropbox_token.rds")) {
      stop("No Dropbox token found. Use activate_dropbox()")
    }

    token<-readRDS(".secrets/dropbox_token.rds")

    rdrop2::drop_dir(dtoken = token)

    # Check folder exists within the Dropbox app
    tryCatch({
      nresponses <- nrow(rdrop2::drop_dir(Dropbox_App_folder, dtoken = token))
    }, error = function(e) {
      stop("Dropbox folder does not exist within your Dropbox App")
    })
  }

  # Check screen question input
  if(!missing(screen_questions)) {

    if (missing(screen_questions_ID)) {
      screen_questions_ID <- screen_questions
    }
    screen_questions_type <- rep("selectbox",length(screen_questions))

    if (!length(screen_response_options) == length(screen_questions)) {
      stop("end_response_options not equal to total screen_questions."
      )
    }
  }


  # Check start question input

  if(!missing(start_questions)) {

    if (missing(start_questions_ID)) {
      start_questions_ID <- start_questions
    }

    if(!length(start_questions) == length(start_questions_type)){
      stop("Please provide one response type for each start question")
    }

    n<- sum(start_questions_type=="checkbox", start_questions_type=="selectbox")

    if (!length(start_response_options) == n) {
    stop("start_response_options not equal to total checkbox + selectbox types")
    }

  }


  # Check end question input

  if(!missing(end_questions)) {

    if (missing(end_questions_ID)) {
      end_questions_ID <- end_questions
    }

    if(!length(end_questions) == length(end_questions_type)){
      stop("Please provide one response type for each end question")
    }

    n<- sum(end_questions_type=="checkbox", end_questions_type=="selectbox")

    if (!length(end_response_options) == n) {
      stop("end_response_options not equal to total checkbox + selectbox types")
    }

  }



  # If only one situation ...

  if(length(biowell_situations) == 1){

    if(!inherits(biowell_questions,"list")){
      biowell_questions<-list(biowell_questions)
    }
  }

  if (length(biowell_situations) > 1) {
    if (!inherits(biowell_questions, "list")) {
      stop(
        "biowell_questions should be a list the length of biowell_situations. "
      )
    }
  }

  if (missing(biowell_situations_ID)) {
    biowell_situations_ID <- biowell_situations
  }

  if (missing(biowell_questions_ID)) {
    biowell_questions_ID <- biowell_questions
  }


  if (missing(screen_questions)) {
    screen_questions <- "Do you want to continue to the survey?"
    screen_questions_ID <- "continue_to_survey"
    screen_questions_type <- "selectbox"
    screen_response_options <- c("yes")
    no_screen <- "no_screen"
  }

  if (missing(start_questions)) {
    start_questions <- NULL
    start_df <- NULL
  }

  if (missing(end_questions)) {
    end_questions <- NULL
    end_df <- NULL
  }

  match.arg(language, choices = c('arabic',
                                  'bengali_india',
                                  'chinese_hong kong',
                                  'chinese_taiwan',
                                  'danish',
                                  'dutch',
                                  'edo',
                                  'english',
                                  'finnish',
                                  'french',
                                  'german',
                                  'gujarati',
                                  'indonesian',
                                  'irish',
                                  'italian',
                                  'japanese',
                                  'korean',
                                  'malay',
                                  'norwegian',
                                  'polish',
                                  'portuguese',
                                  'portuguese_brasil',
                                  'punjabi_india',
                                  'punjabi_pakistan',
                                  'russian',
                                  'scottish_gaelic',
                                  'spanish',
                                  'swedish',
                                  'urdu_india',
                                  'urdu_pakistan',
                                  'welsh'))


  if(!curl::has_internet()) {
    offline_mode <- TRUE
    message("You are working offline. No data will be stored to Dropbox.")
  }

  if (!getwd() == BW_app_path){
    message("Ensure your current working directory is your BIO-WELL app folder")
  }

  #----------------------------------------------------------------------------
  # Create survey's Shiny App UI element
  #----------------------------------------------------------------------------

  # Default messages

  sb <- "Please do not close the window until your responses have been submitted."
  aq <- "In this survey, you must answer all questions to complete."
  pc <- "These questions must be answered to proceed."
  gb <- "Sorry you have not met the criteria for this survey. Please exit."
  fm <- "Thank you. Please close your browser to finish the survey."
  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    title = survey_title,     # Set the name that will appear on browser tab
    shinytitle::use_shiny_title(),
    shinyFeedback::useShinyFeedback(),
    shiny::titlePanel(shiny::div(survey_title,
                                 style = "color: #004A86;font-weight: bold")),
    shiny::sidebarPanel(
      shiny::div("Summary", style = "font-weight: bold; font-size: 20px"),
      if(!missing(sidepanel_message)){shiny::tags$div(sidepanel_message)},
      shiny::br(),
      if(!missing(organisation)){shiny::tags$div("\n Created by:")},
      if(!missing(organisation)){shiny::tags$a(organisation,
                                               href=organisation_website)},
      shiny::br(),
      shiny::br(),
      shiny::p(sb , style = "font-weight: bold"),
      shiny::br(),
      if(all_questions){shiny::p(aq ,style = "font-style: italic")},
      shiny::tags$script('
    $(function() {
      var time_now = new Date()
      $("input#client_time").val(time_now.getTime())
      $("input#client_time_zone_char").val(time_now.toTimeString())
      $("input#tz_intern").val(Intl.DateTimeFormat().resolvedOptions().timeZone)
    });
  '),
      shiny::tags$script('
            $(document).ready(function(){
            var d = new Date();
            var target = $("#clientTime");
            target.val(d.toLocaleString());
            target.trigger("change");
            });
            '),
      shiny::conditionalPanel('false',shiny::textInput("clientTime",
                                                       "Client Time",
                                                       value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time",
                                                        "Client Time",
                                                        value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time_zone_char",
                                                        "Time zone verbatim",
                                                        value = "")),
      shiny::conditionalPanel('false', shiny::textInput("tz_intern",
                                                        "Time zone international",
                                                        value = ""))

    ),

    shiny::mainPanel(
      shinyjs::hidden(
        shiny::div(class = "page",
                   id = "page1",
                   shiny::tags$div(style = "color:# 004A86;font-size: 18px;",
                   screen_message),
        lapply(add_questions(screen_questions,
                             screen_questions_type,
                             screen_response_options,
                             all_questions = all_questions),
               FUN = function(x) {
                 shiny::HTML(paste(x))
               }),

        shiny::tags$div(id = "Proceed", style = "color: red;
                                                 font-size: 16px;
                                                 font-style: bold",pc),
        shiny::actionButton("continue", "Continue to the survey")),


        shiny::div(class = "page",
                   id = "page2",
                   shiny::tags$div(style = "color: #004A86;font-size: 18px;",
                                   start_message),
                   if (!is.null(start_questions)) {
                     lapply(add_questions(start_questions,
                                          start_questions_type,
                                          start_response_options,
                                          all_questions = all_questions,
                                          prior_qs = screen_questions,
                                          type_q = "end"),
                            FUN = function(x){shiny::HTML(paste(x))})},
                   shiny::textOutput("warn"),
                   shiny::tags$head(shiny::tags$style("#warn{color: red;
                                     font-size: 20px;
                                     font-style: bold;}"))
        ),

        add_biowell_scale(biowell_situations,
                          biowell_questions,
                          all_sliders,
                          language),

        shiny::div(class = "page",
                   id = paste0("page", 3+length(biowell_situations)),
                   shiny::tags$div(id="end_m",
                                   style="color: #004A86;font-size: 18px",
                                   end_message),
                   if (!is.null(end_questions)) {
                     lapply(add_questions(end_questions,
                                          end_questions_type,
                                          end_response_options,
                                          type_q = "end",
                                          prior_qs = c(screen_questions,
                                                       start_questions),
                                          all_questions = all_questions),
                            FUN=function(x){shiny::HTML(paste(x))})},
                   shiny::textOutput("warn2"),
                   shiny::tags$head(shiny::tags$style("#warn2{color: red;
                                           font-size: 20px;
                                           font-style: bold;}")),
                   shiny::actionButton("submit", "Submit")

        ),

        shiny::div(class = "page",
                   id = paste0("page", 4+length(biowell_situations)),
                   shiny::fluidRow(
                     if (!user_report) {
                       shiny::p(fm)
                     },
                     if (user_report) {
                       shiny::div(style = "border-style: solid;
                                           border-color: #004A86;
                                           height: 1500px;
                                           width:100%;
                                           padding:9.5px;
                                           border-width: 5px;
                                           border-radius: 10px;
                                           justify-content: center;
                                           align-items: center;
                                           position: relative;",
                                  shiny::h1("Your BIO-WELL score is:"),
                                  shiny::tags$head(
                                  shiny::tags$style('h1 {font-size: 4vw;
                                           color: #0097AD;
                                           font-weight: bold;
                                           text-align: center;
                                           font: 1px}')),
                                  shiny::div(class = "square",
                                             shiny::textOutput("text"),
                                             shiny::tags$head(shiny::tags$style(
                                             '.square {
                                             width: 60%;
                                             height:15%;
                                             line-height: 110%;
                                             border-radius: 5%;
                                             font-size: 700%;
                                             color: white;
                                             position:absolute;
                                             bottom:20%;
                                             top:7%;
                                             padding: 0px 0;
                                             right:25%;
                                             left:20%;
                                             vertical-align: middle;
                                             text-align: center;
                                             background: #0097AD}'))),

                                  shiny::div(class = "square2",
                                             "100",
                                             shiny::tags$head(shiny::tags$style(
                                             '.square2 {
                                              width: 60%;
                                              height:15%;
                                              line-height: 110%;
                                              border-radius: 0%;
                                              font-size: 700%;
                                              color: white;
                                              position:absolute;
                                              bottom:20%;
                                              top:15%;
                                              padding: 0px 0;
                                              right:25%;
                                              left:20%;
                                              vertical-align: middle;
                                              text-align: center;
                                              background: transparent}'))),
                                  shiny::htmlOutput("rep"),
                                  shiny::tags$style('#mydiv6{list-style-position:
                                  inside;
                                  padding-left: 0;
                                  font-size: 20px;
                                  color: black;
                                  text-align: center;
                                  position:absolute;
                                  bottom:1%;
                                  top:23%;
                                  right:5%;
                                  left:5%;
                                  ul {margin-left: 0;text-align: left};}'),
                                  shiny::div(class = "line",
                                             shiny::tags$head(shiny::tags$style(
                                             '.line { width: 30%;
                                                      height:1%;
                                                      line-height: 5%;
                                                      border-radius: 0%;
                                                      position: absolute;
                                                      bottom: 20%;
                                                      top: 14%;
                                                      padding: 0px 0;
                                                      right: 25%;
                                                      left: 35%;
                                                      vertical-align: middle;
                                                      text-align: center;
                                                      background: white}'))),
                                  shiny::tags$head(shiny::tags$style(
                                    '#mytable {background: white}'))

                       )}
                   ),
        ),
        shiny::div(class = "screen_page",
                   id = paste0("screen_page"),
                   shiny::p(gb))
      ),
      shiny::br(),
      shiny::actionButton("prevBtn", "< Previous"),
      shiny::actionButton("nextBtn", "Next >")
    ))

  #----------------------------------------------------------------------------
  # Create survey's Shiny App server element
  #----------------------------------------------------------------------------

  server <- function(input, output, session) {

    # Get system time on survey start
    start1 <- Sys.time()

    # If no screening questions given, hide screening page
    if(exists("no_screen")) {
      shinyjs::hide(id = "select1")
      shinyjs::hide(id = "Proceed")
    }

    # Create reactive values for controlling
    pg <- shiny::reactiveValues(page = 1) # Page number
    showNEXT<- shiny::reactiveVal(FALSE) # TRUE if requirements reached -  next page
    showEND <- shiny::reactiveVal(FALSE) # Screened out page
    showSUBMIT <- shiny::reactiveVal(FALSE) # Submit - to report / allow data upload


    # Code to ensure if all_slider = TRUE, can only click next if all moved
    shiny::observe({

      if (pg$page > 2 && pg$page <= 2 + length(biowell_situations)) {

        if (!all_sliders) {
          showNEXT(TRUE)
        }

        if (all_sliders) {

          slider_no <- unlist(lapply(1:length(biowell_questions),
                                     FUN = function(x) {
                                        rep(x, length(biowell_questions[[x]]))}))

          slider_no <- data.frame(page = 2 + slider_no,
                                  number = 1:length(slider_no))

          page_split<-slider_no[slider_no$page==pg$page,]

          # Get IDs of sliders on current page that need to move
          must_move<-c(paste0("physical",  page_split$number,"ID"),
                           paste0("social",    page_split$number,"ID"),
                           paste0("emotional", page_split$number,"ID"),
                           paste0("cognitive", page_split$number,"ID"),
                           paste0("spiritual", page_split$number,"ID"))

          # Get input into sliders
          slide_in <-get_d(input)

          # Only look at sliders on current page
          slide_in <- slide_in[, must_move]

          if (!length(slide_in) == 0) {
            showNEXT(FALSE)

            # When they are no longer 50, take them off the list and continue
            if (any(!slide_in == 50)) {
              slide_in <- slide_in[-which(!slide_in == 50)]
            }
          }

          # If no more to move, then toggle next button on
          if (length(slide_in) == 0) {
            showNEXT(TRUE)
          }

        }}

    })


    # Controlling the page

    NUM_PAGES <- c(2 + length(biowell_situations) + 2)
    navPage <- function(direction) {pg$page <- pg$page + direction}
    shiny::observeEvent(input$prevBtn, navPage(-1))
    shiny::observeEvent(input$nextBtn, navPage(1))

    shiny::observe({

      # Hide next button - must click submit to generate report
      if (NUM_PAGES - 1 == pg$page) {
        shinyjs::hide("nextBtn")

      }
      # Previous button active unless first page
      shinyjs::toggleState(id = "prevBtn", condition = pg$page > 1)

      # On any other page...
      if(!NUM_PAGES-1 == pg$page){
        # Turn off if we are showing the end page (screening)
        shinyjs::toggleState(id = "nextBtn", condition =  !showEND())
        if(showEND()){shinyjs::hide("prevBtn")
          }

        # Turn on if other rules met (showNEXT = TRUE)
        shinyjs::toggleState(id = "nextBtn", condition =  showNEXT())}


      shinyjs::toggle(id = "screen_page", condition =  showEND())
      shinyjs::hide(selector = ".page")
      shinyjs::show(
        paste0("page", pg$page)
      )

    })

    # Ensure next button is only hidden if on penultimate page
    shiny::observe({
      if(pg$page == NUM_PAGES-1){
        shinyjs::hide(id = "nextBtn")}
      if(pg$page != NUM_PAGES-1){
        shinyjs::show(id = "nextBtn")}
    })


    output$warn2<- shiny::renderText("Please answer all questions.")

    # Have the screen questions been answered?
    shiny::observeEvent(input$continue,

       if (!is.null(screen_questions)) {

         # Get screening response options
         scrn_rs<-add_questions(screen_questions,
                                  screen_questions_type,
                                  screen_response_options,
                                  return_screen = TRUE)
         # Get user input
         scrn_in <- get_d(input)
         scrn_in2 <- as.data.frame(scrn_in[, scrn_rs$inputname])

         # For every screening q, check input is not the disallowed response
         for (x in 1:nrow(scrn_in2)) {
           if (scrn_in2[x, ] == scrn_rs$inputresponse[x]) {
             showEND(TRUE)
             navPage(NUM_PAGES)
           }
         }

         # If after all checked, met screen criteria - move to next page !
         if (!showEND()) {

        # Check the boxes have been moved from empty
           if (!"" %in% scrn_in[, paste0("select", 1:length(screen_questions))])
             navPage(1)
         }
       })

    start_nms <-add_questions(start_questions,
                              start_questions_type,
                              start_response_options,
                              return_names=T,
                              prior_qs = screen_questions,
                              type_q = "end" )

    start_txt <- start_nms[grepl("text|select", start_nms)]

    end_nms <- add_questions(end_questions,
                             end_questions_type,
                             end_response_options,
                             return_names = T,
                             prior_qs = c(screen_questions, start_questions),
                             type_q = "end")

    end_txt <-  end_nms[grepl("text|select", end_nms)]

    # Have the start questions been answered?
    shiny::observe({

      # If no start questions, then activate the next button
      if (is.null(start_questions) && pg$page == 2) {
        showNEXT(TRUE)
      }

      # If no start questions, then activate the next button
      if (!all_questions && pg$page == 2) {
        showNEXT(TRUE)
      }

      if(all_questions){

        if(!is.null(start_questions)){
          if(pg$page == 2){

            # Check for missing input cols or blank spaces
            if (answers(start_nms, colnames(get_d(input))) ||
                "" %in% get_d(input)[, start_txt]) {

            output$warn<- shiny::renderText("Please answer all questions.")

            shinyjs::show("warn")
            showNEXT(FALSE)
            } else {
              showNEXT(TRUE)
              shinyjs::hide("warn")
            }
          }}}

    })

    # Have the end questions been answered?
    if (!is.null(end_questions)) {
      shiny::observe({
        if (!answers(end_nms, colnames(get_d(input)))
            && !"" %in% get_d(input)[, end_txt]) {
          showSUBMIT(TRUE)
        }
      })
    } else
      (showSUBMIT(TRUE))

    shinyjs::hide(id = "warn2")

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {

      if (!all_questions) {
        showSUBMIT(TRUE)
      }

      if (!showSUBMIT()) {
        shinyjs::show(id = "warn2")
      }

      if (showSUBMIT()) {
        shinyjs::hide(id = "submit")
        shinyjs::hide(id = "prevBtn")
        shinyjs::hide(id = "nextBtn")
        shinyjs::hide(id = "end_m")
        shinyjs::show(paste0("page", NUM_PAGES))
        shinyjs::hide(paste0("page", NUM_PAGES-1))
        dataframe <- get_d(input)

        screen_df <- sort_qa(paste0("select", length(screen_questions)),
                                dataframe,
                                screen_questions_ID,
                                screen_response_options)

        # Sort out start question segment of results df using IDs
        if(!is.null(start_questions)){

          start_df <- sort_qa(start_nms,
                              dataframe,
                              start_questions_ID,
                              start_response_options)}

        # Sort out end question segment of results df using IDs
        if(!is.null(end_questions)) {
          end_df <- sort_qa(end_nms,
                            dataframe,
                            end_questions_ID,
                            end_response_options)
        }

        results <- generate_results_data(start1,
                                         dataframe,
                                         screen_df,
                                         end_df,
                                         start_df,
                                         input$clientTime,
                                         input$client_time_zone_char,
                                         input$tz_intern,
                                         biowell_situations_ID,
                                         biowell_questions_ID)

        nresponses <- length(list.files(tempdir()))

        # If internet get no. responses to survey so far
        if (!offline_mode) {
          nresponses <- nrow(rdrop2::drop_dir(Dropbox_App_folder,
                                              dtoken = token))
        }

        # Save file to temp directory to then upload to Dropbox
        filePath <- file.path(paste0(tempfile(), "_ID_", nresponses, ".csv"))

        # Upload the file to Dropbox
       write.csv(results, filePath, row.names = FALSE, quote = TRUE)

        results <- read.csv(filePath)

        running_average <- sort_running_average(filePath,
                                                Dropbox_App_folder,
                                                token,
                                                results,
                                                offline_mode)

        output$text <- shiny::renderText({
          paste(round(results$mean_biowell_INVERTED))
        })

        output$rep <- shiny::renderUI({
          generate_report(results, running_average)
        })

      }})

    # Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(shiny::stopApp)
  }

  if (return == "run") {
    return(shiny::shinyApp(ui, server))
  }

  if (return == "ui") {
    return(ui)
  }

  if (return == "server") {
    return(server)
  }

}


