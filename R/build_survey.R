#'Create BIO-WELL survey Shiny App
#'
#'Function to generate custom BIO-WELL survey with R Shiny
#'
#'@param survey_title a character string, the title of the survey to display on
#'  each page.
#'@param sidepanel_message a character string, text to display on side panel
#'  throughout survey.
#'@param Dropbox_App_folder path to folder in Dropbox App to save survey
#'  responses to. Defaults to the root directory.
#'@param BW_app_path a character string, the path to your BIO-WELL Shiny App
#'  folder.
#'@param organisation a character string, the name of the organisation running
#'  the survey.
#'@param organisation_website a character string, the web address for the
#'  organisation running the survey.
#'@param screen_message a character string, text to display above screening
#'  questions.
#'@param screen_questions a character string or vector, the screening questions
#'  to ask.
#'@param screen_questions_ID a character string or vector the length of
#'  screen_questions, IDs for screen questions in output response data frame.
#'@param screen_response_options a list the length of screen_questions with each
#'  element containing vector of choices for answering screen questions. To
#'  specify answers to screen again add :SCREEN to the option. See details for
#'  an example.
#'@param start_message a character string, text to display above the start
#'  questions.
#'@param start_questions a character string or vector, the questions to ask
#'  participants prior to the bio-well questions.
#'@param start_questions_ID a character string or vector the length of
#'  start_questions, IDs for start questions in output response data frame.
#'@param start_questions_type optional; a character string or vector, the type
#'  of response for each start question. Must be length of `start_questions`.
#'  One of; `text`, `checkbox`, `selectbox`, `likert_five` and `likert_seven`.
#'  See details for more information.
#'@param start_response_options optional; a list of character strings or
#'  vectors, the options to offer for each `start_questions` of type `checkbox`
#'  or `selectbox`.
#'@param biowell_situations a character string or vector, describing the
#'  environmental space setting for BIO-WELL questions.
#'@param biowell_situations_ID a character string or vector the length of
#'  biowell_situation, IDs for each bio-well situation in output response data
#'  frame.
#'@param biowell_questions a list of character strings or vectors, the questions
#'  for each separate `biowell_situations` for BIO-WELL responses.
#'@param biowell_questions_ID a character string or vector the length of
#'  biowell_questions, IDs for bio-well questions in output response data frame.
#'@param end_message a character string, text to display above the end
#'  questions.
#'@param end_questions optional; a character string or vector, the questions to
#'  ask participants following the bio-well questions.
#'@param end_questions_type optional; a character string or vector, the type of
#'  response for each end question. Must be length of `end_questions`. One of;
#'  `text`, `checkbox`, `selectbox`, `likert_five` and `likert_seven`. See
#'  details for more information.
#'@param end_questions_ID a character string or vector the length of
#'  end_questions, IDs for end questions in output response data frame.
#'@param end_response_options optional; a list of character strings or vectors,
#'  the options to offer for each `end_questions` of type `checkbox` or
#'  `selectbox`.
#'@param all_questions a logical, indicating whether all start and end questions
#'  must be answered to continue with the survey.
#'@param all_sliders a logical, indicating whether all sliders must be moved at
#'  least once to continue with the survey.
#'@param user_report a logical, indicating whether to generate a BIO-WELL score
#'  report for participants. See details for more information.
#'@details
#'
#'# BIO-WELL survey Shiny App structure
#'
#' + Screening: This section allows you to set questions where certain responses
#'will prevent users from completing the survey. For instance, if they are not
#'of a certain age, or do not consent their involvement.
#'
#' + Start: This section allows you to set questions that precede the BIO-well
#' questions. For example, these may be generic questions, such as gaining
#' information on participants location, gender or employment.
#'
#'+ BIO-WELL: The questions in this section are all responded to be participants
#'using the five sliders that comprise the BIO-WELL scale. For each situation
#'given (although often there may be only be one environment of interest), a new
#'page is added to the survey. See Irvine et al., (2023) for more details on the
#'BIO-WELL scale and survey set-up.
#'
#'+ End: This section allows you to set questions that follow the BIO-WELL
#'questions. This may include asking participants for their contact details or
#'questions that may help disentangle patterns in BIO-WELL scores, such as
#'whether individuals spend a lot of time outdoors.
#'
#'+ Report: This section will calculate the participants average BIO-WELL score
#'across the five wellbeing domains in the BIO-WELL scale (physical, emotional,
#'cognitive, social and spiritual) and provide users some interpretation of
#'their BIO-WELL scores compared to other survey participants.
#'
#'# Structure: messages, question, IDs, types and options.
#'
#'Each section can be specified using arguments relating to the following aspects:
#'
#'## Messages
#'Text that will accompany each section or display throughout the survey.
#'
#'## Questions and IDs
#'
#'Questions provide the exact text for each question to ask participants.
#'
#'IDs provide shorted versions or codes for each question that can be used in
#'the output data frame or participants responses. This streamlines the data
#'frame and facilitates analyses of results.
#'
#'For example, the question "The variety of textures in this forest makes me
#'feel…" could be given the ID "Variety of textures", so that output data frame
#'and plots do not have lengthy names.
#'
#'
#'## Response types and options
#'
#'There are five response type options to accompany non-bio-well questions.
#'These include:
#'
#'1) text - This is a simple text box that the participant can type their answer
#'into.
#'
#'2) selectbox* - This is a drop down box that the participant can choose one
#'answer from.
#'
#'3) checkbox* - This is a list of checkboxes that the participant can choose
#'multiple or none of.
#'
#'4) likert_five - This is a five point likert slider scale going from "strongly
#'agree" to "strongly disagree".
#'
#'5) likert_seven - This is a seven point likert slider scale that also includes
#'"somewhat agree" and "somewhat disagree".
#'
#' For selectbox and checkbox response types, you will want to set the options
#' that participants have available to them. This can be done using the
#' "...response_options" arguments.
#'
#' *To specify this, for each selectbox and checkbox response type in each
#' section, create a list the same length, with each element within the list
#' containing a vector of options for each. This must be in the same order as
#' the questions come within the section.
#'
#' For example:
#' start_questions <- c("How old are you?",
#'                      "What is your current employment?",
#'                      "Which of the following pets do you own?")
#' start_questions_ID <- c("Age","Employment","Pets")
#' start_questions_type <- c("selectbox", "text", "checkbox")
#' start_response_options <- list(c("0-17", "18-49", "51+"),
#'                                c("dog", "cat", "rabbit", "fish", "snake"))
#'
#'# Screening questions
#'
#'Screening questions offer you the opportunity to remove participants from
#'taking part if they do not meet certain criteria. These all must be selectbox
#'response types. It is recommended to set the screening answer as the first
#'option, as it will default to this and so users will have to actively deselect
#'them to continue. This can be done by adding ":SCREEN" to the response options
#'for which, if chosen, the participant should be removed.
#'
#'For example:
#' screen_questions <- c("Do you consent to this study?","How old are you?")
#' screen_questions_ID <- c("Consent","Age")
#' screen_response_options <- list(c("No:SCREEN", "Yes"),
#'                                 c("0-17:SCREEN", "18-49", "51+"))
#'
#'
#'# Happy with your BIO-WELL survey design?
#'
#'Once you have customised your survey, you need to save the R script as "app.R"
#'with a designated folder for your BIOWELL survey Shiny App.
#'
#'This R script should contain nothing other than "BIOWELL::build_survey()" with
#'the arguments to the function in-filled between the brackets. See Vignette 3
#'for more details.
#'
#'You can then deploy your survey onto the Shiny Server using the `create_URL()`
#'function.
#'
#'@returns Generates a custom BIO-WELL survey Shiny App.
#'@export
#'@references Irvine, K.N., Fisher, J.C., Bentley, P.R., Nawrath, M., Dallimer,
#'M., Austen, G.E., Fish, R. and Davies, Z.G., 2023. BIO-WELL: The development
#'and validation of a human wellbeing scale that measures responses to
#'biodiversity. Journal of Environmental Psychology, 85, p.101921.
#'@example
#'\dontrun{
#'build_survey(
#'  survey_title = "Forest biodiversity and human wellbeing",
#'  sidepanel_message= "The questions in this survey are designed to explore how forest biodiversity impacts wellbeing.",
#'  start_message= "Welcome to the survey. Please fill out the questions below.",
#'  screening_message="Consent to participate in research",
#'  screen_questions = c("I confirm I have read and understood the Participant Information Sheet for the Forest Questionnaire. I understand that my participation is voluntary and that I am free to withdraw at any time without giving any reason. I agree to take part in this research project.",
#'                       "What is your age?"),
#'  screen_questions_ID = c("Consent", "Age"),
#'  screen_questions_type = c("selectbox", "selectbox"),
#'  screen_drop_down_options = list(c("No:SCREEN", "Yes"), c('0-17:SCREEN',  '18-29',  '30-59', 'Over 60',  'Prefer not to say')),
#'  start_questions = c("How would you describe your gender?", "What country do you currently live in?", "Do you live in an area that you consider mainly…"),
#'  start_questions_ID = c("Gender", "Country", "Urbanicity"),
#'  start_questions_type = c("selectbox", "selectbox", "selectbox"),
#'  start_drop_down_options = list(c('Male',  'Female',  'In another way', 'Prefer not to say'),
#'                                 c('Scotland',  'England',  'Wales'),
#'                                 c("Urban", "Rural")),
#'  biowell_situations=c("For each of the following questions,  imagine yourself in a nearby forest at this time of year. Please think about the living things,  including the plants,  fungi and animals (but not pets,  horses,  cows,  sheep),  in that forest."),
#'  biowell_situations_ID=c("forest"),
#'  biowell_questions = list(c("encountering the living things (e.g. plants,  fungi and animals) in this forest makes me feel…",
#'                             "The number of living things (e.g. plants,  fungi and animals) in this forest makes me feel…",
#'                             "The variety of living things (e.g. plants,  fungi and animals) in this forest makes me feel…",
#'                             "The interactions between plants,  fungi and animals (e.g. pollination,  predator-prey) in this forest make me feel…",
#'                             "The living processes (e.g. decomposing,  growing) that happen in this forest make me feel…",
#'                             "The variety of sounds in this forest makes me feel…",
#'                             "The distinctive sounds in this forest make me feel…",
#'                             "The variety of colours in this forest makes me feel…",
#'                             "The vivid colours in this forest make me feel…",
#'                             "The variety of shapes in this forest make me feel…",
#'                             "The maturity of living things (e.g. plants,  fungi and animals) in this forest makes me feel…",
#'                             "The variety of textures in this forest makes me feel…",
#'                             "The sponginess of living things (e.g. plants,  fungi and animals) in this forest makes me feel…",
#'                             "The variety of smells in this forest makes me feel…",
#'                             "The woody smells in this forest make me feel…",
#'                             "Changes in this season make me feel...",
#'                             "The presence of animals in this forest makes me feel…")),
#'  biowell_questions_ID = list(c("Encountering living things",
#'                                "Number of living things",
#'                                "Variety of living things",
#'                                "Interactions between living things",
#'                                "Living processes",
#'                                "Variety of sounds",
#'                                "Distinctive sounds",
#'                                "Variety of colours",
#'                                "Vivid colours",
#'                                "Variety of shapes",
#'                                "Maturity of living things",
#'                                "Variety of textures",
#'                                "Sponginess of living things",
#'                                "Variety of smells",
#'                                "Woody smells",
#'                                "Changes in this season",
#'                                "Presence of animals")),
#'  end_questions = c("How much do you agree that you spent a lot of time in forests as a child?",
#'                    "How much do you agree that you spent a lot of time in forests as a teenager?",
#'                    "How much do you agree that you spent a lot of time in forests as a adult?",
#'                    "Please select any sensory impairments (e.g. difficulty hearing) that you have:"),
#'  end_questions_type = c("likert_five", "likert_five", "likert_five", "checkbox"),
#'  end_questions_ID = c("visits_aschild", "visits_asteen", "visits_asadult", "impairment"),
#'  end_drop_down_options=list(c("smell", "sound", "sight")),
#'  end_message = "Thank you for completing the survey. Click submit to generate report",
#'  drop_box_folder = "survey_responses",
#'  BW_app_path = paste0(tempdir(),"/my_app_name_1"),
#'  organisation = "BIOWELL package example",
#'  organisation_website = "https://github.com/r-a-dobson/BIOWELL",
#'  all_questions=TRUE,
#'  all_sliders = TRUE,
#'  user_report = TRUE)
#'
#'}

build_survey <- function(survey_title,
                         sidepanel_message,
                         Dropbox_App_folder,
                         BW_app_path,
                         organisation,
                         organisation_website,
                         screen_message,
                         screen_questions,
                         screen_questions_ID,
                         screen_response_options=NULL,
                         start_message,
                         start_questions,
                         start_questions_ID,
                         start_questions_type,
                         start_response_options=NULL,
                         biowell_situations,
                         biowell_situations_ID,
                         biowell_questions,
                         biowell_questions_ID,
                         end_message,
                         end_questions,
                         end_questions_type,
                         end_questions_ID,
                         end_response_options = NULL,
                         all_questions = TRUE,
                         all_sliders = TRUE,
                         user_report = TRUE,
                         ...) {

  #----------------------------------------------------------------------------
  # Catch errors and set defaults
  #----------------------------------------------------------------------------

  if(!curl::has_internet()) {
    message("You are currently working offline. No data will be stored to Dropbox.")
  }

  if (!getwd() == BW_app_path){
    stop("Ensure your current working directory is your BIO-WELL app folder")
  }

  # Check for Dropbox token
  if(curl::has_internet()) {

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

  # Set defaults to prevent error

  if (missing(screen_message)) {
    screen_message <- ""
  }

  if(missing(survey_title)) {
    survey_title <- ""
  }

  if (missing(sidepanel_message)) {
    sidepanel_message <- ""
  }

  if (missing(start_message)) {
    start_message <- ""
  }

  if (missing(end_message)) {
    end_message <- ""
  }


  if (missing(organisation_website)) {
    organisation_website <- NULL
  }


  # Check screen question input

  if(!missing(screen_questions)) {

    if (missing(screen_questions_ID)) {
      screen_questions_ID <- screen_questions
    }
    screen_questions_type <- rep("selectbox",length(screen_questions))

    n<- sum(screen_questions_type=="checkbox", screen_questions_type=="selectbox")

   if (!length(screen_response_options) == n) {
     stop("total screen_response_options not equal to total checkbox + selectbox screen_questions_type"
     )
   }
  }


  # Check start question input

  if(!missing(start_questions)) {

    if (missing(start_questions_ID)) {
      start_questions_ID <- start_questions
    }

    if(missing(start_questions_type)){
      stop("Please specify start question response type")
    }

    if(!length(start_questions) == length(start_questions_type)){
      stop("Please provide one response type for each start question")
    }

    n<- sum(start_questions_type=="checkbox", start_questions_type=="selectbox")

    if (!length(start_response_options) == n) {
      stop("total start_response_options not equal to total checkbox + selectbox start_questions_type"
      )
    }

  }



  # Check end question input

  if(!missing(end_questions)) {

    if (missing(end_questions_ID)) {
      end_questions_ID <- end_questions
    }

    if(missing(end_questions_type)){
      stop("Please specify end question response type")
    }

    if(!length(end_questions) == length(end_questions_type)){
      stop("Please provide one response type for each end question")
    }

    n<- sum(end_questions_type=="checkbox", end_questions_type=="selectbox")

    if (!length(end_response_options) == n) {
      stop("total end_response_options not equal to total checkbox + selectbox end_questions_type"
      )
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
        "biowell_questions should be a list with each element containing the questions for each biowell_situations. "
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
    start_questions <- "NULL"
    start_questions_type <- "NA"
    start_response_options <- NULL
    no_start <- "no_start"
  }

  if (missing(end_questions)) {
    end_questions <- "NULL"
    end_questions_type <- "NA"
    end_response_options <- NULL
    no_end <- "no_end"
  }



  #----------------------------------------------------------------------------
  # Create survey's Shiny App UI element
  #----------------------------------------------------------------------------

  # Default messages

sb <- "Please do not close the window until your responses have been submitted."
aq <- "In this survey, you must answer all questions to complete."
pc <- "These questions must be answered to proceed."
gb <- "Sorry you have not met the criteria for this survey. Please exit."

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
      $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
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
  shiny::conditionalPanel('false', shiny::textInput("client_time_zone_offset",
                                                    "Time zone offset",
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
      shiny::div(
          class = "page",
          id = "page1",
          shiny::tags$div(style = "color:# 004A86;font-size: 18px;",
                          screen_message),
          lapply(add_questions(screen_questions,
                               screen_questions_type,
                               screen_response_options,
                               all_questions = all_questions),
                 FUN=function(x){shiny::HTML(paste(x))}),
          shiny::tags$div(id= "Proceed",style="color: red;
                                     font-size: 20px;
                                     font-style: bold",pc),
          shiny::actionButton("continue", "Continue to the survey")),


      shiny::div(
        class = "page",
        id = "page2",
        shiny::tags$div(style = "color: #004A86;font-size: 18px;",
                        start_message),
        lapply(add_questions(start_questions,
                             start_questions_type,
                             start_response_options,
                             all_questions = all_questions,
                             prior_qs = screen_questions,
                             type_q = "end"),
               FUN = function(x){shiny::HTML(paste(x))}),
        shiny::textOutput("textwarning"),
        shiny::tags$head(shiny::tags$style("#textwarning{color: red;
                                     font-size: 20px;
                                     font-style: bold;}"))
        ),

      add_biowell_scale(biowell_situations, biowell_questions, all_sliders),

      shiny::div(
        class = "page",
        id = paste0("page", 3+length(biowell_situations)),
        shiny::tags$div(style="color: #004A86;font-size: 18px",end_message),
        lapply(add_questions(end_questions,
                             end_questions_type,
                             end_response_options,
                             type_q = "end",
                             prior_qs = c(screen_questions,start_questions),
                             all_questions = all_questions),
               FUN=function(x){shiny::HTML(paste(x))}),
        shiny::textOutput("textwarning2"),
        shiny::tags$head(shiny::tags$style("#textwarning2{color: red;
                                           font-size: 20px;
                                           font-style: bold;}")),
          shiny::actionButton("submit", "Submit")

        ),


      shiny::div(
        class = "page",
        id = paste0("page", 4+length(biowell_situations)),
        shiny::fluidRow(
          if(!user_report){shiny::textOutput("final_message")},
          if(user_report){
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
        shiny::tags$head(shiny::tags$style('h1 {font-size: 4vw;
                                           color:#0097AD;
                                           font-weight: bold;
                                           text-align: center;
                                           font: 1px}')),
        shiny::div(class = "square", shiny::textOutput("text"),
                   shiny::tags$head(shiny::tags$style('.square {
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

        shiny::div(
          class = "square2",
          shiny::textOutput("text1"),
          shiny::tags$head(shiny::tags$style('.square2 {
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

        shiny::htmlOutput("biowell_results2"),
        shiny::tags$style('#mydiv6{list-style-position: inside;
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
                   shiny::tags$head(shiny::tags$style('.line {
                                                      width: 30%;
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
        shiny::tags$head(shiny::tags$style('#mytable {background: white}'))

            )}
        ),
      ),
      shiny::div(class = "FINAL",
                 id = paste0("FINAL"),
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
    start1<-Sys.time()

    # If no screening questions given, hide screening page
    if(exists("no_screen")) {
      shinyjs::hide(id = "select1")
      shinyjs::hide(id = "Proceed")
    }

    # Create reactive values for controlling
    rv <- shiny::reactiveValues(page = 1)
    showNEXT<- reactiveVal(FALSE)
    showEND <- reactiveVal(FALSE)
    ENDQS <- reactiveVal(FALSE)


    showPlot <- reactiveVal(TRUE)

    shiny::observe({

      if(rv$page==1){  showPlot(TRUE)}
      if(rv$page == NUM_PAGES){showPlot(TRUE)}
      if(!rv$page==1){

        if(rv$page<=1+length(biowell_situations)){

          page_working<-NULL
          for(sit in 1:length(biowell_situations)){

            n<-length(biowell_questions[[sit]])

            page_working<-c(page_working, rep(sit,n))}

          page_working<-data.frame(page=1+page_working)
          page_working$number<-1:nrow(page_working)

          page_split<-page_working[page_working$page==rv$page,]

          must_move_IDS<-c(paste0("physical",  page_split$number,"ID"),
                           paste0("social",  page_split$number,"ID"),
                           paste0("emotional",  page_split$number,"ID"),
                           paste0("cognitive",  page_split$number,"ID"),
                           paste0("spiritual",  page_split$number,"ID"))


          input_for_this<-t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,must_move_IDS]
          if(!length(input_for_this)==0){
            if(all_sliders){showPlot(FALSE)}
            if(!all_sliders){ showPlot(TRUE)}
            if(any(!input_for_this==50)){

              input_for_this<-input_for_this[-which(!input_for_this==50)]
            }}

          if(length(input_for_this)==0){

            showPlot(TRUE)


          }

        }}

    })



    NUM_PAGES<-c(2+length(biowell_situations)+2)

    shiny::observe({
      if(NUM_PAGES-1 == rv$page){print("yep")
        shinyjs::hide("nextBtn")
        shinyjs::toggleState(id = "nextBtn", condition =  TRUE)}
      shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)
      if(!NUM_PAGES-1 == rv$page){print("NOPE")
      shinyjs::toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES+1)
      shinyjs::toggleState(id = "nextBtn", condition =  showPlot())
      shinyjs::toggleState(id = "nextBtn", condition =  !showEND())
      shinyjs::toggleState(id = "nextBtn", condition =  showNEXT())}
      shinyjs::toggleState(id = "prevBtn", condition =  !showEND())
      shinyjs::toggleState(id = "prevBtn", condition =  showNEXT())
      shinyjs::toggle(id = "FINAL", condition =  showEND())
      shinyjs::hide(selector = ".page")
      shinyjs::show(
        paste0("page", rv$page)
      )

    })

    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }

    shiny::observeEvent(input$prevBtn, navPage(-1))

    shiny::observeEvent(showEND(),

                        if(showEND()){
                          navPage(NUM_PAGES)
                        })


    output$textwarning2<- shiny::renderText("Please answer all questions.")
      shiny::observeEvent(input$continue,

                          if(rv$page ==1 && answers(names11=add_questions(screen_questions,screen_questions_type,screen_response_options,return_names=T,all_questions =all_questions ),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
                             || "" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(screen_questions,screen_questions_type,screen_response_options,return_names_text=T,all_questions =all_questions)]
                          )

                          {

                            output$textwarning<- shiny::renderText("Please answer all questions.")
                            shinyjs::show("textwarning")
                          } else {
                            screening<-add_questions(screen_questions,screen_questions_type,screen_response_options,all_questions=all_questions,return_screen=TRUE)

                            if(!is.null(screening)){
                              input_for_this<-as.data.frame(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,screening$inputname])

                              for(x in 1:nrow(input_for_this)){

                                if(input_for_this[x,] == screening$inputresponse[x]){  showEND(TRUE)}
                              }}
                            if(!showEND()){
                            navPage(1)
                            showNEXT(TRUE)
                            shinyjs::hide("textwarning")}})


      if(all_questions){
       shiny::observeEvent(input$nextBtn,

                            if(rv$page >1 && answers(names11=add_questions(start_questions,start_questions_type,start_response_options,return_names=T,all_questions =all_questions,prior_qs = screen_questions,type_q = "end" ),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
                              || "" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(start_questions,start_questions_type,start_response_options,return_names_text=T,all_questions =all_questions,prior_qs = screen_questions,type_q = "end")]
                            )

                            {


                              output$textwarning<- shiny::renderText("Please answer all questions.")
                              shinyjs::show("textwarning")
                           } else {navPage(1)
                              shinyjs::hide("textwarning")})}

    if(!all_questions){
      shiny::observeEvent(input$nextBtn,
                          navPage(1))}


    shiny::observe({

      if(rv$page == NUM_PAGES-1){
        shinyjs::hide(id = "nextBtn")}

      if(rv$page != NUM_PAGES-1){

        shinyjs::show(id = "nextBtn")}



    })


    shiny::observe({

      if(!answers(names11=add_questions(end_questions,end_questions_type,end_response_options,return_names=T,all_questions =all_questions,prior_qs = c(screen_questions,start_questions),type_q = "end" ),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
         && !"" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(end_questions,end_questions_type,end_response_options,return_names_text=T,all_questions =all_questions,prior_qs = c(screen_questions,start_questions),type_q = "end")]
      ){

        ENDQS(TRUE)}})

    shinyjs::hide(id = "textwarning2")

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {
      if(!all_questions){ENDQS(TRUE)}
      if(!ENDQS()){ shinyjs::show(id = "textwarning2")}
if(ENDQS()){


    shinyjs::hide(id = "submit")
    shinyjs::hide(id = "prevBtn")
    shinyjs::hide(id = "nextBtn")
    shinyjs::hide(id = "sidebarPanel")
    shinyjs::show(
      paste0("page", NUM_PAGES)
    )
    if(!user_report){
      output$final_message<- shiny::renderText("Thank you. Please close your browser to finish the survey.")
      shinyjs::show("final_message")
    }
    shinyjs::hide(
      paste0("page", NUM_PAGES-1)
    )



      dataframe<- t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))

      results<-NULL
      if(length(start_questions)==1){
        if(start_questions == "NULL"){start_questions<-NULL}}
      if(length(end_questions)==1){
        if(end_questions == "NULL"){end_questions<-NULL}}
      if(length(screen_questions)==1){
        if(screen_questions == "NULL"){screen_questions<-NULL}}
print("a")

      if(is.null(start_questions)){extracted_data<-NULL}
      if(is.null(end_questions)){extracted_data_end<-NULL}
      if(is.null(screen_questions)){extracted_data_screen<-NULL}
print("b")

      if(!is.null(screen_questions)){
        screenqnames<- add_questions(screen_questions,screen_questions_type,screen_response_options,return_names=T,all_questions =all_questions)
        extracted_data_screen<-sort_question_answers(screenqnames,dataframe,screen_questions_ID,screen_response_options)}


      if(!is.null(start_questions)){
        startqnames<- add_questions(start_questions,start_questions_type,start_response_options,return_names=T,all_questions =all_questions,prior_qs = screen_questions,type_q = "end" )
        extracted_data<-sort_question_answers(startqnames,dataframe,start_questions_ID,start_response_options)}
print("c")
      if(!is.null(end_questions)){
        endqnames<- add_questions(end_questions,end_questions_type,end_response_options,return_names=T,type_q = "end",prior_qs = c(screen_questions,start_questions),all_questions =all_questions)
        print(endqnames)
        print(dataframe)
        extracted_data_end<- sort_question_answers(endqnames,dataframe,end_questions_ID,end_response_options)
      }

      track<-0

      print("d")
      start_time_POSIXct<-as.POSIXct(paste(strsplit(input$clientTime,",")[[1]][1],"",strsplit(input$clientTime,", ")[[1]][2]),format="%d/%m/%Y %H:%M:%S")
      start_time_ch<-as.character(start_time_POSIXct)
      start_time<-as.character(strsplit(start_time_ch," ")[[1]][2])
      start_date<-as.character(strsplit(start_time_ch," ")[[1]][1])

      print("e")
      end1<-Sys.time()

      lengthofsurvey<-round(as.numeric(difftime(end1,start1,units="secs")),2)

      sd<-start_time_POSIXct

      sd<-as.character(strftime(sd+lubridate::seconds(lengthofsurvey)))
      print("f")
      end_date<-strsplit(sd," ")[[1]][1]
      end_time<-strsplit(sd," ")[[1]][2]
      timezone<-strsplit(paste(input$client_time_zone_char, sep = "; ")," ")[[1]][2]
      timezonelocation<-paste(input$tz_intern, sep = "; ")


      server_time<-as.character(strftime(Sys.time(), '%F %T', usetz = TRUE))

      print("g")
      for(situation in 1:length(biowell_situations)){
        print(situation)
        situationquestions<-biowell_questions[[situation]]

        for(question in 1:length(situationquestions)){
          track<-track+1
          print(question)
          row<-c(lengthofsurvey,start_date,start_time,end_date,end_time,timezone,timezonelocation,server_time,extracted_data_screen,
                 extracted_data,situation,
                 biowell_situations[situation],question,situationquestions[question],
                 dataframe[,paste0("physical",track,"ID")],100-as.numeric(dataframe[,paste0("physical",track,"ID")]),
                 dataframe[,paste0("emotional",track,"ID")],100-as.numeric(dataframe[,paste0("emotional",track,"ID")]),
                 dataframe[,paste0("cognitive",track,"ID")], 100-as.numeric(dataframe[,paste0("cognitive",track,"ID")]),
                 dataframe[,paste0("social",track,"ID")], 100-as.numeric(dataframe[,paste0("social",track,"ID")]),
                 dataframe[,paste0("spiritual",track,"ID")],100-as.numeric(dataframe[,paste0("spiritual",track,"ID")]),
                 extracted_data_end)

          results<-rbind(results,row)
        }}

print("n")
      results<-as.data.frame(results)
      if(!exists("no_start") & !exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data_screen),colnames(extracted_data),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED",colnames(extracted_data_end))}
      if(exists("no_start") & !exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data_screen),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED",colnames(extracted_data_end))}
      if(!exists("no_start") & exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data_screen),colnames(extracted_data),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED")}
      if(exists("no_start") & exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data_screen),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED")}

      results <- apply(results,2,as.character)

      results<-as.data.frame(results)

      results$mean_biowell_INVERTED<-rep(mean(as.numeric(as.matrix(results[,c("Physical_INVERTED","Emotional_INVERTED","Cognitive_INVERTED","Social_INVERTED","Spiritual_INVERTED")]))),nrow(results))
      results$mean_biowell_RAW<-100-results$mean_biowell_INVERTED

      results<-convert_data_frame(results,biowell_situations_ID,biowell_questions_ID)

      if(!curl::has_internet()) {nresponses<-length(list.files(tempdir()))
      running_average<-NA}

      if(curl::has_internet()) {
      tryCatch({
        nresponses <- nrow(rdrop2::drop_dir(Dropbox_App_folder, dtoken = token))
      }, error = function(e) {
        stop("Dropbox folder does not exist within your Dropbox App")
      })}

      filePath <- file.path(paste0(tempfile(),"_ID_",nresponses,".csv"))

      write.csv(results, filePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox

      results<-read.csv(filePath)

      if(curl::has_internet()) {

      rdrop2::drop_upload(filePath, path = Dropbox_App_folder,dtoken=token)

      rdrop2::drop_dir(dtoken = token)

      filesInfo <- rdrop2::drop_dir(Dropbox_App_folder,dtoken=token)
      filePaths <- filesInfo$path_display
      filePaths <- filePaths[grepl("BIOWELL_RUNNING_AVERAGE.csv" , filePaths)]
      if(length(filePaths)==0){
        running_average<-results$mean_biowell_INVERTED
        filePath1 <- file.path(paste0(tempdir(),"/BIOWELL_RUNNING_AVERAGE.csv"))
        write.csv(results$mean_biowell_INVERTED, filePath1, row.names = FALSE, quote = TRUE)
        rdrop2::drop_upload(filePath1, path = Dropbox_App_folder,dtoken=token)}

      if(length(filePaths)==1){
        x<-rdrop2::drop_read_csv(filePaths,dtoken=token)
        running_average<-x$x
        x$x<-(x$x+results$mean_biowell_INVERTED)/2
        filePath1 <- file.path(paste0(tempdir(),"/BIOWELL_RUNNING_AVERAGE.csv"))
        write.csv(x$x, filePath1, row.names = FALSE, quote = TRUE)
        rdrop2::drop_upload(filePath1, path = Dropbox_App_folder,dtoken=token)
      }

}
      n<-results$mean_biowell_INVERTED-running_average
      if(n<0){words<-"lower than"
      color2<-"red"}
      if(n>0){words<-"higher than"
      color2<-"green"}
      if(n==0){words<-"equal to"
      color2<-"darkorange"}

      output$text <- renderText({paste(round(results$mean_biowell_INVERTED))})
      output$text1 <- renderText({paste("100")})

      color1<-"black"
      messagex<-"NA"
      if(dplyr::between(results$mean_biowell_INVERTED,0,49)){messagex<-"negative"
      color1<-"red"}
      if(dplyr::between(results$mean_biowell_INVERTED,51,100)){messagex<-"positive"
      color1<-"green"}
      if(results$mean_biowell_INVERTED==50){messagex<-"neutral"
      color1<-"darkorange"}

      data_1<-explore_data(data=results,type="situation_question",plot=FALSE)[[2]]
      data_1<-data_1[rev(order(data_1$mean_biowell)),]

      output$biowell_results2<- shiny::renderUI({

        physical_v<-as.numeric(as.character(results[,  grep("Physical_INVERTED",colnames(results))]))

        emotional_v<-as.numeric(as.character(results[,  grep("Emotional",colnames(results))]))

        cognitive_v<-as.numeric(as.character(results[,   grep("Cognitive_INVERTED",colnames(results))]))

        social_v<-as.numeric(as.character(results[,   grep("Social_INVERTED",colnames(results))]))

        spiritual_v<-as.numeric(as.character(results[, grep("Spiritual_INVERTED",colnames(results))]))

        physicalaverage<-round(mean(physical_v,na.rm=T),2)

        emotionalaverage<-round(mean(emotional_v,na.rm=T),2)

        cognitiveaverage<-round(mean(cognitive_v,na.rm=T),2)

        socialaverage<-round(mean(social_v,na.rm=T),2)

        spiritualaverage<-round(mean(spiritual_v,na.rm=T),2)

        types<-c("Physical wellbeing","Emotional wellbeing","Cognitive wellbeing",
                 "Social wellbeing","Spiritual wellbeing")

        return(list(
          shiny::HTML("<div id='mydiv6'>", paste0('<strong>This score indicates a <span style="color: ',color1,';">',messagex,'</span> wellbeing response to biodiversity.</strong><br>',
                                                  '<br><span style="text-align: left">Your strongest responses to biodiversity were in:</span>',
                                                  '<ul style ="text-align: left;padding-left:20%"><li>',data_1[1,1],' (',round(data_1[1,3],2),'/100)</li>',
                                                  '<li>',data_1[2,1],' (',round(data_1[2,3],2),'/100)</li>',
                                                  '<li>',data_1[3,1],' (',round(data_1[3,3],2),'/100)</li></ul>',
                                                  '<br><strong>Your score is <span style="color:',color2,';">',words,'</span> the average score of participants (',round(running_average,2),') .</strong><br>',
                                                  '<br><span style="text-decoration: underline;">How is your score calculated?</span>',
                                                  '<br>Your BIO-WELL score reflects your average response to biodiversity taken across a set of human wellbeing domains.<br>','<br><strong>See the breakdown of your score below:</strong><br>',
                                                  '<table id="mytable">
 <thead>
  <tr>
   <th style="text-align:center;border: solid;padding-left:5px;padding-right:5px;border-color: black;background:#0097AD;color:white"> Category </th>
   <th style="text-align:center;border: solid;padding-left:5px;padding-right:5px;border-color: black;background:#0097AD;color:white"> Description </th>
   <th style="text-align:center;border: solid;padding-left:5px;padding-right:5px;border-color: black;background:#0097AD;color:white"> Your score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;border: solid;padding-left:5px;font-weight: bold;background:#ebfafc "> Physical wellbeing </td>
   <td style="text-align:left;border: solid;padding-left:5px"> related to the functioning of the physical body and how one feels physically, including recovery from stress. </td>
   <td style="text-align:right;border: solid;text-align:center"> ',physicalaverage,' </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left:5px; border: solid;font-weight: bold;background:#ebfafc "> Emotional wellbeing </td>
   <td style="text-align:left;border: solid;padding-left:5px;background:#EBEBEB"> the experience of positive and negative emotions and mood. </td>
   <td style="text-align:right;border: solid;text-align:center;background:#EBEBEB"> ',emotionalaverage,' </td>
  </tr>
  <tr>
   <td style="text-align:center;border: solid;padding-left:5px;font-weight: bold;background:#ebfafc "> Cognitive wellbeing </td>
   <td style="text-align:left;border: solid;padding-left:5px"> an individuals thoughts about their life and cognitive capacity to direct attention. </td>
                                              <td style="text-align:right;border: solid;text-align:center"> ',cognitiveaverage,' </td>
                                              </tr>
                                              <tr>
                                              <td style="text-align:center;border: solid;padding-left:5px;font-weight: bold;background:#ebfafc  "> Social wellbeing </td>
                                              <td style="text-align:left;border: solid;padding-left:5px;background:#EBEBEB"> how an individual perceives their connections with others. </td>
                                              <td style="text-align:right;border: solid;text-align:center;background:#EBEBEB"> ',socialaverage,' </td>
                                              </tr>
                                              <tr>
                                              <td style="text-align:center;border: solid;padding-left:5px;font-weight: bold;background:#ebfafc "> Spiritual wellbeing </td>
                                              <td style="text-align:left;border: solid;padding-left:5px"> concerned with meaning and connection to something greater than oneself. </td>
                                              <td style="text-align:right;border: solid;text-align:center"> ',spiritualaverage,' </td>
                                              </tr>
                                              </tbody>
                                              </table>',
                                                  '<br> You have completed the survey. Please close this window now.</div>'),
                      shiny::HTML("<div id='mydiv66'>", paste0("<br>","</div>")))))})

   } })


    # Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(shiny::stopApp)
  }


  shiny::shinyApp(ui, server)

}


