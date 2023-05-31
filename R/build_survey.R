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

build_survey <- function(survey_title = NULL,
                         sidepanel_message = NULL,
                         Dropbox_App_folder,
                         BW_app_path,
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

  # Check screen question input
  if(!missing(screen_questions)) {

    if (missing(screen_questions_ID)) {
      screen_questions_ID <- screen_questions
    }
    screen_questions_type <- rep("selectbox",length(screen_questions))

    if (!length(screen_response_options) == length(screen_questions)) {
      stop("total screen_response_options not equal to total checkbox + selectbox screen_questions_type"
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
      stop("total start_response_options not equal to total checkbox + selectbox start_questions_type"
      )
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
    start_questions <- NULL
    extracted_data <- NULL
  }

  if (missing(end_questions)) {
    end_questions <- NULL
    extracted_data_end <- NULL
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
          if (!is.null(start_questions)) {lapply(add_questions(start_questions,
                               start_questions_type,
                               start_response_options,
                               all_questions = all_questions,
                               prior_qs = screen_questions,
                               type_q = "end"),
                 FUN = function(x){shiny::HTML(paste(x))})},
          shiny::textOutput("textwarning"),
          shiny::tags$head(shiny::tags$style("#textwarning{color: red;
                                     font-size: 20px;
                                     font-style: bold;}"))
        ),

        add_biowell_scale(biowell_situations, biowell_questions, all_sliders),

        shiny::div(
          class = "page",
          id = paste0("page", 3+length(biowell_situations)),
          shiny::tags$div(id="end_m", style="color: #004A86;font-size: 18px",end_message),
          if (!is.null(end_questions)) {lapply(add_questions(end_questions,
                               end_questions_type,
                               end_response_options,
                               type_q = "end",
                               prior_qs = c(screen_questions,start_questions),
                               all_questions = all_questions),
                 FUN=function(x){shiny::HTML(paste(x))})},
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
            if(!user_report){shiny::p("Thank you. Please close your browser to finish the survey.")},
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
                           "100",
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

                         shiny::htmlOutput("rep"),
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
    start1 <- Sys.time()

    # If no screening questions given, hide screening page
    if(exists("no_screen")) {
      shinyjs::hide(id = "select1")
      shinyjs::hide(id = "Proceed")
    }

    # Create reactive values for controlling
    rv <- shiny::reactiveValues(page = 1)
    showNEXT<- reactiveVal(FALSE) # Only TRUE if requirements are reached - next page
    showEND <- reactiveVal(FALSE) # Screened out page
    showSUBMIT <- reactiveVal(FALSE) # Submit - to report / allow data upload


    # Code to ensure if all_slider = TRUE, can only click next if all moved
    shiny::observe({

      if (rv$page > 2 && rv$page <= 2 + length(biowell_situations)) {

        if (!all_sliders) {
          showNEXT(TRUE)
        }

        if (all_sliders) {

          page_working <- data.frame(page=2+unlist(lapply(1:length(biowell_questions),
                                                          FUN = function(x) {rep(x, length(biowell_questions[[x]]))})))
          page_working$number<-1:nrow(page_working)

          page_split<-page_working[page_working$page==rv$page,]

          must_move_IDS<-c(paste0("physical",  page_split$number,"ID"),
                           paste0("social",    page_split$number,"ID"),
                           paste0("emotional", page_split$number,"ID"),
                           paste0("cognitive", page_split$number,"ID"),
                           paste0("spiritual", page_split$number,"ID"))

          input_for_this<-t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,must_move_IDS]

          if (!length(input_for_this) == 0) {

            showNEXT(FALSE)


            if (any(!input_for_this == 50)) {
              input_for_this <- input_for_this[-which(!input_for_this == 50)]
            }}

          if (length(input_for_this) == 0) {
            showNEXT(TRUE)
          }

        }}

    })


    # Controlling the page

    NUM_PAGES <- c(2 + length(biowell_situations) + 2)
    navPage <- function(direction) {rv$page <- rv$page + direction}
    shiny::observeEvent(input$prevBtn, navPage(-1))
    shiny::observeEvent(input$nextBtn, navPage(1))

    shiny::observe({

      # Hide next button - must click submit to generate report
      if (NUM_PAGES - 1 == rv$page) {
        shinyjs::hide("nextBtn")

      }
      # Previous button active unless first page
      shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)

      # On any other page...
      if(!NUM_PAGES-1 == rv$page){
        # Turn off if we are showing the end page (screening)
        shinyjs::toggleState(id = "nextBtn", condition =  !showEND())
        if(showEND()){shinyjs::hide("prevBtn")
          }

        # Turn on if other rules met (showNEXT = TRUE)
        shinyjs::toggleState(id = "nextBtn", condition =  showNEXT())}


      shinyjs::toggle(id = "FINAL", condition =  showEND())
      shinyjs::hide(selector = ".page")
      shinyjs::show(
        paste0("page", rv$page)
      )

    })

    # Ensure next button is only hidden if on penultimate page
    shiny::observe({
      if(rv$page == NUM_PAGES-1){
        shinyjs::hide(id = "nextBtn")}
      if(rv$page != NUM_PAGES-1){
        shinyjs::show(id = "nextBtn")}
    })


    output$textwarning2<- shiny::renderText("Please answer all questions.")

    # Have the screen questions been answered?
    shiny::observeEvent(input$continue,

       if (!is.null(screen_questions)) {
         # Get screening response options
        screening<-add_questions(screen_questions,screen_questions_type,screen_response_options,all_questions=all_questions,return_screen=TRUE)
         # Get user input
         input_for_this<-as.data.frame(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,screening$inputname])

                          # For every screening q, check input is not the disallowed response
                          for (x in 1:nrow(input_for_this)) {
                            if (input_for_this[x,] == screening$inputresponse[x]) {
                              showEND(TRUE)
                              navPage(NUM_PAGES)
                            }
                          }

                          # If after all checked, met screen criteria - move to next page of survey!
                          if (!showEND()) {

                            if(!"" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,paste0("select",1:length(screen_questions))]
                            )

                            navPage(1)
                          }
                        })

    # Have the start questions been answered?
    shiny::observe({
      if(is.null(start_questions) && rv$page == 2){showNEXT(TRUE)}
      if(all_questions){

        if(!is.null(start_questions)){
          if(rv$page == 2){

            if(answers(names11=add_questions(start_questions,start_questions_type,start_response_options,return_names=T,all_questions =all_questions,prior_qs = screen_questions,type_q = "end" ),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
               || "" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(start_questions,start_questions_type,start_response_options,return_names_text=T,all_questions =all_questions,prior_qs = screen_questions,type_q = "end")]
            )

            {
              output$textwarning<- shiny::renderText("Please answer all questions.")
              shinyjs::show("textwarning")
              showNEXT(FALSE)
            } else {showNEXT(TRUE)
              shinyjs::hide("textwarning")}
          }}}

    })

    # Have the end questions been answered?
    if(!is.null(end_questions)){
      shiny::observe({

        if(!answers(names11=add_questions(end_questions,end_questions_type,end_response_options,return_names=T,all_questions =all_questions,prior_qs = c(screen_questions,start_questions),type_q = "end" ),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
           && !"" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(end_questions,end_questions_type,end_response_options,return_names_text=T,all_questions =all_questions,prior_qs = c(screen_questions,start_questions),type_q = "end")]
        ){

          showSUBMIT(TRUE)}})} else
            (showSUBMIT(TRUE))

    shinyjs::hide(id = "textwarning2")

    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {
      if(!all_questions){showSUBMIT(TRUE)}
      if(!showSUBMIT()){ shinyjs::show(id = "textwarning2")}
      if(showSUBMIT()){
        shinyjs::hide(id = "submit")
        shinyjs::hide(id = "prevBtn")
        shinyjs::hide(id = "nextBtn")
        shinyjs::hide(id = "end_m")
        shinyjs::show(
          paste0("page", NUM_PAGES)
        )

        dataframe<- t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))

        extracted_data_screen<-sort_question_answers(paste0("select",length(screen_questions)),dataframe,screen_questions_ID,screen_response_options)

        if(!is.null(start_questions)){
          startqnames<- add_questions(start_questions,
                                      start_questions_type,
                                      start_response_options,
                                      return_names = T,
                                      all_questions = all_questions,
                                      prior_qs = screen_questions,
                                      type_q = "end" )

          extracted_data<-sort_question_answers(startqnames,dataframe,start_questions_ID,start_response_options)}

        if(!is.null(end_questions)){

          extracted_data_end<- sort_question_answers(endqnames,dataframe,end_questions_ID,end_response_options)
        }

        results<-generate_results_data(start1,dataframe,biowell_questions,biowell_situations,extracted_data_screen,extracted_data_end,extracted_data,input$clientTime,input$client_time_zone_char, input$tz_intern,biowell_situations_ID,biowell_questions_ID)

        nresponses <- length(list.files(tempdir()))

        if(curl::has_internet()) {
          nresponses <- nrow(rdrop2::drop_dir(Dropbox_App_folder, dtoken = token))}

       filePath <- file.path(paste0(tempfile(),"_ID_",nresponses,".csv"))

        # Upload the file to Dropbox
        write.csv(results, filePath, row.names = FALSE, quote = TRUE)

        results <- read.csv(filePath)

        running_average <- sort_running_average(filePath,
                                                Dropbox_App_folder,
                                                token,
                                                results)

    output$text <- renderText({paste(round(results$mean_biowell_INVERTED))})
    output$rep <- shiny::renderUI({generate_report(results, running_average)})

      }})

    # Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(shiny::stopApp)
  }

  shiny::shinyApp(ui, server)

}


