#' add_biowell_scale Adds BIO-WELL scale respone to each question
#'@param biowell_situations a character string or vector, describing the
#'  environmental space setting for BIO-WELL questions.
#'@param biowell_questions a list of character strings or vectors, the questions
#'  for each separate `biowell_situations` for BIO-WELL responses.
#'@param all_sliders a logical, indicating whether all sliders must be moved at
#'  least once to continue with the survey.
#' @noRd

add_biowell_scale <-  function(biowell_situations,
                               biowell_questions,
                               all_sliders,
                               ...) {

  lst <- vector(mode = 'list', length = length(biowell_situations))

  record = 1

  names = NULL

for (x in 1:length(biowell_situations)) {

  sit_qs <- biowell_questions[[x]]
  rc <- c(record:(record + length(sit_qs) - 1))

  lst[[x]] <- shiny::div(class = "page",
                         id = paste0("page", x + 2),
                         shiny::h3(biowell_situations[x]),
                         shiny::br(),
                         shiny::tags$head(shiny::tags$style(
                         'h3 {font-style: italic;
                               color: #004A86;
                               font-size: 20px}')),
    shiny::tags$head(shiny::tags$style('h5 {font-weight: bold}')),
    shiny::tags$style(type = "text/css",
                          " .irs-from {background-color: #df4ed8 !important;}
                            .irs-min {visibility:visible !important;
                                     font-size: 15px;
                                     top: -12px !important;}
                            .irs-max {visibility:visible !important;
                                     font-size: 15px;
                                     color: white !important;
                                     top: -12px !important}"),
    shiny::tags$script(shiny::HTML(custom_slider_text(rc))),
    lapply(sapply(rc, list), sliders, biowell_questions = biowell_questions),
    if(all_sliders) {
      shiny::tags$div(style = "color: red",
                    "All sliders must be moved to continue to the next page.")},
  )

  record <- record + length(sit_qs)

}

  return(lst)

}


#' sliders Input BIO-WELL formatted sliders into survey.
#' @param sid a numerical value, unique slider ID.
#'@param biowell_questions a list of character strings or vectors, the questions
#'  for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd

sliders <- function(sid, biowell_questions, ...) {

  all_qs <- unlist(biowell_questions)

  list(shiny::h4(all_qs[sid]),
       shiny::tags$head(shiny::tags$style('h4 {font-weight: bold;
                                               font-style: italic;
                                               color: #0097AD}')),
    shiny::tags$style(type = "text/css",

    paste0(".","sld",sid,
    " .irs-handle{ background: white !important;
                  size: 1px !important;
                  border-radius: 4px !important;
                  border: 2px solid #000000 !important;}
          ",".","sld",sid,
    " .irs-line{background: #a9a9a9 !important;
                border-radius:0px !important;}
          ",".","sld",sid,
    " .irs-grid-pol:nth-of-type(-n+1) {
                background: linear-gradient(to right, black 50%, transparent 50%);
                height: 25px;
                top: -30px;
                width: 20px;
                left: -20px !important }
          ",".","sld",sid,
    " .irs-grid-pol:nth-of-type(n+2) {background: white}
          ",".","sld",sid,
    " .irs-grid-pol:nth-of-type(n+41) {
                background:linear-gradient(to right, transparent 50%, black 50%);
                height: 25px;
                top: -30px;
                width: 20px;}
          ",".","sld",sid,
    " .irs-grid-text{visibility: hidden !important;color: white;}
          ",".","sld",sid,
    " .irs-bar {visibility:hidden}
          ",".","sld",sid,
    " .irs-min {visibility:visible !important;
                font-size: 15px;
                color: black !important;
                background: white !important;
                text-align:left !important}
          ",".","sld",sid,
    " .irs-max {visibility:visible !important;
                font-size: 15px;
                color: black !important;
                background: white !important;
                text-align:right !important}")),
    shinyWidgets::chooseSliderSkin("Big", color = "#112446"),
    shiny::div(class = paste0("sld", sid),
               style = "height: 70px;",
               shiny::sliderInput(paste0("physical", sid, "ID"), step = 0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50, width = 800)),
    shiny::div(class = paste0("sld", sid),
               style = "height: 70px;",
               shiny::sliderInput(paste0("emotional", sid, "ID"), step = 0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50, width = 800)),
    shiny::div(class = paste0("sld", sid),
               style = "height: 70px;",
               shiny::sliderInput(paste0("cognitive", sid, "ID"), step = 0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50, width = 800)),
    shiny::div(class = paste0("sld", sid),
               style = "height: 70px;",
               shiny::sliderInput(paste0("social", sid, "ID"), step = 0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50, width = 800)),
    shiny::div(class = paste0("sld", sid),
               style = "height: 70px;",
               shiny::sliderInput(paste0("spiritual", sid, "ID"), step = 0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50, width = 800)),
    shiny::br(),
    shiny::br()
    )
}

#' custom_slider_text Generates the custom labels for BIO-WELL slider scales.
#' @param sid a numerical value, the unique IDs for each BIO-WELL slider.
#' @noRd

custom_slider_text<-function(sid1){

  text <- paste0("$(document).ready(function() {
  /**
    Custom slider labels
  **/

  function returnLabels(value) {
    // remove label of selected
    $('.physical",sid1,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); //

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Physically relaxed';
    }else{
      return '<br> Physically tense &#8205';
    }
  }

  var someID = $('#physical",sid1,"ID').ionRangeSlider({ // enter shiny slider
  prettify: returnLabels,
  force_edges: true,
  grid: false,
  hide_from_to: true
});
});
            $(document).ready(function() {
  /**
    Custom slider labels
  **/


  function returnLabels(value) {
    // remove label of selected
    $('.emotional",sid1,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); //

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Joyful';
    }else{
      return '<br> Sad &#8205';
    }
  }

  var someID = $('#emotional",sid1,"ID').ionRangeSlider({ // enter shiny slider
  prettify: returnLabels,
  force_edges: true,
  grid: false,
  hide_from_to: true
});
});


            $(document).ready(function() {
  /**
    Custom slider labels
  **/


  function returnLabels(value) {
    // remove label of selected
    $('.cognitive",sid1,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); //

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Clear minded';
    }else{
      return '<br> Muddled &#8205';
    }
  }

  var someID = $('#cognitive",sid1,"ID').ionRangeSlider({ // enter shiny slider
  prettify: returnLabels,
  force_edges: true,
  grid: false,
  hide_from_to: true
});
});



            $(document).ready(function() {
  /**
    Custom slider labels
  **/


  function returnLabels(value) {
    // remove label of selected
    $('.social",sid1,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); //

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Open to people';
    }else{
      return '<br> Closed to people &#8205';
    }
  }

  var someID = $('#social",sid1,"ID').ionRangeSlider({ // enter shiny slider
  prettify: returnLabels,
  force_edges: true,
  grid: false,
  hide_from_to: true
});
});



            $(document).ready(function() {
  /**
    Custom slider labels
  **/


  function returnLabels(value) {
    // remove label of selected
    $('.spiritual",sid1,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); //

    if (value === 0){ // enter your lowest slider value here
      return '&#8205 Part of something <br> &#8205 bigger than myself';
    }else{
      return 'Not part of something <br> bigger than myself &#8205';
    }
  }

  var someID = $('#spiritual",sid1,"ID').ionRangeSlider({ // enter shiny slider
prettify: returnLabels,
force_edges: true,
grid: false,
hide_from_to: true
});
});")

  return(text)
}

#'add_questions Adds screen, start or end question response boxes into survey
#'@param questions a character string or vector, the questions to associate
#'  response boxes with.
#'@param type optional; a character string or vector, the type of response for
#'  each question. Must be length of `end_questions`. One of; `text`,
#'  `checkbox`, `selectbox`, `likert_five` and `likert_seven`. See details for
#'  more information.
#'@param drop_down   optional; a list of character strings or vectors, the
#'  options to offer for each `end_questions` of type `checkbox` or `selectbox`.
#'@param return_names a logical, indicating whether to return the unique ID
#'  names for each response box.
#'@param return_screen a logical, indicating whether to return the responses to
#'  screen participants by.
#'@param type_q a character string, the position of questions being added. One
#'  of `start` or `end`.
#'@param prior_qs optional; a character string, the screen and/or start
#'  questions of the survey. Required if `type_q = "end"`.
#'@noRd

add_questions <- function(questions,
                          type,
                          drop_down,
                          return_names = FALSE,
                          return_screen = FALSE,
                          type_q = "start",
                          prior_qs = NULL,
                          ...) {

  if (!is.null(questions)) {

    # Check response type matches avalable options
    match.arg(type, choices = c("text",
                                "selectbox",
                                "checkbox",
                                "likert_five",
                                "likert_seven"), several.ok = T)


    lst <- vector(mode = 'list', length = length(questions))
    dropcount <- 0
    names <- NULL
    xx <- 1

    # If there are questions prior to this lot, add the numbers accordingly
    if (type_q == "end") {
      xx <- length(prior_qs) + 1
    }

    sid <- length(questions)

    if (type_q == "end") {
      sid <- length(prior_qs) + length(questions)
    }

    screen_out <- NULL

    for (sq in xx:sid) {

      sq2 <- sq

      if(type_q=="end") {
        sq2 <- sq - length(prior_qs)
      }

      q1 <- questions[sq2]
      answertype <- type[sq2]


      if(answertype == "text") {

        lst[[sq2]] <- shiny::textInput(paste0("text", sq),
                                       shiny::HTML(paste0("<br>", shiny::h5(q1))))

        names <- c(names, paste0("text", sq)) # Record ID name for results
      }



      if (answertype == "checkbox") {

        dropcount <- dropcount + 1
        opt_sel <- drop_down[[dropcount]]

        if (!length(grep(":SCREEN", opt_sel)) == 0) {

          for (scr in 1:length(grep(":SCREEN", opt_sel))) {

            n <- grep(":SCREEN", opt_sel)

            df <- data.frame(inputname = paste0("checkbox", sq),
                             inputresponse = gsub(":SCREEN", "", opt_sel)[n])

            screen_out <- rbind(screen_out, df)
          }
        }

        opt_sel <- gsub(":SCREEN", "", opt_sel)

      lst[[sq2]] <- shiny::checkboxGroupInput(paste0("checkbox", sq),
                                              shiny::HTML(paste0("<br>",
                                                                 shiny::h5(q1))),
                                              choices = opt_sel)
      names <- c(names, paste0("checkbox", sq))

      }




      if (answertype == "selectbox") {

        dropcount <- dropcount + 1

        opt_sel <- drop_down[[dropcount]]

        if(!length(grep(":SCREEN", opt_sel))==0) {


          for(scr in 1:length(grep(":SCREEN", opt_sel))) {

            n <- grep(":SCREEN", opt_sel)

            df <- data.frame(inputname = paste0("select", sq),
                             inputresponse = gsub(":SCREEN", "", opt_sel)[n])

            screen_out <- rbind(screen_out, df)

          }
        }

        opt_sel <- gsub(":SCREEN", "", opt_sel)

      lst[[sq2]]<- shiny::selectizeInput(paste0("select",sq),
                                         label =  shiny::HTML(paste0("<br>",
                                                                     shiny::h5(q1))),
                                         choices = c("", opt_sel),
                                         options = list(
                                         placeholder = 'Choose answer',
                                         onInitialize = I('function() {
                                                          this.setValue(""); }')
                                         ))

      names <- c(names, paste0("select", sq))

      }


      if(answertype == "likert_seven") {

      lst[[sq2]] <- shiny::div(
        shiny::tags$style(type = "text/css",
        paste0(".likert_sev",sq," .irs {height:70px}
                .likert_sev",sq," .irs-handle{background: white !important;
                                              size: 1px !important;
                                              border-radius: 4px !important;
                                              border: 2px solid #000000 !important;}
                .likert_sev",sq," .irs-grid-pol{height: 30px;
                                                top: -30px;
                                                background: black!important;
                                                width:5px}
                .likert_sev",sq," .irs-line{background: #a9a9a9 !important;
                                            border-radius:0px !important;}
                .likert_sev",sq," .irs-grid-pol.small{color:white;
                                                      visibility: hidden !important}
                .likert_sev",sq," .irs-bar{visibility: hidden !important}
                .likert_sev",sq," .irs-grid-text {font-size: 13px;
                                                  color:black!important;
                                                  visibility:visible !important;
                                                  top: 40% }
                .likert_sev",sq," .irs-single{visibility: hidden !important;}
                .likert_sev",sq," .irs-from{visibility: hidden !important;}
                .likert_sev",sq," .irs-to{visibility: hidden !important;}
                .likert_sev",sq," .irs-min {visibility:visible !important;
                                            font-size: 15px;
                                            color: transparent !important;
                                            background: transparent !important;
                                            text-align: left !important}
                .likert_sev",sq," .irs-max {visibility:visible !important;
                                            font-size: 15px;
                                            color: transparent !important;
                                            background: transparent !important;
                                            text-align: right !important}")),
        shiny::div(class = paste0("likert_sev", sq),
            shiny::HTML(paste0(
               '<div class = "form-group shiny-input-container"
                     style = "width:500px;">
                <label class = "control-label"
                       id = ".likert_sev', sq, '-label"
                       for = ".likert_sev',sq,'"><br>',shiny::h5(q1),'</label>
                <input class = "js-range-slider"
                       id = "likert_sev',sq,'"
                       data-skin = "shiny"
                       data-min = "1"
                       data-max = "7"
                       data-from = "3"
                       data-step = "1"
                       data-grid = "true"
                       data-grid-num = "10"
                       data-grid-snap = "false"
                       data-prettify-separator = ","
                       data-prettify-enabled = "true"
                       data-keyboard = "true"
                       data-data-type = "number"
                       data-type = "single"
                       data-values = "Strongly&lt;br&gt;&lt;br&gt; agree,
                                      Agree,
                                      Somewhat&lt;br&gt;&lt;br&gt; agree,
                                      Neither agree&lt;br&gt;&lt;br&gt; nor disagree,
                                      Somewhat&lt;br&gt;&lt;br&gt; disagree,
                                      Disagree,
                                      Strongly&lt;br&gt;&lt;br&gt; disagree"
                       data-drag-interval = "false"/> </div>'))))

      names <- c(names, paste0("likert_sev", sq))

      }


      if(answertype == "likert_five"){


        lst[[sq2]] <- shiny::div(
          shiny::tags$style(type = "text/css",
          paste0(".likert_five",sq," .irs {height:70px;}
                  .likert_five",sq," .irs-handle{background: white !important;
                                                 size: 1px !important;
                                                 border-radius: 4px !important;
                                                 border: 2px solid #000000 !important;}
                  .likert_five",sq," .irs-grid-pol{height: 30px;
                                                   top: -30px;
                                                   background: black!important;
                                                   width:5px}
                  .likert_five",sq," .irs-line{background: #a9a9a9 !important;
                                               border-radius: 0px !important;}
                  .likert_five",sq," .irs-grid-pol.small{color:white;
                                                         visibility:hidden !important}
                  .likert_five",sq," .irs-bar {visibility:hidden !important}
                  .likert_five",sq," .irs-grid-text {font-size: 13px;
                                                     color:black!important;
                                                     visibility:visible !important;
                                                     top: 40% }
                  .likert_five",sq," .irs-single{visibility:hidden !important;}
                  .likert_five",sq," .irs-from{visibility:hidden !important;}
                  .likert_five",sq," .irs-to{visibility:hidden !important;}
                  .likert_five",sq," .irs-min {visibility:visible !important;
                                               font-size: 15px;
                                               color: transparent !important;
                                               background: transparent !important;
                                               text-align:left !important}
                  .likert_five",sq," .irs-max {visibility:visible !important;
                                               font-size: 15px;
                                               color: transparent !important;
                                               background: transparent !important;
                                               text-align:right !important}")),
          shiny::div(class = paste0("likert_five", sq),
                     shiny::HTML(paste0(
                     '<div class = "form-group shiny-input-container"
                           style = "width:500px;">
                      <label class = "control-label"
                             id = ".likert_five',sq,'-label"
                             for = ".likert_five',sq,'"><br>',shiny::h5(q1),'</label>
                      <input class = "js-range-slider"
                             id = "likert_five',sq,'"
                             data-skin = "shiny"
                             data-min = "1"
                             data-max = "5"
                             data-from = "2"
                             data-step = "1"
                             data-grid = "true"
                             data-grid-num = "10"
                             data-grid-snap = "false"
                             data-prettify-separator = ","
                             data-prettify-enabled = "true"
                             data-keyboard = "true"
                             data-data-type = "number"
                             data-type = "single"
                             data-values = "Strongly&lt;br&gt;&lt;br&gt; agree,
                                            Agree,
                                            Neither agree&lt;br&gt;&lt;br&gt; nor disagree,
                                            Disagree,
                                            Strongly&lt;br&gt;&lt;br&gt; disagree"
                             data-drag-interval = "false"/></div>'))))

        names <- c(names, paste0("likert_five", sq))
      }
    }


    if(return_names) {
      return(names)
    }

    if (return_screen) {
      return(screen_out)
    }

    return(lst)
  }

}


#'answers Checks whether questions have been answered by participants
#'@param n1 a character string or vector, the response IDs for the questions on
#'  current page.
#'@param n2 a character string or vector, the column names present in shiny
#'  input.
#'@noRd

answers <- function(n1, n2) {
  # Check whether input contains the columns for answers required
  cols_present <- sapply(n1, grepl, n2)
  cols_present <- rbind(cols_present, cols_present)
  x <- colnames(cols_present)[colSums(cols_present) == 0]

  if (!length(x) == 0) {
    return(TRUE)
  }

  if (length(x) == 0) {
    return(FALSE)
  }
}


#'sort_qa Organises responses start, screen or end questions into results data
#'frame.
#'@param qnames a character string or vector, the response box IDs for the
#'  questions
#'@param response_df a data frame, the shiny input by participants.
#'@param qs  a character string or vector, the IDs for questions to match
#'  responses with.
#'@param dropdownopt the available options for the questions if of response type
#'  `selectbox` or `checkbox`.
#'@noRd

sort_qa <- function(qnames, response_df, qs, dropdownopt) {

  checkno <- 0

  # Get the names of checkbox as these have multiple options to process
  checkstartqnames <- qnames[grepl("checkbox", qnames)]
  checkqs <- qs[grepl("checkbox", qnames)]

  # Get the names of ones where answer is one of options
  option_qnames <- qnames[!grepl("text", qnames)]

  option_qnames <- option_qnames[!grepl("likert", option_qnames)]

  # Get the response options for each checkbox q
  checkopts <- dropdownopt[grepl("checkbox", option_qnames)]

  for (x in 1:length(qnames)) {

    nameof <- qnames[x]

    if (grepl("checkbox", nameof)) {

      checkno <- checkno + 1 # Running tally of how many checkbox

      if(any(grepl(nameof, colnames(response_df)))) {

        # Which checkbox answer selected
        answers <- response_df[, grepl(nameof, colnames(response_df))]

        # Of the checkbox answers possible, which have they selected
        answernumbs <- which(checkopts[[checkno]] %in% answers)

        # What would be position of all possible answers
        optionnumbs<-c(1:length(checkopts[[checkno]]))

        # Turn into a logical TRUE FALSE for each possible answer
        answers_final<-optionnumbs %in% answernumbs

        # Paste checkbox ID with each answer choices
        chkcols <- paste0(checkqs[checkno], ":", checkopts[[checkno]])

        # Create data frame of logicals and add colnames
        answers_final<-t(data.frame(answers_final))
        colnames(answers_final)<-chkcols

      }

      if(!any(grepl(nameof,colnames(response_df)))){

        # If no checkbox answer given, rep FALSE for all choices
        answers_final <- rep(FALSE, length(checkopts[[checkno]]))
        answers_final <- t(data.frame(answers_final))

        # Create and set column names using ID and checkbox options
        chkcols <- paste0(checkqs[checkno], ":", checkopts[[checkno]])
        colnames(answers_final)<-chkcols
      }

      # If this is the first answer, create formatted
      if(x == 1) {
        formatted_data <- answers_final
      }

      # If formatted already exists, add this column
      if (!x == 1) {
        formatted_data <- cbind(formatted_data, answers_final)
      }
    }

    if(!grepl("checkbox", nameof)) {

      if(any(grepl(nameof, colnames(response_df)))) {

        # Get response for this Q + make data frame
        answers<-response_df[,grepl(nameof,colnames(response_df))]

        answers_final<-t(data.frame(answers))

        # If likert five scale, convert no. to words
        if(grepl("likert_five", nameof)) {

          n <- as.numeric(answers_final[1, 1]) + 1 # Add one as goes from zero

          answers_final <- c("Strongly Agree",
                             "Agree",
                             "Neither agree nor disagree",
                             "Disagree",
                             "Strongly disagree")[n]

          answers_final <- as.data.frame(answers_final)

        }

        # If likert seven scale, convert no. to words
        if (grepl("likert_sev", nameof)) {

          n <- as.numeric(answers_final[1, 1]) + 1 # Add one as goes from zero

          answers_final <- c("Strongly Agree",
                             "Agree",
                             "Somewhat agree",
                             "Neither agree nor disagree",
                             "Somewhat disagree",
                             "Disagree",
                             "Strongly disagree")[n]

          answers_final <- as.data.frame(answers_final)
        }

        # Set colname of answer as the Q ID
        colnames(answers_final) <- qs[x]
      }

      if (!any(grepl(nameof, colnames(response_df)))) {
        # If not answered return NA in the column
        answers <- NA
        answers_final <- t(data.frame(answers))
        colnames(answers_final) <- qs[x]
      }


      # If this is the first answer, create formatted
      if(x == 1) {
        formatted_data <- answers_final
      }

      # If formatted already exists, add this column
      if (!x == 1) {
        formatted_data <- cbind(formatted_data, answers_final)
      }

    }
  }

  return(formatted_data)
}




#'generate_report Generate HTML report interpreting participants BIO-WELL
#'scores.
#'@param results a data frame, the BIO-WELL survey responses from the current
#'  participant.
#'@param running_average a numeric value, the running average BIO-WELL score of
#'  all participants so far.
#'@noRd

generate_report <- function(results, running_average) {

  # Get the difference between user score and running average
  n <- results$mean_biowell_INVERTED - running_average
  ra <- round(running_average, 2)
  # Default colours in case running_average = NA (no internet )
  color2 <- "black"
  words <- "NA"

  # Depending on difference from average set words and colours for report
  if (!is.na(n)) {

    if (n < 0) {
      words <- "lower than"
      color2 <- "red"
    }

    if (n > 0) {
      words <- "higher than"
      color2 <- "green"
    }

    if(n==0) {
      words <- "equal to"
      color2 <- "darkorange"
    }
  }


  # Default colours in case running_average = NA (no internet )
  color1 <- "black"
  messagex <- "NA"
  # Depending on value interpret BIO-WELL score colour and text

   if (!is.na(n)) {
    if (dplyr::between(results$mean_biowell_INVERTED, 0, 49)) {
      messagex <- "negative"
      color1 <- "red"
    }
    if (dplyr::between(results$mean_biowell_INVERTED, 51, 100)) {
      messagex <- "positive"
      color1 <- "green"
    }
    if (results$mean_biowell_INVERTED == 50) {
      messagex <- "neutral"
      color1 <- "darkorange"
    }
  }

  # Get the average BIO-WELL for each question
  bw <- explore_data(data = results, plot = FALSE)[[2]]

  # Reorder to highest scores are first (will extract the top 3)
  bw <- bw[rev(order(bw$mean_biowell)),]
  bw$mean_biowell <- round(bw$mean_biowell, 2)

  # Extracted the inverted for each domain
  physical_v <- results[,  grep("Physical_INVERTED", colnames(results))]

  emotional_v <- results[,  grep("Emotional_INVERTED", colnames(results))]

  cognitive_v <- results[,   grep("Cognitive_INVERTED", colnames(results))]

  social_v <- results[,   grep("Social_INVERTED", colnames(results))]

  spiritual_v <- results[, grep("Spiritual_INVERTED", colnames(results))]

  # Take the average BW for each domain
  phys_avg <- round(mean(as.numeric(as.character(physical_v)), na.rm = T), 2)

  emo_avg<-round(mean(as.numeric(as.character(emotional_v)),na.rm=T),2)

  cog_avg<-round(mean(as.numeric(as.character(cognitive_v)),na.rm=T),2)

  soc_avg<-round(mean(as.numeric(as.character(social_v)),na.rm=T),2)

  spi_avg<-round(mean(as.numeric(as.character(spiritual_v)),na.rm=T),2)

  # List the types for the table
  types <- c("Physical wellbeing",
             "Emotional wellbeing",
             "Cognitive wellbeing",
             "Social wellbeing",
             "Spiritual wellbeing")

  # Report text blocks
  tx1 <- "This score indicates a "
  tx2 <- " wellbeing response to biodiversity."
  tx3 <- "Your strongest responses to biodiversity were in:"
  tx4 <- "Your score is "
  tx5 <- " the average score of participants ("
  tx6 <- "How is your score calculated?"
  tx7 <- "Your BIO-WELL score reflects your average response to biodiversity taken across a set of human wellbeing domains."
  tx8 <- "See the breakdown of your score below:"
  tx9 <- " You have completed the survey. Please close this window now."

  # Table style
  tab_head <- "text-align:center;border: solid;padding-left:5px;padding-right:5px;border-color: black;background:#0097AD;color:white"
  tab_side <- "text-align:center;border: solid;padding-left:5px;font-weight: bold;background:#ebfafc"
  tab_desc <- "text-align:left;border: solid;padding-left:5px"
  tab_avg <- "text-align:right;border: solid;text-align:center"

  # Wellbeing domain descriptions
  phs <- " related to the functioning of the physical body and how one feels physically, including recovery from stress. "
  emo <- " the experience of positive and negative emotions and mood. "
  cog <- " an individuals thoughts about their life and cognitive capacity to direct attention. "
  soc <- " how an individual perceives their connections with others. "
  spi <- " concerned with meaning and connection to something greater than oneself. "


  return(list(
    shiny::HTML("<div id='mydiv6'>",
    paste0('<strong>', tx1, '<span style="color: ', color1, ';">', messagex,
           '</span>', tx2,'</strong><br>',
           '<br><span style = "text-align: left">',tx3,'</span>',
           '<ul style ="text-align: left; padding-left:20%"
             ><li>',bw[1,3],' (',bw[1,4],'/100)</li>',
             '<li>',bw[2,3],' (',bw[2,4],'/100)</li>',
             '<li>',bw[3,3],' (',bw[3,4],'/100)</li></ul>',
           '<br><strong>', tx4,
           '<span style="color:', color2, ';">', words, '</span>', tx5, ra, ') .
                </strong><br>',
           '<br><span style="text-decoration: underline;">', tx6, '</span>',
           '<br>', tx7, '<br>','<br><strong>',tx8,'</strong><br>',
           '<table id="mytable">
 <thead>
  <tr>
   <th style="',tab_head,'"> Category </th>
   <th style="',tab_head,'"> Description </th>
   <th style="',tab_head,'"> Your score </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="', tab_side,'"> Physical wellbeing </td>
   <td style="', tab_desc,'">', phs,'</td>
   <td style="', tab_avg,'"> ', phys_avg,' </td>
  </tr>
  <tr>
   <td style="', tab_side,'"> Emotional wellbeing </td>
   <td style="', tab_desc,';background:#EBEBEB">', emo, '</td>
   <td style="', tab_avg,';background:#EBEBEB"> ', emo_avg, ' </td>
  </tr>
  <tr>
   <td style="', tab_side,'"> Cognitive wellbeing </td>
   <td style="', tab_desc,'">', cog, '</td>
   <td style="', tab_avg,'"> ', cog_avg,' </td>
  </tr>
  <tr>
   <td style="', tab_side,'"> Social wellbeing </td>
   <td style="', tab_desc,';background:#EBEBEB">', soc,'</td>
   <td style="', tab_avg,';background:#EBEBEB"> ', soc_avg,' </td>
  </tr>
  <tr>
   <td style="', tab_side,'"> Spiritual wellbeing </td>
   <td style="', tab_desc,'">', spi,'</td>
   <td style="', tab_avg,'"> ', spi_avg,' </td>
  </tr>
  </tbody>
  </table>',
  '<br>', tx9,'</div>'),
  shiny::HTML("<div id='mydiv66'>", paste0("<br>","</div>")))))}


#'sort_running_average Extracts the current running average BIO-WELL score of
#'all participants.
#'@param filePath a character string, the path to the saved participant response
#'  data frame.
#'@param Dropbox_App_folder path to folder in Dropbox App to save survey
#'  responses to. Defaults to the root directory.
#'@param token an activated, refreshable Dropbox access token.
#'@param results a data frame, the BIO-WELL survey responses from the current
#'  participant.
#'@noRd


sort_running_average <- function(filePath,
                                 Dropbox_App_folder,
                                 token,
                                 results) {

  if(curl::has_internet()) {

    rdrop2::drop_upload(filePath, path = Dropbox_App_folder, dtoken = token)

    rdrop2::drop_dir(dtoken = token)

    # Get file names in Dropbox folder
    filesInfo <- rdrop2::drop_dir(Dropbox_App_folder, dtoken = token)
    filePaths <- filesInfo$path_display

    # See if the running average file is present
    filePaths <- filePaths[grepl("BIOWELL_RUNNING_AVERAGE.csv" , filePaths)]

    # If not present, this is first participant response, so create running avg
    if (length(filePaths) == 0) {

      running_average <- results$mean_biowell_INVERTED

      filePath1 <- file.path(paste0(tempdir(), "/BIOWELL_RUNNING_AVERAGE.csv"))

      # Write to tempdir to then upload
      write.csv(results$mean_biowell_INVERTED,
                filePath1,
                row.names = FALSE,
                quote = TRUE)

      rdrop2::drop_upload(filePath1, path = Dropbox_App_folder, dtoken = token)
    }

    # If running avg file present then add this response to the average

    if (length(filePaths) == 1) {

      x <- rdrop2::drop_read_csv(filePaths, dtoken = token)
      running_average <- x$x
      x$x <- (x$x + results$mean_biowell_INVERTED) / 2
      filePath1 <- file.path(paste0(tempdir(), "/BIOWELL_RUNNING_AVERAGE.csv"))

      write.csv(x$x, filePath1, row.names = FALSE, quote = TRUE)

      rdrop2::drop_upload(filePath1, path = Dropbox_App_folder, dtoken = token)
    }
  }

  # If no internet then return NA for running average
  if (!curl::has_internet()) {
    running_average <- NA
  }

  return(running_average)
}


#' get_d Converts Shiny input into data frame
#'@param input the Shiny input object.
#' @noRd

get_d <- function(input) {
  df <- t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))
  return(df)
}


#'generate_results_data Generates processed data frame of participants responses
#'@param start1 an object of class POSIXct, the system time when participants
#'  started the survey.
#'@param df a data frame, the Shiny input of participants responses.
#'@param extracted_data_screen a data frame, the processed screening questions
#'  responses.
#'@param extracted_data_end a data frame, the processed end questions responses.
#'@param extracted_data a data frame, the processed start questions responses.
#'@param input_client a character string, the local time when participants began
#'  the survey.
#'@param input_tz a character string, the local time zone code of participants
#'  completing the survey.
#'@param input_tz_ch a character string, the location of the participant's local
#'  time zone.
#'@param biowell_situations_ID a character string or vector the length of
#'  biowell_situation, IDs for each bio-well situation in output response data
#'  frame.
#'@param biowell_questions_ID a character string or vector the length of
#'  biowell_questions, IDs for bio-well questions in output response data frame.
#'@noRd

generate_results_data<-function(start1,
                                df,
                                extracted_data_screen,
                                extracted_data_end,
                                extracted_data,
                                input_client ,
                                input_tz,
                                input_tz_ch,
                                biowell_situations_ID,
                                biowell_questions_ID ){

  ### Sort time and dates for results

  # Extract time and date from input
  start_time_POSIXct<-as.POSIXct(paste(strsplit(input_client,";")[[1]][1],
                                       "",
                                       strsplit(input_client,"; ")[[1]][2]),
                                 format="%d/%m/%Y %H:%M:%S")

  # Character of date and time
  start_time_ch<-as.character(start_time_POSIXct)
  start_time<-as.character(strsplit(start_time_ch," ")[[1]][2])
  start_date<-as.character(strsplit(start_time_ch," ")[[1]][1])

  # Get system time at end of survey
  end1<-Sys.time()

  # Get duration from system time at start (start1) in seconds
  duration <- round(as.numeric(difftime(end1, start1, units = "secs")), 2)

  # Get date/time object and add duration to it
  st <- start_time_POSIXct
  st <- as.character(strftime(st + lubridate::seconds(duration)))

  # Extract end date and time from this
  end_date <- strsplit(st, " ")[[1]][1]
  end_time <- strsplit(st, " ")[[1]][2]

  # Time zone to character
  timezone <- strsplit(paste(input_tz, sep = "; "), " ")[[1]][2]
  timezonelocation <- paste(input_tz_ch, sep = "; ")

  server_time<-as.character(strftime(Sys.time(), '%F %T', usetz = TRUE))

  time_df <- data.frame(survey_duration = duration,
                        start_date = start_date,
                        start_time = start_time,
                        end_date = end_date,
                        end_time = end_time,
                        timezone = timezone,
                        timezone_location = timezonelocation,
                        server_submit_time = server_time
                        )


  ### Sort BIOWELL RESULTS
  track<-0

  results<-NULL

  for(sit in 1:length(biowell_situations_ID)){

    # Get questions for the situation
    sit_qs <- biowell_questions_ID[[sit]]

    for(question in 1:length(sit_qs)){

      # Record which Q done
      track <- track + 1

      colnames <- c("Physical_RAW",
                    "Physical_INVERTED",
                    "Emotional_RAW",
                    "Emotional_INVERTED",
                    "Cognitive_RAW",
                    "Cognitive_INVERTED",
                    "Social_RAW",
                    "Social_INVERTED",
                    "Spiritual_RAW",
                    "Spiritual_INVERTED")

      colnames <- paste0(biowell_situations_ID[sit],"_",sit_qs[question],"_",colnames)

      col<-c(df[,paste0("physical",track,"ID")],
             100-as.numeric(df[,paste0("physical",track,"ID")]),
             df[,paste0("emotional",track,"ID")],
             100-as.numeric(df[,paste0("emotional",track,"ID")]),
             df[,paste0("cognitive",track,"ID")],
             100-as.numeric(df[,paste0("cognitive",track,"ID")]),
             df[,paste0("social",track,"ID")],
             100-as.numeric(df[,paste0("social",track,"ID")]),
             df[,paste0("spiritual",track,"ID")],
             100-as.numeric(df[,paste0("spiritual",track,"ID")]))

      res<- t(as.data.frame(col))
      colnames(res)<-colnames
      results<-cbind(results,res)
    }}


  results <- as.data.frame(results)

  final <- time_df

  if(!is.null(extracted_data_screen)){

    final<-cbind(final,extracted_data_screen)
  }

  if(!is.null(extracted_data)){

    final<-cbind(final,extracted_data)
  }

  final<-cbind(final,results)

  if(!is.null(extracted_data_end)){

    final<-cbind(final,extracted_data_end)
  }

  #final <- apply(final,2,as.character)

  final<- as.data.frame(final)

  n <- grepl("INVERTED", colnames(final))

  final$mean_biowell_INVERTED <- mean(as.numeric(as.matrix(final[, n])))
  final$mean_biowell_RAW <- 100 - final$mean_biowell_INVERTED

  return(final)
}

