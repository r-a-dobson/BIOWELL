#'Create shiny survey with BIOWELL scale
#'
#'Function to generate custom shiny survey with BIOWELL scale
#'
#'@param survey_title a character string, the title of the survey to display on each page.
#'@param biowell_situations a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param biowell_questions a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#'@param start_message a character string, a message to display to participants at the start of the survey.
#'@param start_questions optional; a character string or vector, the questions to ask participants at the start of the survey.
#'@param start_questions_type optional; a character string or vector, the type of response for each start question. Must be length of `start_questions`. One of; `text`, `checkbox` or `selectbox`. See details for information.
#'@param start_drop_down_options optional; a list of character strings or vectors, the options to offer for each `start_questions` of type `checkbox` or `selectbox`.
#'@param end_message a character string, a message to display to participants at the end of the survey.
#'@param end_questions optional; a character string or vector, the questions to ask participants at the end of the survey.
#'@param end_questions_type optional; a character string or vector, the type of response for each end question. Must be length of `end_questions`. One of; `text`, `checkbox` or `selectbox`. See details for information.
#'@param end_drop_down_options optional; a list of character strings or vectors, the options to offer for each `end_questions` of type `checkbox` or `selectbox`.
#'@param organisation_website a character string, the web address for the organisation running the survey.
#'@param organisation a character string, the name of the organisation running the survey.
#'@param drop_box_folder path to folder in Dropbox to save responses to. Defaults to the root directory.
#'@details df
#'@returns runs a customised shiny survey app or returns UI / server components for a shiny survey.
#'@export

build_survey <- function(survey_title,
                         sidepanel_message,
                         biowell_situations,
                         biowell_situations_ID,
                         biowell_questions,
                         biowell_questions_ID,
                         start_message,
                         start_questions,
                         start_questions_ID,
                         start_questions_type,
                         start_drop_down_options=NULL,
                         end_message,
                         end_questions,
                         end_questions_type,
                         end_questions_ID,
                         end_drop_down_options=NULL,
                         drop_box_folder,
                         BW_app_path,
                         organisation,
                         organisation_website,
                         all_questions = TRUE,
                         all_sliders = TRUE,
                         user_report = TRUE,
                         ...) {


  token<-readRDS(".secrets/dropbox_token.rds")

  rdrop2::drop_dir(dtoken = token)



  tryCatch({
    nresponses <- nrow(rdrop2::drop_dir(drop_box_folder, dtoken = token))
  }, error = function(e) {
    stop("Dropbox folder does not exist within your Dropbox App")
  })

  #if(!getwd() == BW_app_path){stop("Please set your app folder as the working directory")}
  if(missing(survey_title)){survey_title<-""}
  if(missing(start_message)){start_message<-""}
  if(missing(end_message)){end_message<-""}
  if(missing(sidepanel_message)){sidepanel_message<-""}

  if(!missing(start_questions)){
  if(missing(start_questions_ID)){start_questions_ID<-start_questions}}

  if(missing(organisation_website)){organisation_website<-NULL}
  if(!missing(end_questions)){
  if(missing(end_questions_ID)){end_questions_ID<-end_questions}}

  if(missing(biowell_situations_ID)){biowell_situations_ID<-biowell_situations}
  if(missing(biowell_questions_ID)){biowell_questions_ID<-biowell_questions}

  if(missing(start_questions)){start_questions<-"NULL"
  start_questions_type<-"NA"
  start_drop_down_options<-NULL
  no_start<-"no_start"}

  if(missing(end_questions)){end_questions<-"NULL"
  end_questions_type<-"NA"
  end_drop_down_options<-NULL
  no_end<-"no_end"}

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),

    shiny::titlePanel(shiny::div(survey_title, style = "color: #004A86;font-weight:bold")),
    shiny::sidebarPanel(
      shiny::div("Summary", style = "font-weight:bold; font-size: 20px"),
      if(!missing(sidepanel_message)){shiny::tags$div(style="",sidepanel_message)},
      shiny::br(),
      if(!missing(organisation)){shiny::tags$div(style="","\n Created by:")},
      if(!missing(organisation)){shiny::tags$a(organisation,
                    href=organisation_website)},
      shiny::br(),
      shiny::br(),
      shiny::p("Please do not close this window until you have submitted your responses." ,style = "font-weight: bold"),
      shiny::br(),
      if(all_questions){shiny::p("In this survey, you will be unable to complete without answering all questions." ,style = "font-style: italic")},
      shiny::tags$script('
    $(function() {
      var time_now = new Date()
      $("input#client_time").val(time_now.getTime())
      $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())
      $("input#client_time_zone_char").val(time_now.toTimeString())
      $("input#client_time_zone_international").val(Intl.DateTimeFormat().resolvedOptions().timeZone)
    });
  '), shiny::tags$script('
            $(document).ready(function(){
            var d = new Date();
            var target = $("#clientTime");
            target.val(d.toLocaleString());
            target.trigger("change");
            });
            '),
      shiny::conditionalPanel('false',shiny::textInput("clientTime", "Client Time", value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time", "Client Time", value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time_zone_offset", "Time zone offset", value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time_zone_char", "Time zone verbatim", value = "")),
      shiny::conditionalPanel('false', shiny::textInput("client_time_zone_international", "Time zone international", value = ""))

    ),

    shiny::mainPanel(
      shinyjs::hidden(
        shiny::div(
          class = "page",
          id = "page1",
          shiny::tags$div(style="color:#004A86;font-size: 18px",start_message),
          lapply(add_questions(start_questions,start_questions_type,start_drop_down_options),FUN=function(x){shiny::HTML(paste(x))}),
          shiny::textOutput("textwarning"),
          shiny::tags$head(shiny::tags$style("#textwarning{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"))
        )

        ,

        add_biowell_scale(biowell_situations,biowell_questions,all_sliders),


        shiny::div(
          class = "page",
          id = paste0("page", 2+length(biowell_situations)),
          shiny::tags$div(style="color:#004A86;font-size: 18px",end_message),
          lapply(add_questions(end_questions,end_questions_type,end_drop_down_options,type_q = "end",start_questions_pr = start_questions),FUN=function(x){shiny::HTML(paste(x))}),
          shiny::actionButton("submit", "Submit")

        ),


        shiny::div(
          class = "page",
          id = paste0("page", 3+length(biowell_situations)),




          shiny::fluidRow(
            if(!user_report){shiny::textOutput("final_message")},
            if(user_report){
              shiny::div(style = "border-style: solid;border-color: #004A86;height: 1500px;width:100%;padding:9.5px;border-width: 5px; border-radius: 10px;justify-content: center;
  align-items: center; position:relative;",
                  shiny::h1("Your BIO-WELL score is:"),
                  shiny::tags$head(shiny::tags$style('h1 {font-size: 4vw;color:#0097AD;font-weight: bold;text-align: center; font: 1px}')),
                  shiny::div(class="square", shiny::textOutput("text"),
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

                  shiny::div(class="square2", shiny::textOutput("text1"),
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
                  shiny::tags$style('#mydiv6{
          list-style-position: inside;
          padding-left: 0;
          font-size: 20px;
          color: black;
          text-align: center;
          position:absolute;
          bottom:1%;
          top:23%;
          right:5%;
          left:5%;
          ul {margin-left: 0;text-align: left}

                            ;}
                            '),
                  shiny::div(class ="line",
                      shiny::tags$head(shiny::tags$style('.line {

            width: 30%;
                           height:1%;
                           line-height: 5%;
                           border-radius: 0%;
                          position:absolute;
                          bottom:20%;
                          top:14%;
                          padding: 0px 0;
                          right:25%;
                          left:35%;
                          vertical-align: middle;
                          text-align: center;
                          background: white}'))),

                  shiny::tags$head(shiny::tags$style('#mytable {

                                               background: white}'))





              )}





          ),







        )

      ),
      shiny::br(),
      shiny::actionButton("prevBtn", "< Previous"),
      shiny::actionButton("nextBtn", "Next >")
    ))




  server <- function(input, output, session) {


    start1<-Sys.time()

    rv <- shiny::reactiveValues(page = 1)


    if(user_report){

    }




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



    NUM_PAGES<-c(1+length(biowell_situations)+2)

    shiny::observe({
      shinyjs::toggleState(id = "prevBtn", condition = rv$page > 1)
      shinyjs::toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES+1)
      shinyjs::toggleState(id = "nextBtn", condition =  showPlot())
      shinyjs::hide(selector = ".page")
      shinyjs::show(
        paste0("page", rv$page)
      )

    })

    navPage <- function(direction) {
      rv$page <- rv$page + direction
    }

    shiny::observeEvent(input$prevBtn, navPage(-1))


    if(all_questions){
      shiny::observeEvent(input$nextBtn,

                          if(answers(names11=add_questions(start_questions,start_questions_type,start_drop_down_options,return_names=T),names22=colnames(t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))))
                             || "" %in% t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))[,add_questions(start_questions,start_questions_type,start_drop_down_options,return_names_text=T)]
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

      if(rv$page == NUM_PAGES){
        shinyjs::hide(id = "nextBtn")}

      if(rv$page != NUM_PAGES){

        shinyjs::show(id = "nextBtn")}

      if(input$submit==1){
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

      }

    })











    # When the Submit button is clicked, save the form data
    shiny::observeEvent(input$submit, {

      dataframe<- t(as.data.frame(unlist(shiny::reactiveValuesToList(input))))

      results<-NULL
if(length(start_questions)==1){
      if(start_questions == "NULL"){start_questions<-NULL}}
      if(length(end_questions)==1){
      if(end_questions == "NULL"){end_questions<-NULL}}
      if(is.null(start_questions)){extracted_data<-NULL}
      if(is.null(end_questions)){extracted_data_end<-NULL}

      if(!is.null(start_questions)){
      startqnames<- add_questions(start_questions,start_questions_type,start_drop_down_options,return_names=T)
      extracted_data<-sort_question_answers(startqnames,dataframe,start_questions_ID,start_drop_down_options)}

      if(!is.null(end_questions)){
      endqnames<- add_questions(end_questions,end_questions_type,end_drop_down_options,return_names=T,type_q = "end",start_questions_pr=start_questions)
      extracted_data_end<- sort_question_answers(endqnames,dataframe,end_questions_ID,end_drop_down_options)
      }

      track<-0


      start_time_POSIXct<-as.POSIXct(paste(strsplit(input$clientTime,",")[[1]][1],"",strsplit(input$clientTime,", ")[[1]][2]),format="%d/%m/%Y %H:%M:%S")
      start_time_ch<-as.character(start_time_POSIXct)
      start_time<-as.character(strsplit(start_time_ch," ")[[1]][2])
      start_date<-as.character(strsplit(start_time_ch," ")[[1]][1])


      end1<-Sys.time()

      lengthofsurvey<-round(as.numeric(difftime(end1,start1,units="secs")),2)

      sd<-start_time_POSIXct

      sd<-as.character(strftime(sd+lubridate::seconds(lengthofsurvey)))

      end_date<-strsplit(sd," ")[[1]][1]
      end_time<-strsplit(sd," ")[[1]][2]
      timezone<-strsplit(paste(input$client_time_zone_char, sep = "; ")," ")[[1]][2]
      timezonelocation<-paste(input$client_time_zone_international, sep = "; ")


      server_time<-as.character(strftime(Sys.time(), '%F %T', usetz = TRUE))


      for(situation in 1:length(biowell_situations)){

        situationquestions<-biowell_questions[[situation]]

        for(question in 1:length(situationquestions)){
          track<-track+1

          row<-c(lengthofsurvey,start_date,start_time,end_date,end_time,timezone,timezonelocation,server_time,
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


      results<-as.data.frame(results)
      if(!exists("no_start") & !exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED",colnames(extracted_data_end))}
      if(exists("no_start") & !exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time","Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED",colnames(extracted_data_end))}
      if(!exists("no_start") & exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time",colnames(extracted_data),"Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED")}
      if(exists("no_start") & exists("no_end")){colnames(results)<-c("survey_duration","start_date","start_time","end_date","end_time","timezone","timezone_location","server_submit_time","Situation","Situation_Description","Question","Question_txt","Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED")}

      results <- apply(results,2,as.character)

      results<-as.data.frame(results)

      results$mean_biowell_INVERTED<-rep(mean(as.numeric(as.matrix(results[,c("Physical_INVERTED","Emotional_INVERTED","Cognitive_INVERTED","Social_INVERTED","Spiritual_INVERTED")]))),nrow(results))
      results$mean_biowell_RAW<-100-results$mean_biowell_INVERTED

      results<-convert_data_frame(results,biowell_situations_ID,biowell_questions_ID)

      tryCatch({
        nresponses <- nrow(rdrop2::drop_dir(drop_box_folder, dtoken = token))
      }, error = function(e) {
        stop("Dropbox folder does not exist within your Dropbox App")
      })

      filePath <- file.path(paste0(tempfile(),"_ID_",nresponses,".csv"))

      write.csv(results, filePath, row.names = FALSE, quote = TRUE)
      # Upload the file to Dropbox

      rdrop2::drop_upload(filePath, path = drop_box_folder,dtoken=token)

      rdrop2::drop_dir(dtoken = token)
      results<-read.csv(filePath)
      filesInfo <- rdrop2::drop_dir(drop_box_folder,dtoken=token)
      filePaths <- filesInfo$path_display
      filePaths <- filePaths[grepl("BIOWELL_RUNNING_AVERAGE.csv" , filePaths)]
      if(length(filePaths)==0){
        running_average<-results$mean_biowell_INVERTED
        filePath1 <- file.path(paste0(tempdir(),"/BIOWELL_RUNNING_AVERAGE.csv"))
        write.csv(results$mean_biowell_INVERTED, filePath1, row.names = FALSE, quote = TRUE)
        rdrop2::drop_upload(filePath1, path = drop_box_folder,dtoken=token)}

      if(length(filePaths)==1){
        x<-rdrop2::drop_read_csv(filePaths,dtoken=token)
        running_average<-x$x
        x$x<-(x$x+results$mean_biowell_INVERTED)/2
        filePath1 <- file.path(paste0(tempdir(),"/BIOWELL_RUNNING_AVERAGE.csv"))
        write.csv(x$x, filePath1, row.names = FALSE, quote = TRUE)
        rdrop2::drop_upload(filePath1, path = drop_box_folder,dtoken=token)
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

        physical_v<-100-as.numeric(as.character(results[,  grep("Physical_INVERTED",colnames(results))]))

        emotional_v<-100-as.numeric(as.character(results[,  grep("Emotional",colnames(results))]))

        cognitive_v<-100-as.numeric(as.character(results[,   grep("Cognitive_INVERTED",colnames(results))]))

        social_v<-100-as.numeric(as.character(results[,   grep("Social_INVERTED",colnames(results))]))

        spiritual_v<-100-as.numeric(as.character(results[, grep("Spiritual_INVERTED",colnames(results))]))

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

    })


    # Automatically stop a Shiny app when closing the browser tab
    session$onSessionEnded(shiny::stopApp)
  }


  shiny::shinyApp(ui, server)

}


