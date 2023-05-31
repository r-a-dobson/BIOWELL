#' add_biowell_scale Adds BIO-WELL scale to shiny survey for each question
#'@param biowell_situations a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param biowell_questions a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd


add_biowell_scale<-function(biowell_situations,biowell_questions,all_sliders,...){

  no.sit<-length(biowell_situations)
  listofstuff<-vector(mode='list', length=no.sit)

  record=1
  allquuestions<-unlist(biowell_questions)
  names=NULL
  for(x in 1:no.sit){

    questions_for_sit<-biowell_questions[[x]]

    listofstuff[[x]]<-
      shiny::div(
        class = "page",
        id = paste0("page",x+2),
        shiny::h3(biowell_situations[x]),
        shiny::br(),
        shiny::tags$head( shiny::tags$style('h3 {font-style: italic;color: #004A86;font-size:20px}')),
        shiny::tags$head( shiny::tags$style('h5 {font-weight: bold}')),
        shiny::tags$style(type = "text/css", "
              .irs-from {background-color: #df4ed8 !important;}
              .irs-min {visibility:visible !important;font-size: 15px;top: -12px !important;}
              .irs-max {visibility:visible !important;font-size: 15px; color: white !important; top: -12px !important}"),


        shiny::tags$script(shiny::HTML(custom_slider_text(c(record:(record+length(questions_for_sit)-1))))),

        lapply(sapply(c(record:(record+length(questions_for_sit)-1)),list),sliders,biowell_questions=biowell_questions),
        if(all_sliders){shiny::tags$div(style="color:red","All sliders must be moved to continue to the next page.")},
      )

    record<-record+length(questions_for_sit)





  }

  return(listofstuff)

}


#' sliders Input BIO-WELL formatted sliders into survey.
#' @param yy a numerical value, unique IDs for sliders.
#' @param biowell_questions a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd


sliders<-function(yy,biowell_questions,...){
  allquuestions<-unlist(biowell_questions)

  list(
    shiny::h4(allquuestions[yy]),
    shiny::tags$head(shiny::tags$style('h4 {font-weight: bold;font-style: italic; color: #0097AD}')),
    shiny::tags$style(type = "text/css", paste0(".","sld",yy,"
            .irs-handle{background: white !important;size: 1px !important;
  border-radius: 4px !important;
  border: 2px solid #000000 !important;}
           ",".","sld",yy," .irs-line{background: #a9a9a9 !important;border-radius:0px !important;}
            ",".","sld",yy," .irs-grid-pol:nth-of-type(-n+1) {background:linear-gradient(to right, black 50%, transparent 50%); height: 25px;top: -30px;width: 20px; left: -20px !important}
             ",".","sld",yy," .irs-grid-pol:nth-of-type(n+2) {background:white}
             ",".","sld",yy," .irs-grid-pol:nth-of-type(n+41) {background:linear-gradient(to right, transparent 50%, black 50%); height: 25px;top: -30px;width: 20px; }
             ",".","sld",yy,"  .irs-grid-text{visibility:hidden !important;color: white;}
              ",".","sld",yy," .irs-bar {visibility:hidden }
            ",".","sld",yy,"  .irs-min {visibility:visible !important;font-size: 15px;color: black !important;background: white !important;text-align:left !important}
             ",".","sld",yy," .irs-max {visibility:visible !important;font-size: 15px; color: black !important;background: white !important; text-align:right !important}")),
    shinyWidgets::chooseSliderSkin("Big", color = "#112446"),
    shiny::div(class=paste0("sld",yy), # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("physical",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=paste0("sld",yy), # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("emotional",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=paste0("sld",yy), # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("cognitive",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=paste0("sld",yy), # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("social",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=paste0("sld",yy), # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("spiritual",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::br(),
    shiny::br())}

#' custom_slider_text Generates the custom labels for BIO-WELL slider scales.
#' @param vvv a numerical value, the unique IDs for each BIO-WELL slider.
#' @noRd

custom_slider_text<-function(vvv){

  lol<-paste0("$(document).ready(function() {
  /**
    Custom slider labels
  **/


  function returnLabels(value) {
    // remove label of selected
    $('.physical",vvv,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Physically relaxed';
    }else{
      return '<br> Physically tense &#8205';
    }
  }

  var someID = $('#physical",vvv,"ID').ionRangeSlider({ // enter your shiny slider ID here
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
    $('.emotional",vvv,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Joyful';
    }else{
      return '<br> Sad &#8205';
    }
  }

  var someID = $('#emotional",vvv,"ID').ionRangeSlider({ // enter your shiny slider ID here
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
    $('.cognitive",vvv,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Clear minded';
    }else{
      return '<br> Muddled &#8205';
    }
  }

  var someID = $('#cognitive",vvv,"ID').ionRangeSlider({ // enter your shiny slider ID here
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
    $('.social",vvv,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

    if (value === 0){ // enter your lowest slider value here
      return '<br> &#8205 Open to people';
    }else{
      return '<br> Closed to people &#8205';
    }
  }

  var someID = $('#social",vvv,"ID').ionRangeSlider({ // enter your shiny slider ID here
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
    $('.spiritual",vvv,"').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

    if (value === 0){ // enter your lowest slider value here
      return '&#8205 Part of something <br> &#8205 bigger than myself';
    }else{
      return 'Not part of something <br> bigger than myself &#8205';
    }
  }

  var someID = $('#spiritual",vvv,"ID').ionRangeSlider({ // enter your shiny slider ID here
prettify: returnLabels,
force_edges: true,
grid: false,
hide_from_to: true
});
});")
  return(lol)
}

#' add_questions Adds start or end question response boxes into shiny survey
#' @param questions a character string or vector, the questions to associated response boxes with.
#'@param type a character string or vector, the type of response for each question. Must be length of `questions`. One of; `text`, `checkbox` or `selectbox`. See details for information.
#'@param drop_down  a list of character strings or vectors, the options to offer for each `questions` of type `checkbox` or `selectbox`.
#'@param return_names a logical, indicating whether to return the unique ID names for each response box.
#'@param type_q a character string, the position of questions being added. One of `start` or `end`.
#'@param prior_qs optional; a character string, the start questions of the survey. Required if `type_q = "end"`.
#' @noRd

add_questions<-function(questions,type,drop_down,return_names=FALSE,return_names_text=FALSE,return_screen=FALSE, type_q="start",prior_qs=NULL,all_questions,...){
if(!is.null(questions)){

  listofstuff<-vector(mode='list', length=length(questions))
  dropcount<-0
  names<-NULL
  names_text<-NULL
  xx<-1
  if(type_q=="end"){xx<-length(prior_qs)+1}
  yy<-length(questions)
  if(type_q=="end"){yy<-length(prior_qs)+length(questions)}
  screenanswer<-NULL
  for(sq in xx:yy){

    sq2<-sq
    if(type_q=="end"){sq2<-sq-length(prior_qs)}
    question1<-questions[sq2]
    answertype<-type[sq2]


    if(answertype == "text"){
      listofstuff[[sq2]]<- shiny::textInput(paste0("text",sq),  shiny::HTML(paste0("<br>",shiny::h5(question1))))
      names<-c(names,paste0("text",sq))
      names_text<-c(names_text,paste0("text",sq))
    }

    if(answertype == "checkbox"){
      dropcount<-dropcount+1
      drop_down_options_select<-drop_down[[dropcount]]

      if(!length(grep(":SCREEN",drop_down_options_select))==0){
        if(!all_questions){
          message("Setting all_questions as TRUE because screening questions present")
          all_questions<-TRUE}

        for(scr in 1:length(grep(":SCREEN",drop_down_options_select))){

          screenanswer<-rbind(screenanswer, data.frame(inputname=paste0("checkbox",sq), inputresponse = gsub(":SCREEN","",drop_down_options_select)[grep(":SCREEN",drop_down_options_select)]))
        }}

      drop_down_options_select<-gsub(":SCREEN","",drop_down_options_select)

      listofstuff[[sq2]]<- shiny::checkboxGroupInput(paste0("checkbox",sq),
                                                     shiny::HTML(paste0("<br>",shiny::h5(question1))),
                                                     choices = drop_down_options_select)
      names<-c(names,paste0("checkbox",sq))}



    if(answertype == "selectbox"){
      dropcount<-dropcount+1
      drop_down_options_select<-drop_down[[dropcount]]

      if(!length(grep(":SCREEN",drop_down_options_select))==0){
        if(!all_questions){
          message("Setting all_questions as TRUE because screening questions present")
          all_questions<-TRUE}
        for(scr in 1:length(grep(":SCREEN",drop_down_options_select))){
          screenanswer<-rbind(screenanswer, data.frame(inputname=paste0("select",sq), inputresponse = gsub(":SCREEN","",drop_down_options_select)[grep(":SCREEN",drop_down_options_select)]))
        }}

      drop_down_options_select<-gsub(":SCREEN","",drop_down_options_select)


      listofstuff[[sq2]]<- shiny::selectizeInput(paste0("select",sq), label =  shiny::HTML(paste0("<br>",shiny::h5(question1))),
                                                 choices = c("",drop_down_options_select),
                                                 options = list(placeholder = 'Choose answer',
                                                                onInitialize = I('function() { this.setValue(""); }')))
      names<-c(names,paste0("select",sq))
      names_text<-c(names_text,paste0("select",sq))
    }



    if(answertype == "likert_seven"){

      listofstuff[[sq2]]<- shiny::div(
        shiny::tags$style(type = "text/css", paste0("
          .likert_sev",sq," .irs {height:70px}
              .likert_sev",sq," .irs-handle{background: white !important;size: 1px !important;
  border-radius: 4px !important;
  border: 2px solid #000000 !important;}
           .likert_sev",sq," .irs-grid-pol{height: 30px;top: -30px; background: black!important;width:5px}
            .likert_sev",sq," .irs-line{background: #a9a9a9 !important;border-radius:0px !important;}
             .likert_sev",sq," .irs-grid-pol.small{color:white;visibility:hidden !important}
            .likert_sev",sq," .irs-bar {visibility:hidden !important}
           .likert_sev",sq," .irs-grid-text {font-size: 13px;color:black!important;visibility:visible !important;top: 40% }
          .likert_sev",sq," .irs-single{visibility:hidden !important;}
         .likert_sev",sq," .irs-from{visibility:hidden !important;}
         .likert_sev",sq," .irs-to{visibility:hidden !important;}
          .likert_sev",sq," .irs-min {visibility:visible !important;font-size: 15px;color: transparent !important;background: transparent !important;text-align:left !important}
           .likert_sev",sq," .irs-max {visibility:visible !important;font-size: 15px; color: transparent !important;background: transparent !important; text-align:right !important}")),
        shiny::div(class=paste0("likert_sev",sq), # to be able to manipulate it with JQuery
                   shiny::HTML(paste0('<div class="form-group shiny-input-container" style="width:500px;">
                               <label class="control-label" id=".likert_sev',sq,'-label" for=".likert_sev',sq,'"><br>',shiny::h5(question1),'</label>
                               <input class="js-range-slider" id="likert_sev',sq,'" data-skin="shiny" data-min="1" data-max="7" data-from="3" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number" data-type="single" data-values="Strongly&lt;br&gt;&lt;br&gt; agree,Agree,Somewhat&lt;br&gt;&lt;br&gt; agree,Neither agree&lt;br&gt;&lt;br&gt; nor disagree,Somewhat&lt;br&gt;&lt;br&gt; disagree,Disagree,Strongly&lt;br&gt;&lt;br&gt; disagree" data-drag-interval="false"/>
                               </div>'))))

      names<-c(names,paste0("likert_sev",sq))

    }


    if(answertype == "likert_five"){



      listofstuff[[sq2]]<- shiny::div(
        shiny::tags$style(type = "text/css", paste0("
          .likert_five",sq," .irs {height:70px;}
              .likert_five",sq," .irs-handle{background: white !important;size: 1px !important;
  border-radius: 4px !important;
  border: 2px solid #000000 !important;}
           .likert_five",sq," .irs-grid-pol{height: 30px;top: -30px; background: black!important;width:5px}
            .likert_five",sq," .irs-line{background: #a9a9a9 !important;border-radius:0px !important;}
             .likert_five",sq," .irs-grid-pol.small{color:white;visibility:hidden !important}
            .likert_five",sq," .irs-bar {visibility:hidden !important}
           .likert_five",sq," .irs-grid-text {font-size: 13px;color:black!important;visibility:visible !important;top: 40% }
          .likert_five",sq," .irs-single{visibility:hidden !important;}
         .likert_five",sq," .irs-from{visibility:hidden !important;}
         .likert_five",sq," .irs-to{visibility:hidden !important;}
          .likert_five",sq," .irs-min {visibility:visible !important;font-size: 15px;color: transparent !important;background: transparent !important;text-align:left !important}
           .likert_five",sq," .irs-max {visibility:visible !important;font-size: 15px; color: transparent !important;background: transparent !important; text-align:right !important}")),
        shiny::div(class=paste0("likert_five",sq), # to be able to manipulate it with JQuery
                   shiny::HTML(paste0('<div class="form-group shiny-input-container" style="width:500px;">
                               <label class="control-label" id=".likert_five',sq,'-label" for=".likert_five',sq,'"><br>',shiny::h5(question1),'</label>
                               <input class="js-range-slider" id="likert_five',sq,'" data-skin="shiny" data-min="1" data-max="5" data-from="2" data-step="1" data-grid="true" data-grid-num="10" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number" data-type="single" data-values="Strongly&lt;br&gt;&lt;br&gt; agree,Agree,Neither agree&lt;br&gt;&lt;br&gt; nor disagree,Disagree,Strongly&lt;br&gt;&lt;br&gt; disagree" data-drag-interval="false"/>
                               </div>'))))
      names<-c(names,paste0("likert_five",sq))
}
    }



  if(return_names_text){return(names_text)}
  if(return_names){return(names)}
  if(return_screen){return(screenanswer)}

  return(listofstuff)
}

}

















#' answers Adds BIO-WELL scale to shiny survey for each question
#'@param names11 a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param names22 a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd

answers<-function(names11,names22){
  d<- sapply(names11,grepl,names22)
  d<-rbind(d,d)
  x<-colnames(d)[colSums(d)==0]

  if(!length(x)==0){return(TRUE)}
  if(length(x)==0){return(FALSE)}
}

#' convert_data_frame Adds BIO-WELL scale to shiny survey for each question
#'@param data a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param biowell_situations_ID a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#'@param biowell_questions_ID a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd



convert_data_frame<-function(data,biowell_situations_ID,biowell_questions_ID){

  data<-as.data.frame(data)
  initial<-data[1,1:8]

  start<-data %>% dplyr::select(colnames(data)[9:(match("Situation",colnames(data))-1)])  %>% dplyr::slice(1)

  end<-data %>% dplyr::select(colnames(data)[(match("Spiritual_INVERTED",colnames(data))+1):ncol(data)])  %>% dplyr::slice(1)

  situations<-unique(data$Situation)

  cols<-c("Physical_RAW","Physical_INVERTED","Emotional_RAW","Emotional_INVERTED","Cognitive_RAW","Cognitive_INVERTED","Social_RAW","Social_INVERTED","Spiritual_RAW","Spiritual_INVERTED")

  middle<-cbind(initial,start)

  for (x in 1:length(situations)){

    split<-data[data$Situation==x,]

    situation<-split$Situation_Description

    if(!missing(biowell_situations_ID)){
      situation<-biowell_situations_ID[x]}

    questions<-unique(split$Question)

    for (y in 1:length(questions)){

      split2<-split[split$Question==y,]

      question<-split2$Question_txt

      if(!missing(biowell_questions_ID)){
        question<-biowell_questions_ID[[x]][y]
      }

      df<-split2[,cols]
      colnames(df)<-paste0(situation,"_",question,"_",cols)



      #QIDs paste0("Q",x,"_",y,"_",cols)
      middle<-cbind(middle,df)

    }

  }

  middle<-cbind(middle,end)
  return(middle)
}


#' sort_question_answers Adds BIO-WELL scale to shiny survey for each question
#'@param qnames a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param response_df a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#'@param qs a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#'@param dropdownopt a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd

sort_question_answers<-function(qnames,response_df,qs,dropdownopt){

  checkbox_numb<-0
  checkstartqnames<-qnames[grepl("checkbox",qnames)]
  checkstartqs<-qs[grepl("checkbox",qnames)]
  option_qnames<-qnames[!grepl("text",qnames)]
  option_qnames<-qnames[!grepl("likert",qnames)]
  checkboxdropdown<- dropdownopt[grepl("checkbox",option_qnames)]


  for (x in 1:length(qnames)){

    nameof<-qnames[x]



    if(grepl("checkbox",nameof)){

      checkbox_numb<-checkbox_numb+1

      if(any(grepl(nameof,colnames(response_df)))){

        answers<-response_df[,grepl(nameof,colnames(response_df))]


        answernumbs<-which(checkboxdropdown[[checkbox_numb]] %in% answers)

        optionnumbs<-c(1:length(checkboxdropdown[[checkbox_numb]]))

        answers_final<-optionnumbs %in% answernumbs

        start_questions9<-paste0(checkstartqs[checkbox_numb],":",checkboxdropdown[[checkbox_numb]])

        answers_final<-t(data.frame(answers_final))
        colnames(answers_final)<-start_questions9
        if(x ==1 ){formatted_data<-answers_final}
        if(!x ==1 ){formatted_data<-cbind(formatted_data,answers_final)}

      }

      if(!any(grepl(nameof,colnames(response_df)))){

        answers_final<-rep(FALSE,length(checkboxdropdown[[checkbox_numb]]))
        answers_final<-t(data.frame(answers_final))
        start_questions9<-paste0(checkstartqs[checkbox_numb],":",checkboxdropdown[[checkbox_numb]])
        colnames(answers_final)<-start_questions9
        if(x ==1 ){formatted_data<-answers_final}
        if(!x ==1 ){formatted_data<-cbind(formatted_data,answers_final)}
      }

    }

    if(!grepl("checkbox",nameof)){

      if(any(grepl(nameof,colnames(response_df)))){

        answers<-response_df[,grepl(nameof,colnames(response_df))]

        answers_final<-t(data.frame(answers))

        if(grepl("likert_five",nameof)){

          answers_final<-c("Strongly Agree","Agree","Neither agree nor disagree","Disagree","Strongly disagree")[as.numeric(answers_final[1,1])+1]

          answers_final<-as.data.frame(answers_final)

        }
        if(grepl("likert_sev",nameof)){
          answers_final<-c("Strongly Agree","Agree","Somewhat agree","Neither agree nor disagree","Somewhat disagree","Disagree","Strongly disagree")[as.numeric(answers_final[1,1])+1]
          answers_final<-as.data.frame(answers_final)
        }


        colnames(answers_final)<-qs[x]

        if(x ==1 ){formatted_data<-answers_final}
        if(!x ==1 ){formatted_data<-cbind(formatted_data,answers_final)}

      }


      if(!any(grepl(nameof,colnames(response_df)))){

        answers<-NA
        answers_final<-t(data.frame(answers))
        colnames(answers_final)<-qs[x]
        if(x ==1 ){formatted_data<-answers_final}
        if(!x ==1 ){formatted_data<-cbind(formatted_data,answers_final)}

      }

    }
  }

  return(formatted_data)
}















generate_report<-function(results,running_average){
  n<-results$mean_biowell_INVERTED-running_average

  color2<-"black"
  words<-"NA"

  if(!is.na(n)){
  if(n<0){words<-"lower than"
  color2<-"red"}
  if(n>0){words<-"higher than"
  color2<-"green"}
  if(n==0){words<-"equal to"
  color2<-"darkorange"}}


  color1<-"black"
  messagex<-"NA"
  if(!is.na(n)){
  if(dplyr::between(results$mean_biowell_INVERTED,0,49)){messagex<-"negative"
  color1<-"red"}
  if(dplyr::between(results$mean_biowell_INVERTED,51,100)){messagex<-"positive"
  color1<-"green"}
  if(results$mean_biowell_INVERTED==50){messagex<-"neutral"
  color1<-"darkorange"}}

  data_1<-explore_data(data=results,plot=FALSE)[[2]]
  data_1<-data_1[rev(order(data_1$mean_biowell)),]


  physical_v<-as.numeric(as.character(results[,  grep("Physical_INVERTED",colnames(results))]))

  emotional_v<-as.numeric(as.character(results[,  grep("Emotional_INVERTED",colnames(results))]))

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
                                            '<ul style ="text-align: left;padding-left:20%"><li>',data_1[1,3],' (',round(data_1[1,4],2),'/100)</li>',
                                            '<li>',data_1[2,3],' (',round(data_1[2,4],2),'/100)</li>',
                                            '<li>',data_1[3,3],' (',round(data_1[3,4],2),'/100)</li></ul>',
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
                shiny::HTML("<div id='mydiv66'>", paste0("<br>","</div>")))))}



sort_running_average<-function(filePath,Dropbox_App_folder,token,results){

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

  if(!curl::has_internet()) {
    running_average<-NA}

  return(running_average)
}





generate_results_data<-function(start1,dataframe,biowell_questions,biowell_situations,extracted_data_screen,extracted_data_end,extracted_data,input_client ,input_tz  ,input_tz_ch,biowell_situations_ID,biowell_questions_ID ){

  track<-0
  results<-NULL

  start_time_POSIXct<-as.POSIXct(paste(strsplit(input_client,",")[[1]][1],"",strsplit(input_client,", ")[[1]][2]),format="%d/%m/%Y %H:%M:%S")
  start_time_ch<-as.character(start_time_POSIXct)
  start_time<-as.character(strsplit(start_time_ch," ")[[1]][2])
  start_date<-as.character(strsplit(start_time_ch," ")[[1]][1])


  end1<-Sys.time()

  lengthofsurvey<-round(as.numeric(difftime(end1,start1,units="secs")),2)

  sd<-start_time_POSIXct

  sd<-as.character(strftime(sd+lubridate::seconds(lengthofsurvey)))

  end_date<-strsplit(sd," ")[[1]][1]
  end_time<-strsplit(sd," ")[[1]][2]
  timezone<-strsplit(paste(input_tz, sep = "; ")," ")[[1]][2]
  timezonelocation<-paste(input_tz_ch, sep = "; ")

  server_time<-as.character(strftime(Sys.time(), '%F %T', usetz = TRUE))


  for(situation in 1:length(biowell_situations)){

    situationquestions<-biowell_questions[[situation]]

    for(question in 1:length(situationquestions)){
      track<-track+1

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
  return(results)
}
