#' add_biowell_scale Adds BIO-WELL scale to shiny survey for each question
#'@param biowell_situations a character string or vector, describing the environmental space setting for BIO-WELL questions.
#'@param biowell_questions a list of character strings or vectors, the questions for each separate `biowell_situations` for BIO-WELL responses.
#' @noRd


add_biowell_scale<-function(biowell_situations,biowell_questions,...){

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
        id = paste0("page",x+1),
        shiny::h3(biowell_situations[x]),
        shiny::tags$head( shiny::tags$style('h3 {font-style: italic;color: #004A86;font-weight: bold}')),
        shiny::tags$style(type = "text/css", "
              .irs-from {background-color: #df4ed8 !important;}
              .irs-min {visibility:visible !important;font-size: 15px;top: -12px !important;}
              .irs-max {visibility:visible !important;font-size: 15px; color: white !important; top: -12px !important}"),


        shiny::tags$script(shiny::HTML(custom_slider_text(c(record:(record+length(questions_for_sit)-1))))),

        lapply(sapply(c(record:(record+length(questions_for_sit)-1)),list),sliders,biowell_questions=biowell_questions),
        shiny::tags$div(style="color:red","All sliders must be moved to continue to the next page."
        ))

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
    shiny::h2("l"),
    shiny::tags$head(shiny::tags$style('h2 {color:white; font: 1px}')),
    shiny::h4(allquuestions[yy]),
    shiny::tags$head( shiny::tags$style('h4 {font-weight: bold;font-style: italic; color: #0097AD}')),

    shiny::tags$style(type = "text/css", "
              .irs-handle{background: white !important;size: 1px !important;
  border-radius: 4px !important;
  border: 2px solid #000000 !important;}
               .irs-line{background: #a9a9a9 !important;border-radius:0px !important;}
               .irs-grid-pol:nth-of-type(-n+1) {background:linear-gradient(to right, black 50%, transparent 50%); height: 25px;top: -30px;width: 20px; left: -20px !important}
               .irs-grid-pol:nth-of-type(n+2) {background:white}
               .irs-grid-pol:nth-of-type(n+41) {background:linear-gradient(to right, transparent 50%, black 50%); height: 25px;top: -30px;width: 20px; }
               .irs-grid-text{visibility:hidden !important;color: white}
              .irs-bar {visibility:hidden !important}
              .irs-min {visibility:visible !important;font-size: 15px;color: black !important;background: white !important;text-align:left !important}
              .irs-max {visibility:visible !important;font-size: 15px; color: black !important;background: white !important; text-align:right !important}"),
    shinyWidgets::chooseSliderSkin("Big", color = "#112446"),
    shiny::div(class=yy, # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("physical",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=yy, # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("emotional",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=yy, # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("cognitive",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=yy, # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("social",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)),
    shiny::div(class=yy, # to be able to manipulate it with JQuery
               style="height: 70px;",
               shiny::sliderInput(paste0("spiritual",yy,"ID"),step=0.0001,
                                  "", ticks = T,
                                  min = 0, max = 100, value = 50,width=800)))}

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
#'@param start_questions_pr optional; a character string, the start questions of the survey. Required if `type_q = "end"`.
#' @noRd

add_questions<-function(questions,type,drop_down,return_names=FALSE,return_names_text=FALSE,type_q="start",start_questions_pr=NULL,...){

  listofstuff<-vector(mode='list', length=length(questions))
  dropcount<-0
  names<-NULL
  names_text<-NULL
  xx<-1
  if(type_q=="end"){xx<-length(start_questions_pr)+1}
  yy<-length(questions)
  if(type_q=="end"){yy<-length(start_questions_pr)+length(questions)}

  for(sq in xx:yy){

    sq2<-sq
    if(type_q=="end"){sq2<-sq-length(start_questions_pr)}
    question1<-questions[sq2]
    answertype<-type[sq2]


    if(answertype == "text"){
      listofstuff[[sq2]]<- shiny::textInput(paste0("text",sq), shiny::h5(question1))
      names<-c(names,paste0("text",sq))
      names_text<-c(names_text,paste0("text",sq))
    }

    if(answertype == "checkbox"){
      dropcount<-dropcount+1
      drop_down_options_select<-drop_down[[dropcount]]
      listofstuff[[sq2]]<- shiny::checkboxGroupInput(paste0("checkbox",sq),
                                                     shiny::h5(question1),
                                                     choices = drop_down_options_select)
      names<-c(names,paste0("checkbox",sq))}

    if(answertype == "selectbox"){
      dropcount<-dropcount+1
      drop_down_options_select<-drop_down[[dropcount]]
      listofstuff[[sq2]]<- shiny::selectizeInput(paste0("select",sq), label = shiny::h5(question1),
                                                 choices = drop_down_options_select,
                                                 options = list(placeholder = 'Choose answer',
                                                                onInitialize = I('function() { this.setValue(""); }')))
      names<-c(names,paste0("select",sq))
    }

  }
  if(return_names_text){return(names_text)}
  if(return_names){return(names)}

  return(listofstuff)


}



















answers<-function(names11,names22){
  d<- sapply(names11,grepl,names22)
  d<-rbind(d,d)
  x<-colnames(d)[colSums(d)==0]

  if(!length(x)==0){return(TRUE)}
  if(length(x)==0){return(FALSE)}
}




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




#qnames<-c("text1","selectbox2","text3","checkbox4")
#response_df<-c(text1="hi",selectbox2="15-20",text3="yo",checkbox41="dog",checkbox42="cat")
#qs<-c("greeting1","age","greeting2","pet")
#dropdownopt<-list(c("15-20","10-29"),
#                  c("frog","cat","dog"))

sort_question_answers<-function(qnames,response_df,qs,dropdownopt){

  checkbox_numb<-0
  checkstartqnames<-qnames[grepl("checkbox",qnames)]
  checkstartqs<-qs[grepl("checkbox",qnames)]
  option_qnames<-qnames[!grepl("text",qnames)]
  checkboxdropdown<- dropdownopt[grepl("checkbox",option_qnames)]

  formatted_data<-NULL

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
        formatted_data<-cbind(formatted_data,answers_final)

      }

      if(!any(grepl(nameof,colnames(response_df)))){

        answers_final<-rep(FALSE,length(checkboxdropdown[[checkbox_numb]]))
        answers_final<-t(data.frame(answers_final))
        start_questions9<-paste0(checkstartqs[checkbox_numb],":",checkboxdropdown[[checkbox_numb]])
        colnames(answers_final)<-start_questions9
        formatted_data<-cbind(formatted_data,answers_final)
      }

    }

    if(!grepl("checkbox",nameof)){

      if(any(grepl(nameof,colnames(response_df)))){

        answers<-response_df[,grepl(nameof,colnames(response_df))]

        answers_final<-t(data.frame(answers))
        colnames(answers_final)<-qs[x]
        formatted_data<-cbind(formatted_data,answers_final)

      }


      if(!any(grepl(nameof,colnames(response_df)))){

        answers<-NA
        answers_final<-t(data.frame(answers))
        colnames(answers_final)<-qs[x]
        formatted_data<-cbind(formatted_data,answers_final)

      }

    }
  }

  return(formatted_data)
}


