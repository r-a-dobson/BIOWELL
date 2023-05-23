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
#'@examples
#'results<-evaluate_survey(data = example_survey_data, type = "situation_question")
#'results<-evaluate_survey(data = example_survey_data, type = "question")
#'
evaluate_survey<-function(data,type = "situation_question"){

  df1<-data[,grep("Social_INVERTED", colnames(data))]
  df2<-data[,grep("Spiritual_INVERTED", colnames(data))]
  df3<-data[,grep("Physical_INVERTED", colnames(data))]
  df4<-data[,grep("Emotional_INVERTED", colnames(data))]
  df5<-data[,grep("Cognitive_INVERTED", colnames(data))]

  average_biowell<-(df1+df2+df3+df4+df5)/5

  column_names<-gsub("Social_INVERTED","",colnames(average_biowell))
  df<-do.call(rbind.data.frame, strsplit(column_names,"_"))
  colnames(df)<-c("Situation","Question")
  raw<-as.data.frame(t(average_biowell))
  rownames(raw)<-NULL
  colnames(raw)<-paste0("participant_",1:ncol(raw))
  df<-cbind(df,raw)

  if(type == "situation_question"){analysis_data<-raw}

  if(type == "question"){
    analysis_data<-aggregate(.~ Question, df[,2:ncol(df)], mean, na.rm = TRUE)
    analysis_data<-analysis_data[,2:ncol(analysis_data)]}


  #calculate Cronbach's Alpha

  cronback_alpha<-ltm::cronbach.alpha(t(analysis_data))

  item_total<-multilevel::item.total(t(analysis_data))

  results_list<-list(data_frame = df ,cronback_alpha =cronback_alpha,item_total = item_total)

  return(results_list)

}




