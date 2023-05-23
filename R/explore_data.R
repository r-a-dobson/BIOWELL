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
#'data("example_survey_data")
#'
#'results <- explore_data(data = example_survey_data, type = "column", column_name = "Age", plot = FALSE)
#'results <- explore_data(data = example_survey_data, type = "column", column_name = "Visit.moorlands", plot = FALSE)
#'results <- explore_data(data = example_survey_data, type = "situation", plot = FALSE)
#'results <- explore_data(data = example_survey_data, type = "question", plot = FALSE)
#'results <- explore_data(data = example_survey_data, type = "situation_question", plot = FALSE)

explore_data<-function(data,
                       type = "situation_question",
                       column_name,
                       var_name,
                       biowell_scale,
                       plot = TRUE

){

  if(!missing(biowell_scale)){biowell_scale<-stringr::str_to_title(biowell_scale)}

  if(type=="column"){

    plot_data<-data[,c(column_name,"mean_biowell_INVERTED")]
    colnames(plot_data)<-c("variable","mean_biowell")

    ylab_name<-"BIO-WELL score"

    if(!missing(biowell_scale)){
      average_biowell<-rowMeans(data[,grep(paste0(biowell_scale,"_INVERTED"), colnames(data))])
      plot_data<-as.data.frame(cbind(data[,c(column_name)],average_biowell))
      colnames(plot_data)<-c("variable","mean_biowell")
      ylab_name<-paste0("BIO-WELL score:",biowell_scale)
    }


    if(missing(var_name)){var_name<-column_name}

    plot1<-ggplot2::ggplot(data=plot_data,ggplot2::aes(x=variable,y=as.numeric(mean_biowell),fill=variable))+
      ggplot2::geom_boxplot()+
      ggplot2::geom_point(position = "jitter",alpha=0.2)+
      ggplot2::coord_flip()+
      ggplot2::xlab(var_name)+
      ggplot2::ylab(ylab_name)+
      ggplot2::theme_bw()+
      ggplot2::labs(colour=var_name)+
      ggplot2::theme(legend.position = "none")



    if(plot){

      oldpar<- par(no.readonly=TRUE)
      on.exit(suppressWarnings(graphics::par(oldpar)),add=T)

      #Function to allow users to click through each plot individually
      graphics::par(ask=TRUE, new = FALSE)


      plot(ggplot2::ggplot(data=plot_data,ggplot2::aes(x=variable,y=as.numeric(mean_biowell),fill=variable))+
             ggplot2::geom_boxplot()+
             ggplot2::coord_flip()+
             ggplot2::geom_point(position = "jitter",alpha=0.2)+
             ggplot2::xlab(var_name)+
             ggplot2::ylab(ylab_name)+
             ggplot2::theme_bw()+
             ggplot2::labs(colour=var_name)+
             ggplot2::theme(legend.position = "none"))}


    colnames(plot_data)<-c(var_name,"mean_biowell")

    if(!missing(biowell_scale)){
      colnames(plot_data)[colnames(plot_data)=="mean_biowell"]<-paste0("mean_biowell",biowell_scale)
    }

    results_list<-list(plot = plot1, plot_data = plot_data)
  }

  if(!type=="column"){

    df1<-data[,grep("Social_INVERTED", colnames(data))]
    df2<-data[,grep("Spiritual_INVERTED", colnames(data))]
    df3<-data[,grep("Physical_INVERTED", colnames(data))]
    df4<-data[,grep("Emotional_INVERTED", colnames(data))]
    df5<-data[,grep("Cognitive_INVERTED", colnames(data))]

    average_biowell<-(df1+df2+df3+df4+df5)/5

    ylab_name<-"BIO-WELL score"
    default<-"Social_INVERTED"
    if(!missing(biowell_scale)){

      average_biowell<-data[,grep(paste0(biowell_scale,"_INVERTED"), colnames(data))]
      ylab_name<-paste0("BIO-WELL score:",biowell_scale)
      default<-paste0(biowell_scale,"_INVERTED")
    }


    column_names<-gsub(default,"",colnames(average_biowell))

    df<-do.call(rbind.data.frame, strsplit(column_names,"_"))
    colnames(df)<-c("situation","question")
    df$question<-gsub("[.]"," ",df$question)
    df$situation_question<-paste0(df$situation,": ",df$question)
    raw<-as.data.frame(t(average_biowell))
    rownames(raw)<-NULL
    colnames(raw)<-paste0("participant_",1:ncol(raw))
    df<-cbind(df,raw)

    if(4==ncol(df)){ df$mean_biowell<-df[,4]}
    if(!4==ncol(df)){ df$mean_biowell<-rowMeans(df[,4:ncol(df)])}

    df$variable<-df[,type]
    if(missing(var_name)){var_name<-type}

    plot_data<-NULL
    for(x in 1:ncol(raw)){

      plot_datax<- df[,c("situation","question","situation_question",colnames(raw)[x],"variable")]
      colnames(plot_datax)[grep(colnames(raw)[x],colnames(plot_datax))]<-"mean_biowell"
      plot_datax$participant<-rep(x,nrow(plot_datax))
      plot_data<-rbind(plot_data,plot_datax)
    }

    if(!type=="situation_question"){

    plot_data_final<-aggregate(mean_biowell ~ variable + participant,data=plot_data,FUN="mean")
    plot_data_final$mean_biowell<-as.numeric(plot_data_final$mean_biowell)

    plot1<-ggplot2::ggplot(data=plot_data_final,ggplot2::aes(x=variable,y=mean_biowell,fill=variable))+
      ggplot2::geom_boxplot()+
      ggplot2::coord_flip()+
      ggplot2::geom_point(position = "jitter",alpha=0.2)+
      ggplot2::xlab(var_name)+
      ggplot2::ylab(ylab_name)+
      ggplot2::theme_bw()+
      ggplot2::labs(colour=var_name)+
      ggplot2::theme(legend.position = "none")


    if(plot){

      oldpar<- par(no.readonly=TRUE)
      on.exit(suppressWarnings(graphics::par(oldpar)),add=T)

      #Function to allow users to click through each plot individually
      graphics::par(ask=TRUE, new = FALSE)


    plot(ggplot2::ggplot(data=plot_data_final,ggplot2::aes(x=variable,y=mean_biowell,fill=variable))+
           ggplot2::geom_boxplot()+
           ggplot2::coord_flip()+
           ggplot2::geom_point(position = "jitter",alpha=0.2)+
           ggplot2::xlab(var_name)+
           ggplot2::ylab(ylab_name)+
           ggplot2::theme_bw()+
           ggplot2::labs(colour=var_name)+
           ggplot2::theme(legend.position = "none"))
    }}






    if(type=="situation_question"){

      plot1 <- vector(mode = "list", length = length(unique(plot_data$situation)))

      plot_data_final<-NULL
      for(sq in 1:length(unique(plot_data$situation))){


      plot_data_filt<-  plot_data[plot_data$situation==unique(plot_data$situation)[sq],]

      plot_data_final_split<-aggregate(mean_biowell ~ variable + participant,data=plot_data_filt,FUN="mean")
      plot_data_final_split$mean_biowell<-as.numeric(plot_data_final_split$mean_biowell)
      plot_data_final<-rbind(plot_data_final,plot_data_final_split)
      plot1[[sq]]<-ggplot2::ggplot(data=plot_data_final_split,ggplot2::aes(x=variable,y=mean_biowell,fill=variable))+
        ggplot2::geom_boxplot()+
        ggplot2::coord_flip()+
        ggplot2::geom_point(position = "jitter",alpha=0.2)+
        ggplot2::xlab(var_name)+
        ggplot2::ylab(ylab_name)+
        ggplot2::theme_bw()+
        ggplot2::ggtitle(unique(plot_data$situation)[sq])+
        ggplot2::labs(colour=var_name)+
        ggplot2::theme(legend.position = "none")

     }

      if(plot){

        oldpar<- par(no.readonly=TRUE)
        on.exit(suppressWarnings(graphics::par(oldpar)),add=T)

        #Function to allow users to click through each plot individually
        graphics::par(ask=TRUE, new = FALSE)

        for (i in 1:length(plot1)){
          plot(plot1[[i]])
        }

        }
        }



    if(!missing(biowell_scale)){
      colnames(plot_data_final)[colnames(plot_data_final)=="mean_biowell"]<-paste0("mean_biowell",biowell_scale)
    }

    colnames(plot_data_final)[1]<-type
    results_list<-list(plot = plot1, plot_data = plot_data_final)
  }

  return(results_list)




}

