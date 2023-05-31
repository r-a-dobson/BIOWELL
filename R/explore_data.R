#'Process and plot BIO-WELL scores
#'
#'Function to generate plots of BIO-WELL scores across questions and groups from
#'BIO-WELL survey response data.
#'
#'@param data a data frame containing the combined response data frames from a
#'  `BIOWELL` package survey. See `download_data()` to obtain this.
#'@param column_name optional; a character string, the name of the column in
#'  `data` that contains categorical responses to plot BIO-WELL scores between.
#'@param var_name optional; the name for the variable to appear on the y axis of
#'  plot.
#'@param biowell_scale optional; a character string, the wellbeing domain to
#'  solely plot instead of full BIO-WELL scores. One of; `physical`,
#'  `emotional`, `cognitive`, `social` and `spiritual.` See details for more
#'  information.
#'@param plot optional; a logical, indicating whether to show plots in R window.
#'  Default is `TRUE.`
#'@details
#'
#'This function can be used to plot average BIO-WELL scores from participant
#'reponses to your custom BIO-well survey (generated using `build_survey()`).
#'Please see function `download_data()` for extracting your BIO-WELL survey
#'response data from Dropbox.
#'
#'All plots are created using the R package `ggplot2` (Wickham and Chang, 2016).
#'
#'There are two approaches:
#'
#'+ Default: This will generate a box plot of BIO-WELL scores for each BIO-WELL
#'question (`biowell_questions` in `build_survey()`) within each BIO-WELL situation
#'(`biowell_situations` in `build_surveys()`)
#'
#'+ Column: If a `column_name` is provided, for each unique category in this
#'column, a box plot of average BIO-WELL scores for each participant will be
#'generated. This can be used to compare average BIO-WELL scores across
#'responses to other questions with your BIO-WELL survey (e.g. age categories,
#'likert scale responses)
#'
#'
#'# BIO-WELL scores
#'
#'By default, the typical, inverted BIO-well score is plotted. This is
#'calculated by taking the average score from across five wellbeing domains
#'(physical, emotional, cognitive, social and spiritual). When inverted, the
#'BIO-WELL score can be interpreted as 0-49, a negative wellbeing response to
#'biodiversity, 50, a neutral wellbeing responses and 51-100 representing a
#'positive wellbeing response to biodiversity.
#'
#'Please see Irvine et al., (2023) for more details on the BIO-WELL score.
#'
#'However, if you would like to only look at one of the domains in your plots,
#'then you can use the argument `biowell_scale` to specify which to plot.
#'
#'@returns a list object containing the plots and processed data frame.
#'@references
#'Irvine, K.N., Fisher, J.C., Bentley, P.R., Nawrath, M., Dallimer,
#'M., Austen, G.E., Fish, R. and Davies, Z.G., 2023. BIO-WELL: The development
#'and validation of a human wellbeing scale that measures responses to
#'biodiversity. Journal of Environmental Psychology, 85, p.101921.
#'
#'
#'Wickham, H. and Chang, W. , 2016. Package ‘ggplot2’. Create
#'elegant data visualisations using the grammar of graphics. Version, 2(1),
#'pp.1-189.
#'
#'@export
#'@examples
#'
#'data("sample_BW_data")
#'
#'# Average BIO-WELL scores for each question across all participants
#'results <- explore_data(sample_BW_data)
#'
#'# Average BIO-WELL score for each Gender response category
#'results <- explore_data(sample_BW_data,
#'                        column_name = "Gender")
#'
#'# Average score in the emotional wellbeing domain of BIO-WELL across age groups.
#'results <- explore_data(sample_BW_data,
#'                        column_name = "Age",
#'                        biowell_scale = "emotional")
#'
#'# Average BIO-WELL scores across sight impairment responses
#'results <- explore_data(sample_BW_data,
#'                        column_name = "impairment.sight")
#'
explore_data <- function(data,
                         column_name,
                         var_name,
                         biowell_scale,
                         plot = TRUE) {


  if(!missing(biowell_scale)){

    # Check scale type is accepted.
    biowell_scale<- match.arg(biowell_scale, choices = c("physical",
                                                         "social",
                                                         "cognitive",
                                                         "spiritual",
                                                         "emotional"))
    biowell_scale <- stringr::str_to_title(biowell_scale)
  }

  if (!inherits(data, "data.frame")) {
    stop("Please provide data as object of class data.frame")
  }

  if (length(grep("Social_INVERTED", colnames(data))) == 0) {
    stop(
      "Columns missing: were these data collected using the BIOWELL package?"
    )
  }


  #--------------------------------------------------------------------
  # Process data and plot BIO-WELL scores across column categories
  #--------------------------------------------------------------------

  if(!missing(column_name)){

    # Get column data and mean bio-well scores
    plot_data <- data[, c(column_name, "mean_biowell_INVERTED")]

    # Default x axis column label for plot.
    xlab_name <- "BIO-WELL score"

    # If specified, select only one of the wellbeing domain BIO-WELL scores

    if (!missing(biowell_scale)) {

      biowell_scale_type<-paste0(biowell_scale, "_INVERTED")

      # Select columns contain values for this domain only and take average
      avg_biowell <- rowMeans(data[, grep(biowell_scale_type, colnames(data))])

      plot_data <- as.data.frame(cbind(data[, c(column_name)], avg_biowell))

      # Change x axis column label to specify domain of BIO-WELL scores.
      xlab_name <- paste0("BIO-WELL score:", biowell_scale)
    }

    # Label columns for plotting
    colnames(plot_data) <- c("variable", "mean_biowell")


    # Set default y axis label if not given by user
    if (missing(var_name)) {
      var_name <- column_name
    }

    # Plot data in ggplot and store as object
    plot1 <- ggplot2::ggplot(data = plot_data,
                             ggplot2::aes(x = variable,
                                          y = as.numeric(mean_biowell),
                                          fill = variable))+
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(position = "jitter", alpha = 0.2) +
      ggplot2::coord_flip() +
      ggplot2::xlab(var_name) +
      ggplot2::ylab(xlab_name) +
      ggplot2::theme_bw() +
      ggplot2::labs(colour = var_name) +
      ggplot2::theme(legend.position = "none")



    # If user wants to plot figure in R
    if(plot){

      # Save previous settings so that these can be returned after function
      oldpar<- par(no.readonly=TRUE)
      on.exit(suppressWarnings(graphics::par(oldpar)),add=T)

      #Function to allow users to click through each plot individually
      graphics::par(ask=TRUE, new = FALSE)


      plot(ggplot2::ggplot(data = plot_data,
                           ggplot2::aes(x = variable,
                                        y = as.numeric(mean_biowell),
                                        fill = variable))+
             ggplot2::geom_boxplot() +
             ggplot2::coord_flip() +
             ggplot2::geom_point(position = "jitter", alpha = 0.2) +
             ggplot2::xlab(var_name) +
             ggplot2::ylab(xlab_name) +
             ggplot2::theme_bw() +
             ggplot2::labs(colour = var_name) +
             ggplot2::theme(legend.position = "none"))

    }

    # Re-name plot_data columns to specify the variable
    colnames(plot_data) <- c(var_name, "mean_biowell")

    #If only one wellbeing domain averaged rename column
    if (!missing(biowell_scale)) {
      colnames(plot_data)[2] <- paste0("mean_biowell", biowell_scale)
    }

    # Create list object containing plot and processed data
    results_list <- list(plot = plot1, plot_data = plot_data)
  }


  #----------------------------------------------------------------------------
  # Process and plot BIO-WELL scores across BIO-WELL questions in each situation
  #----------------------------------------------------------------------------

  if(missing(column_name)){

    # Get BIO-WELL scores for each domain
    df1 <- data[, grep("Social_INVERTED", colnames(data))]
    df2 <- data[, grep("Spiritual_INVERTED", colnames(data))]
    df3 <- data[, grep("Physical_INVERTED", colnames(data))]
    df4 <- data[, grep("Emotional_INVERTED", colnames(data))]
    df5 <- data[, grep("Cognitive_INVERTED", colnames(data))]

    # Calculate average BIO-WELL scores for each question
    average_biowell <- (df1 + df2 + df3 + df4 + df5) / 5

    # Default x axis column label for plot.
    xlab_name <- "BIO-WELL score"

    default <- "Social_INVERTED"

    if(!missing(biowell_scale)){

      biowell_scale_type <- paste0(biowell_scale, "_INVERTED")

      average_biowell <- data[, grep(biowell_scale_type, colnames(data))]

      # Change x axis column label to specify domain of BIO-WELL scores.
      xlab_name <- paste0("BIO-WELL score:", biowell_scale)

      default <- paste0(biowell_scale, "_INVERTED")
    }


    column_names <- gsub(default, "", colnames(average_biowell))

    df <- do.call(rbind.data.frame, strsplit(column_names, "_"))

    # Re-label columns
    colnames(df) <- c("situation", "question")

    # Replace periods with spaces
    df$question <- gsub("[.]", " ", df$question)

    # Generate situation:question labels for plotting
    df$situation_question <- paste0(df$situation, ": ", df$question)

    # Flip the average BIO-WELL score data
    raw <- as.data.frame(t(average_biowell))

    #Remove row names
    rownames(raw) <- NULL

    # Label each column as each participant
    colnames(raw) <- paste0("participant_", 1:ncol(raw))

    df <- cbind(df, raw)

    # If only one response, cannot take average
    if(4 == ncol(df)) {
      df$mean_biowell <- df[, 4]
    }

    # If over one reponse, take average of scores for each question
    if (!4 == ncol(df)) {
      df$mean_biowell <- rowMeans(df[, 4:ncol(df)])
    }

    # Add variable column
    df$variable <- df[, "situation_question"]

    if (missing(var_name)) {
      var_name <- "situation_question"
    }

    plot_data<-NULL

    # For every participant response, create a row for plotting with ggplot

    for (x in 1:ncol(raw)) {

      # Select participant data
      plot_datax <- df[, c(1, 2, 3, 3 + x, ncol(df))]

      # Rename column
      colnames(plot_datax)[4]<-"mean_biowell"

      # Column for participant ID
      plot_datax$participant <- rep(x, nrow(plot_datax))

      plot_data <- rbind(plot_data, plot_datax)

    }


    # Create empty list for binding each plot for each situation to

    plot1 <- vector(mode = "list", length = length(unique(plot_data$situation)))

    plot_data_final <- NULL

    # For every unique BIO-WELL situation within survey...
    for(sq in 1:length(unique(plot_data$situation))) {

      # Select only responses for this situation
      filt <- plot_data[plot_data$situation == unique(plot_data$situation)[sq],]

      filt$mean_biowell<-as.numeric(filt$mean_biowell)

      plot_data_final<-rbind(plot_data_final,filt)

      # Add plot to list of plots
      plot1[[sq]] <- ggplot2::ggplot(data = filt,
                                     ggplot2::aes(x = variable,
                                                  y = mean_biowell,
                                                  fill = variable))+
        ggplot2::geom_boxplot() +
        ggplot2::coord_flip() +
        ggplot2::geom_point(position = "jitter", alpha = 0.2) +
        ggplot2::xlab(var_name) +
        ggplot2::ylab(xlab_name) +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(unique(plot_data$situation)[sq]) +
        ggplot2::labs(colour = var_name) +
        ggplot2::theme(legend.position = "none")

    }

    # If user wants plot to be generated in R

    if (plot) {
      # Save previous settings so that these can be returned after function
      oldpar <- par(no.readonly = TRUE)
      on.exit(suppressWarnings(graphics::par(oldpar)), add = T)

      #Function to allow users to click through each plot individually
      graphics::par(ask = TRUE, new = FALSE)

      for (i in 1:length(plot1)) {
        plot(plot1[[i]])
      }

    }

    # Relabel if only on wellbeing domain
    if(!missing(biowell_scale)){
      colnames(plot_data_final)[4] <- paste0("mean_biowell", biowell_scale)
    }

    # Remove variable column used for plotting
    plot_data_final <- plot_data_final[, !names(plot_data_final) %in%
                                         c("variable")]

    results_list<-list(plot = plot1, plot_data = plot_data_final)
  }

  return(results_list)

}
