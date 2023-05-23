#'Combine survey responses into data frame
#'
#'Function accesses survey responses from Dropbox folder and combines into single data frame.
#'
#'@param drop_box_folder path to folder in Dropbox. Defaults to the root directory.
#'@details
#'
#'With multiple survey responses stored in a Dropbox cloud storage folder, this
#'function downloads all responses and combined into a single data frame for
#'analysis of BIO-WELL scores across participants.
#'
#'@returns Data frame containing combined responses to survey
#'@export
#'@examplesIf interactive()
#'
#'data<-extract_biowell("responses")


download_data<-function(drop_box_folder,BW_app_path){

   token<-readRDS(paste0(BW_app_path,"/.secrets/dropbox_token.rds"))
   rdrop2::drop_dir(dtoken = token)
    # Read all the files into a list
    filesInfo <- rdrop2::drop_dir(drop_box_folder,dtoken=token)

    filePaths <- filesInfo$path_display
    filePaths <- filePaths[!grepl("BIOWELL_RUNNING_AVERAGE.csv" , filePaths)]
    data <- lapply(filePaths, rdrop2::drop_read_csv, dtoken=token,stringsAsFactors = FALSE)

    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)

    return(data)
  }

