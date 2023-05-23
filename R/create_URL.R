#'Create shiny survey with BIOWELL scale
#'
#'Function to generate custom shiny survey with BIOWELL scale
#'
#'@param dates a
#'@details d
#'@returns "dd"
#'@export


create_URL<-function(BW_app_path,
                     appName,
                     shiny_server_username,
                     shiny_server_token,
                     shiny_server_secret){

  accounts<-rsconnect::accounts()

  if(!shiny_server_username %in% accounts$name){

  rsconnect::setAccountInfo(name=shiny_server_username,
                            token=shiny_server_token,
                            secret=shiny_server_secret)}

 message("Deploying survey - be patient this will take a couple of minutes")

 URL<-utils::capture.output( rsconnect::deployApp(appTitle = appName,BW_app_path,account=shiny_server_username))


 URL<-gsub("Application successfully deployed to ","",URL[grep("Application successfully deployed",URL)])

 if(length(URL)==0){message("Possible error deploying App, retrying:")
   rsconnect::deployApp(appTitle = appName,BW_app_path,account=shiny_server_username)}

 return(URL)
}
