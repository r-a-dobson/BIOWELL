#' Deploy BIO-WELL survey to Shiny Server
#'
#' Function to deploy BIO-WELL survey Shiny App to Shiny Server and generate
#' shareable URL for participants.
#'
#'@param BW_app_path a character string, the path to your BIO-WELL Shiny App
#'  folder.
#'@param BW_app_name a character string, a name for your BIO-WELL Shiny App.
#'@param Shiny_Server_user a character string, your Posit Shiny Server username.
#'@param Shiny_Server_token a character string, your Posit Shiny Server Token.
#'@param Shiny_Server_secret a character string, your Posit Shiny Server Secret.
#'@details
#'
#'To share the BIO-WELL survey that you have built using `build_survey()` with
#'participants, we provide `create_URL()` to upload your BIOWELL survey Shiny
#'App to the [Posit Shiny
#'Server](https://posit.co/products/open-source/shinyserver/).
#'
#'
#'For this, you will first need to register for a free Posit Shiny Server
#'account on their website. This will provide you with your Shiny Server
#'username, token and secret for input into the `create_URL()` function, which
#'acts as a wrapped for the `rsconnect` [R
#'package](https://CRAN.R-project.org/package=rsconnect).
#'
#'
#'To deploy the app successfully, ensure that you have:
#'
#'1) Activate a long-lived Dropbox access token. This should be stored within
#'your BIO-WELL survey Shiny App (automatically done using `activate_dropbox()`
#'function)
#'
#'2) Within your BIO-WELL survey Shiny App folder, an R script named "app.R"
#'should be saved. This R script should contain your input into
#'`build_survey()`. Please remember to include nothing in the script apart from
#'this function and your filled arguments (e.g. no "Library(BIOWELL)" or
#'"setwd()"), and to specify the package that the build_survey() function is
#'from by using the double-colon operator (i.e. `BIOWELL::build_survey()` with
#'your arguments within the brackets).
#'
#'
#'Once the function has been run, the BIO-WELL survey Shiny App will be deployed
#'to your Shiny Server account. This may take a couple of minutes, and will
#'finish by loading the URL in your browser and returning the URL in the R
#'Console.
#'
#'
#'If you are re-uploading the BIO-WELL survey for any reason, look out for a
#'message to confirm this by running 'Y' in the Console.
#'
#'
#'The URL can now be shared to participants for online access to your BIO-WELL
#'survey. Responses will be remotely saved to your Dropbox account.You can check
#'the status of your App on the posit Shiny Server app dashboard.
#'
#'@returns a character string, the URL to your deployed BIO-WELL survey Shiny
#'  App.
#'@export
#'@examplesIf interactive()
#'
#'BW_app_path <- paste0(tempdir(),"/my_app_name_1")
#'
#'dir.create(BW_app_path) # Store your "app.R" script here
#'
#'BIOWELL::create_URL(BW_app_path = paste0(tempdir(),"/my_app_name_1"),
#'                    BW_app_name = "my_app_name_1",
#'                    Shiny_Server_user = 'your_username_here',
#'                    Shiny_Server_token = 'your_token_here',
#'                    Shiny_Server_secret = 'your_secret_here')
#'
#'

create_URL <- function(BW_app_path,
                       BW_app_name,
                       Shiny_Server_user,
                       Shiny_Server_token,
                       Shiny_Server_secret) {

  if(!dir.exists(BW_app_path)){stop("Cannot find BIO-WELL app folder.")}

  accounts <- rsconnect::accounts()

  if (!Shiny_Server_user %in% accounts$name) {

    rsconnect::setAccountInfo(name = Shiny_Server_user,
                              token = Shiny_Server_token,
                              secret = Shiny_Server_secret)
  }

  files <- list.files(BW_app_path)

  # Check for app R script

  if (!"app.R" %in% files) {
    stop("No app.R file in BIO-WELL app folder. See details for guidance.")
  }

  # Check for Dropbox token directory

  directories <- list.dirs(BW_app_path, full.names = F)

  if (!".secrets" %in% directories) {
    stop("No .secrets folder in BIO-WELL app folder. See activate_dropbox().")
  }

  files <- list.files(paste0(BW_app_path,"/.secrets"))

  # Check for Dropbox token in .secrets folder

  if (!"dropbox_token.rds" %in% files) {
    stop("No dropbox_token.rds in BIO-WELL .secrets. See activate_dropbox().")
  }

  message("Deploying survey - be patient this will take a couple of minutes")

  URL <- utils::capture.output(rsconnect::deployApp(appTitle = BW_app_name,
                                                    BW_app_path,
                                                   account = Shiny_Server_user))

  # Get URL from deployApp output.

  URL <- gsub("Application successfully deployed to ",
              "",
              URL[grep("Application successfully deployed", URL)])

  if (length(URL) == 0) {

    stop("Error deploying survey - please read function details & vignettes")

  }

  return(URL)

}
