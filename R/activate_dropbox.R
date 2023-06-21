#' Activate Dropbox access token
#'
#'Function to generate a long-lived Dropbox access token for BIO-WELL survey
#'Shiny App.
#'@param Dropbox_App_Name a character string, the name given to your Dropbox
#'  App.
#'@param Dropbox_App_Key a character string, the Dropbox App Key. See details
#'  for more information.
#'@param Dropbox_App_Secret a character string, the Dropbox App Secret. See
#'  details for more information.
#'@param BW_app_path a character string, the path to your BIO-WELL Shiny App
#'  folder. See details for more information.
#'@details
#'
#'When your BIO-WELL survey Shiny App is hosted on the [Shiny
#'Server](https://posit.co/products/open-source/shinyserver/), you will need to
#'save participant responses to a remote [Dropbox folder](https://www.dropbox.com/home).
#'
#'First, you must register for a free Dropbox account. Then, to generate a
#'long-lived access token (over four hours) for your BIO-WELL survey, you will
#'need to set up a Dropbox App within your Dropbox account. It is
#'important that certain Dropbox App settings are edited before running
#'`activate_dropbox()`, including:
#'
#'+ Under "OAUTH2: Redirect URIs" add "http://localhost:1410/"
#'+ Under "Permissions" tick for access to Dropbox folder to read and write files.
#'
#'Full details on completing these changes are detailed in Vignette 1 within the
#'`BIOWELL` package. Then you can generate your Dropbox App's Key and Secret in
#'the settings tab.
#'
#'Once you have created a Dropbox App, the details (name, Key and Secret) can be
#'plugged into `activate_dropbox()`, alongside the path to your BIO-WELL survey
#'Shiny App folder (argument `BW_app_path`). This is a folder that you need to
#'create on your local computer for storing your BIO-WELL survey code (as an R
#'script entitled "app.R") and the Dropbox token (automatically done by
#'`activate_dropbox()`) for upload to the Shiny Server.
#'
#'This function will generate a long-lived, refreshable Dropbox access token and
#'store it within a ".secrets" folder within your BIO-WELL survey Shiny App
#'folder. This enables the token to be accessible whilst hosted on the Shiny
#'Server.
#'
#'Please see Vignette 1 in the `BIOWELL` package for full guidance on this
#'function.
#'
#'@returns Saves a long-lived Dropbox token to your BIO-WELL survey Shiny App
#'  folder.
#'@export
#'@examplesIf interactive()
#'
#'BW_app_path <- paste0(tempdir(),"/my_app_name_1")
#'
#'dir.create(BW_app_path) # Create App directory to store token
#'
#'activate_dropbox(Dropbox_App_Name = "my_app_name_1",
#'                 Dropbox_App_Key = "your_app_key_here",
#'                 Dropbox_App_Secret = "your_app_secret_here",
#'                 BW_app_path = BW_app_path)
#'
activate_dropbox <- function(Dropbox_App_Name,
                             Dropbox_App_Key,
                             Dropbox_App_Secret,
                             BW_app_path) {

  message("Ensure that you have followed prior steps given in Vignette 1")

  if(!dir.exists(BW_app_path)){stop("Cannot find BIO-WELL app folder.")}

  .dstate <- new.env(parent = emptyenv())

  auth <- "https://www.dropbox.com/oauth2/authorize?token_access_type=offline"
  access <- "https://api.dropbox.com/oauth2/token"

  dropbox <- httr::oauth_endpoint(authorize = auth,
                                  access = access)

  dropbox_app <- httr::oauth_app(Dropbox_App_Name,
                                 Dropbox_App_Key,
                                 Dropbox_App_Secret)

  dropbox_token <- httr::oauth2.0_token(dropbox,
                                        dropbox_app,
                                        cache = TRUE)

  if (!inherits(dropbox_token, "Token2.0")) {
    stop("Error creating refreshable token. See advice in Vignette 1.")
  }

  .dstate$token <- dropbox_token

  refreshable_token <-  .dstate$token

  if (refreshable_token$can_refresh()) {

    dir.create(paste0(BW_app_path, "/.secrets"))

    saveRDS(refreshable_token,
            file = paste0(BW_app_path, "/.secrets/dropbox_token.rds"))

    return(refreshable_token)
  }

  if (!refreshable_token$can_refresh()) {
    stop("Error creating refreshable token. See advice in Vignette 1.")
  }

}
