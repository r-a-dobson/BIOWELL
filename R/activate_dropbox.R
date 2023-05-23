
library(rdrop2)

activate_dropbox<-function(Dropbox_App_Name, Dropbox_App_Key, Dropbox_App_Secret,BW_app_path){

  message("Ensure that you have followed steps given in Vignette 1")

  .dstate <- new.env(parent = emptyenv())

  drop_auth_RT <- function (new_user1 = FALSE,
                            Dropbox_App_Key1 = Dropbox_App_Key,
                            Dropbox_App_Secret1 = Dropbox_App_Secret,
                            cache1 = TRUE,
                            rdstoken1 = NA,
                            Dropbox_App_Name1=Dropbox_App_Name)
  {

    dropbox <- httr::oauth_endpoint(authorize = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline",
                                    access = "https://api.dropbox.com/oauth2/token")
    # added "?token_access_type=offline" to the "authorize" parameter so that it can return an access token as well as a refresh token

    dropbox_app <- httr::oauth_app(Dropbox_App_Name1, Dropbox_App_Key1, Dropbox_App_Secret1)

    dropbox_token <- httr::oauth2.0_token(dropbox, dropbox_app,
                                          cache = cache1)
    if (!inherits(dropbox_token, "Token2.0")) {
      stop("something went wrong, try again")
    }


    .dstate$token <- dropbox_token
  }


  refreshable_token <- drop_auth_RT()

  if(refreshable_token$can_refresh()){
  dir.create(paste0(BW_app_path,"/.secrets"))
  saveRDS(refreshable_token,file=paste0(BW_app_path,"/.secrets/dropbox_token.rds"))
  return(refreshable_token)}

  if(!refreshable_token$can_refresh()){
    error("Error creating refreshable token")}}


