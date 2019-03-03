# https://code.markedmondson.me/launch-rstudio-server-google-cloud-in-two-lines-r/
# steps:
# go to google cloud platform, create a project or login to your project
# go to IAM & Admin -> Service Accounts
# Click "Create Service Account", select a role for the account 
# create a key when the service account is being created. It will be downloaded
# as a json file.
# Once the service account has been created, enable access for APIs required
# go to "APIs and Services" in the left bar menu and navigate to "Library"
# enable apis such as BigQuery, Google Analytics, Compute Engine, Kubernetes, etc
# Navigate back to IAM & Admin -> Service Accounts

setwd("C:/Users/User/Documents/cloudy-rstudio")
readRenviron("C:/Users/User/Documents/cloudy-rstudio/.Renviron")
library(googleComputeEngineR)

#project <- "rstudio-cloud-69"
#zone <- "us-central1-a"
#account_key <- "C:/Users/User/Documents/cloudy-rstudio/Rstudio cloud-3c97342e81c9.json"

#Sys.setenv(
#  GCE_AUTH_FILE= account_key,
#  GCE_DEFAULT_PROJECT_ID= project,
#  GCE_DEFAULT_ZONE= zone
#)
#Sys.setenv("GCE_AUTH_FILE" = "C:/Users/User/Documents/cloudy-rstudio/Rstudio cloud-3c97342e81c9.json")

#gce_global_project(project)
#gce_global_zone(zone)

default_project <- gce_get_project()


vm <- gce_vm(template = "rstudio",
             name = "my-rstudio",
             username = "kevin", password = "kev6969",
             predefined_type = "f1-micro")


