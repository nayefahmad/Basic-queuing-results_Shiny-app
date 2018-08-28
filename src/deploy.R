
#************************************
# App deployment 
#************************************

library("rsconnect")

rsconnect::deployApp(here("src"), appName = "Basic-Queuing-Results")
