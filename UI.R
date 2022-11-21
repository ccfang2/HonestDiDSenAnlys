
# Options
options(warn = -1)
options(spinner.type=5)

# Import Libraries
library(HonestDiD)
library(did)
library(fixest)
library(dplyr)
library(mvtnorm)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinydisconnect)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)
#library(lubridate)
library(Rglpk)
library(WriteXLS)
library(readxl)
library(tools)


source("Tabs.R")

# UI 
UI <- shinyUI({
  fluidPage(
   
    # setBackgroundColor(
    #   color = c("#F7FBFF", "#2171B5"),
    #   gradient = "linear",
    #   direction = "bottom"
    # ),
    # useShinyjs(),
    
    # Isend messages
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});')),
              tags$style(HTML("hr {border-top: 1px solid #000000;}"))),

    disconnectMessage(
      text = "Your session timed out, reload the application.",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.3,
      refreshColour = "brown"
    ),

    # Navbar structure for UI
    navbarPage("HonestDiD: Sensitivity Analysis", theme = shinytheme("lumen"),
               Home,
               Examples,
               OwnData,
               Help,
               More
               )
    )
  })
  
  