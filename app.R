rm(list = ls())

# Import UI and Server Functions
source('Server.R')
source('UI.R')

# button_color_css <- "
#     #DivCompClear, #FinderClear, #EnterTimes{
#     /* Change the background color of the update button
#     to blue. */
#     background: DodgerBlue;
#     /* Change the text size to 15 pixels. */
#     font-size: 15px;}"

# Run Shiny App
shinyApp(
  ui = UI,
  server = server
)




