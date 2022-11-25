
# Home Panel
Home <- tabPanel("Home", icon = icon("house"),
                 fluidRow(
                   column(6, 
                          h4(p("About this Shiny App")),
                          h5(p("This application is intended to facilitate the sensitivity analysis proposed in ", a("Rambachan and Roth (2022).", href="https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf"), "This is a useful tool when you are uncertain about if parallel trends hold in your difference-in-difference setting. It reports confidence sets for causal effect of interest under a variety of possible restrictions on the underlying trends, and is helpful in telling the degree of violation on parallel trends that is needed to make a particular causal conclusion.")),
                          h5(p("In", strong("[ Examples ], "), "you are able to conduct sensitivity analysis for a paper you select. Once you follow the steps to complete the setup, you will get outputs including an event-study plot, a sensitivity analysis plot, sensitivity result data which is used to draw the sensitivity plot, and R code which you can copy and paste in your own R console to get the same outputs. You can also revise the R code to further tailor your analysis. All outputs are downloadable by a click of button. For links of papers in [ Examples ], please go to [ More ] > [ Resources ].")),
                          h5(p("In", strong("[ Analyze Your Own Data ], "), "you can upload a data file containing your own estimated event-study coefficients, variance-covariance matrix and some other needed information to perform a sensitivity analysis for your own research. You will also be able to download your outputs by a click of button. R code that can be used to replicate the analysis in your own R console is provided as well.")),
                          h5(p("If you haven't estimated coefficients or variance-covariance matrix yet, then you need to go to ", strong("[ Quick Help ],"), "where you can get your estimation quickly. It consists of two subpanels:", strong("[ Non-Staggered DiD ]"), "and", strong("[ Staggered DiD ]."), "In [ Non-Staggered DiD ], coefficients and matrix are estimated using two-way fixed effects, while in [ Staggered DiD ], estimation is done with group-time average treatment effects, as proposed in ", a("Callaway and Sant'Anna (2021).", href="https://www.sciencedirect.com/science/article/abs/pii/S0304407620303948?via%3Dihub"), "If you have more specifications on your model which cannot be set up in this app, please revise the output R code accordingly and run it locally. The estimation result which is available for download can be directly used as the data file you need to upload for sensitivity analyses in [ Analyze Your Own Data ]."))#,
                         # h5(p("It may take more than 2 minutes for each sensitivity analysis to run, depending on your input data, the specific setup you choose, and your machine capability. R codes that produce your sensitivity result are provided alongside your output plots."))
                         ),
                   column(6,
                          h4(p("Instruction Video")),
                          box(
                            width = 12, 
                            HTML('<iframe width="595" height="335" src="https://www.youtube.com/embed/PyY-2ptiLTU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen> </iframe>'),
                            p("Note: The layout of Shiny app shown in this video may be slightly different from its latest version.")
                            )
                          )  
                   ),
                 hr(),
                 h5(p("I hope you find this app useful and/or interesting. Any comments or questions are welcome at", a("ccfang[at]uni-bonn.de",href="mailto:ccfang@uni-bonn.de"),", and you are encouraged to report any issues", a("here.", href="https://github.com/ccfang2/HonestDiDSenAnlys/issues"))),
                 h5("Built with",
                    img(src = "shiny.png", height = "30px"),
                    "by",
                    img(src = "RStudio-Logo-Flat.png", height = "30px"),
                    ".")
)
                   
                   
# Examples Panel
Examples <- tabPanel("Examples", fluid = TRUE, icon = icon("magnifying-glass"),
                     # tags$style(button_color_css),
                     # Examples sidebar layout with an input and output definitions
                     sidebarLayout(
                       # Examples Sidebar Panel
                       sidebarPanel(
                         p(h4("Sensitivity Analysis for a Selected Paper")),
                         hr(),
                         # Step 1
                         p(strong(span("[ Step 1 ] : ", style="color:black"))),
                         selectInput("paper",
                                     label="Select a paper to analyze",
                                     choices = list("Bailey and Goodman-Bacon (2015)"=1,
                                                    "Benzarti and Carloni (2019)"=2,
                                                    "Bosch and Campos-Vazquez (2014)"=3,
                                                    "Deschenes et al. (2017)"=4,
                                                    "Fitzpatrick and Lovenheim (2014)"=5,
                                                    "Gallagher (2014)"=6,
                                                    "He and Wang (2017)"=7,
                                                    "Kuziemko et al. (2018)"=8,
                                                    "Lafortune et al. (2018)"=9,
                                                    "Lovenheim and Willen (2019)"=10,
                                                    "Markevich and Zhuravskaya (2018)"=11,
                                                    "Tewari (2014)"=12,
                                                    "Ujhelyi (2014)"=13#,
                                                    #"Deryugina (2017)"=14
                                                    ),
                                     selected = 1),
                         # Step 2
                         tags$div(title="At least one base delta must be selected, while sign and monotonicity restrictions are optional. Importantly, if Bounding Relative Magnitudes is included in the base delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions.",
                                  p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                         fluidRow(column(4,
                                         # Select base choice of Delta
                                         checkboxGroupInput("example_delta","Base choice of Delta",
                                                            choices = list("Bounding Relative Magnitudes"=2,   
                                                                           "Smoothness restriction"=1), 
                                                            selected = 2)  
                                         ),
                                  column(4, offset = 0,
                                         # Select Sign restrictions
                                         radioButtons("example_sign","Sign restriction",
                                                      choices = list("None"=1,
                                                                     "Positive"=2,
                                                                     "Negative"=3),
                                                      selected = 1)
                                         ),
                                  column(4, offset = 0,
                                         # Select Monotonicity restrictions
                                         radioButtons("example_monotonicity","Monotonicity restriction",
                                                      choices = list("None"=1,
                                                                     "Increasing"=2,
                                                                     "Decreasing"=3),
                                                      selected = 1)
                                         )
                                  ),
                         # Step 3
                         tags$div(title="If 'Smoothness Restriction' is the only base Delta chosen at Step [2], then the default method here is 'Fixed Length Confidence Intervals'. \n \n If 'Bounding Relative Magnitudes' is included in the base Delta, then the default method here is 'Conditional Least Favorable Hybrid'. \n \n Fixed Length Confidence Intervals and Conditional FLCI Hybrid only work when smoothness restriction is the only base restriction at [ Step 2 ].",
                                  p(strong(span("[ Step 3 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                         conditionalPanel(
                           condition = "input.example_delta == '1'", #JS expression
                           radioButtons("example_method_sd","Select a method to construct confidence interval",
                                        choices = list("Conditional"="Conditional",
                                                       "Conditional Least Favorable Hybrid"="C-LF",
                                                       "Fixed Length Confidence Intervals"="FLCI",
                                                       "Conditional FLCI Hybrid"="C-F"),
                                        selected = "FLCI")
                         ),
                         conditionalPanel(
                           condition = c("input.example_delta.includes('2') | input.example_delta == ''"), #JS expression
                           radioButtons("example_method_rm","Select a method to construct confidence interval",
                                        choices = list("Conditional"="Conditional",
                                                       "Conditional Least Favorable Hybrid"="C-LF",
                                                       "Fixed Length Confidence Intervals"="FLCI",
                                                       "Conditional FLCI Hybrid"="C-F"),
                                        selected = "C-LF")
                         ),
                         # Step 4
                         tags$div(title="If ONLY smoothness restriction is selected as the base Delta, the vector you enter below is a vector of M values, which is needed to define smoothness restriction. \n \n Otherwise, if bounding relative magnitudes is included in base Delta, then the vector you enter below is a vector of Mbar values, which is needed to define the restriction of bounding relative magitudes. \n \n There are two ways to enter the vector. You can either enter an arbitrary vector with numbers being seperated by commas (e.g., 0.25, 0.5, 0.75, 1, 1.25), or enter a sequence with lower, upper bounds and step (e.g., lower=0.25, upper=1.25, step=0.25). All numbers you enter must be non-negative. It is NOT allowed to enter a vector in BOTH ways simultaneously. \n \n As a special case, if you are unsure about how large M or Mbar could be and prefer to using default values, then it is HIGHLY suggested to just leave them blank. The default values of Mbars are already visible at [ Step 4 ]. For default values of M, please go to [ More ] > [ FAQ ] for details.",
                                  p(strong(span("[ Step 4 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                         conditionalPanel(
                           condition = "input.example_delta == '1'", # JS expression
                           p("Enter a vector of M to define the base Delta you select at [ Step 2 ]"),
                           p("EITHER enter an arbitrary vector"),
                           textInput("example_m_textinput", NULL, 
                                     value = ""),
                           p("OR enter the vector by defining a sequence"),
                           fluidRow(
                             column(3,numericInput("example_m_lower", "Lower", value = "")),
                             column(3,numericInput("example_m_upper", "Upper", value = "")),
                             column(6,numericInput("example_m_step", "Step", value = ""))
                           )
                         ),
                         conditionalPanel(
                           condition = c("input.example_delta.includes('2') | input.example_delta == ''"), # JS expression
                           p("Enter a vector of Mbar to define the base Delta you select at [ Step 2 ]"),
                           p("EITHER enter an arbitrary vector"),
                           textInput("example_mbar_textinput", NULL,
                                     value = ""),
                           p("OR enter the vector by defining a sequence"),
                           fluidRow(
                             column(3,numericInput("example_mbar_lower", "Lower", value = 0.25)),
                             column(3,numericInput("example_mbar_upper", "Upper", value = 1.25)),
                             column(6,numericInput("example_mbar_step", "Step", value = 0.25))
                           )
                         ),
                         # Step 5
                         tags$div(title=" Examples: \n 1 indicates the first post-treatment period; \n 1,2 indicates the first two post-treatment periods; \n 1,2,3 means indicates the first three post-treatment periods; \n Leave it blank if you are interested in all post-treatment periods. \n \n This app will give the same weight to the period(s) that you enter when computing weight vector. It means that the average causal effect over those period(s) will be considered, as this is the most common parameter of interest for researchers. \n If you need to give different weights, please revise the produced R code accordingly and run it locally.",
                                  p(strong(span("[ Step 5 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                         textInput("example_lvec", "Enter a vector of indices to indicate the post-treatment period(s) of your interest", 
                                   value = "1"),
                         # Step 6
                         p(strong(span("[ Step 6 ] : ", style="color:black"))),
                         sliderInput("example_alpha", "Select alpha, the level of significance", min = 0, 
                                     max = 1, value = 0.05),
                         # Step 7
                         tags$div(title="If Yes, the confidence intervals in parallel will be constructed. This uses the Foreach package and doParallel package. \n \n It is suggested to select No as is given by default and to increase computational efficiency.",
                                  p(strong(span("[ Step 7 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                         radioButtons("example_parallel","Indicate whether to construct robust confidence intervals in parallel",
                                      choices = list("Yes"=TRUE,
                                                     "No (Suggested)"=FALSE),
                                      selected = FALSE),
                         # Step 8
                         useShinyjs(),
                         tags$div(title="\n You are HIGHLY suggested to leave lower and upper bounds blank, so default values will be used. \n \n However, this step will be super helpful when the lower or upper bounds of your output confidence sets turn out to be exactly the same for different Mbar. Then, you need to adjust the bounds of grid for test inversion here in order to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details. \n \n Also, you are HIGHLY suggested to leave number of grid points as is given (=1000), but it's okay to change it for computational reasons.",
                                  hidden(p(id="example_grid_hint",strong(span("[ Step 8 ] : ", style="color:black")), span(" [ Hover to see hints ] ")))),
                         hidden(p(id="example_grid_note","Enter the lower and upper bounds of grid as well as the number of grid points which will be used for underlying test inversion because the restriction of bounding relative magnitudes is included in your base Delta at [ Step 2 ].")),
                           fluidRow(
                             column(3,hidden(numericInput("example_grid_lb","Lower", value = ""))),
                             column(3,hidden(numericInput("example_grid_ub","Upper", value = ""))),
                             column(6,hidden(numericInput("example_grid_points","Number of grid points", value = 1000, min =1, step=1)))
                           ),
                         hr(),
                         
                         # Start and Clear Buttons
                         fluidRow(
                           column(6,actionButton("example_start", "Start")),
                           column(6,actionButton("example_reset", "Reset"))
                           )
                         ),
                       
                       # Examples Main Panel
                       mainPanel(
                         fluidRow(
                           column(10,
                                  h4(textOutput("example_currentTime")),
                                  p("The clock will be frozen after 'Start' is clicked, but it will return to correct time after outputs pop out. The suspension of clock is a signal that code is running. Please be reminded that it may take more than 2 minutes for each sensitivity analysis to run, depending on the selected paper, the restrictions you choose, and your machine capability. The more M or Mbar you enter, the slower it will be. The process may take longer if you include bounding relative magnitudes in your restriction, so please be patient."),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Event Study Plot", 
                                                       fluidRow(
                                                         br(),
                                                         column(12,
                                                                shinycssloaders::withSpinner(plotOutput("example_betaplot"))
                                                                )
                                                         )
                                                       ),
                                              tabPanel("Sensitivity Plot", 
                                                       fluidRow(
                                                         br(),
                                                         column(12, 
                                                                shinycssloaders::withSpinner(plotOutput("example_ssplot"))
                                                                ),
                                                         br(),
                                                         column(12,
                                                                shinycssloaders::withSpinner(p(textOutput("example_note")))
                                                                ),
                                                         br(),
                                                         column(12,
                                                                shinycssloaders::withSpinner(p(textOutput("example_grid_warning")))
                                                                )
                                                         )
                                                       ),
                                              tabPanel("Sensitivity Result", 
                                                       fluidRow(
                                                         br(),
                                                         column(12,
                                                                shinycssloaders::withSpinner(tableOutput("example_ssdata")),
                                                                style = "overflow-y: scroll;overflow-x: scroll;")
                                                         )
                                                       ),
                                              tabPanel("R Code", 
                                                       shinycssloaders::withSpinner(verbatimTextOutput("example_code")),
                                                       tags$head(tags$style("#example_code{overflow-y:scroll;overflow-x:scroll;}"))
                                                       )
                                              )
                                  ),
                           column(2,
                                  p(strong(span("[ Downloads ]", style="color:black"))),
                                  # tags$style(".btn {width: 80%}"),
                                  downloadButton("example_betaplotdownload","Event Study Plot"),
                                  downloadButton("example_ssplotdownload","Sensitivity Plot"),
                                  downloadButton("example_ssdatadownload","Sensitivity Result"),
                                  downloadButton("example_inputdownload","Data File"),
                                  br(),
                                  br(),
                                  p(strong(span("[ Reload ]", style="color:black"))),
                                  actionButton("example_disconnect", "Disconnect the App")
                                  )
                           )
                         )
                       )
                     )


# Own Data Panel
OwnData <- tabPanel("Analyze Your Own Data", fluid = TRUE, icon = icon("pen-to-square"),
                    # tags$style(button_color_css),
                    # OwnData sidebar layout with an input and output definitions
                    sidebarLayout(
                      # OwnData Input Panel
                      sidebarPanel(
                        p(h4("Sensitivity Analysis for Your Own Data")),
                        hr(),
                        h5("You must upload a data file containing estimated event-study coefficients and variance-covariance matrix. If you don't have them, please go to 'Quick Help' for more instructions."),
                        hr(),
                        # Step 1
                        tags$div(title="File must be in xlsx format, and Click [Example File] to check the exact format. There must be 6 sheets, each of which gives an object which is needed for sensitivity analysis. \n \n beta: vector of estimated event study coefficients; \n sigma: Covariance matrix of event study coefficients; \n tVec: vector that contains the time periods associated with the event study coefficients; \n referencePeriod: scalar that shows the reference period; \n prePeriodIndices: indices of pre treatment periods; \n postPeriodIndices: indices of post treatment periods. \n \n Names of variables and titles of sheets shouldn't be changed.",
                                 p(strong(span("[ Step 1 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        uiOutput("owndata_fileupload"),
                        # Step 2
                        tags$div(title="At least one base delta must be selected, while sign and monotonicity restrictions are optional. Importantly, if Bounding Relative Magnitudes is included in the base delta, it is not allowed to select both shape (aka monotonicity) and sign restrictions as additions.",
                                 p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        fluidRow(column(4,
                                        # Select base choice of Delta
                                        checkboxGroupInput("owndata_delta","Base choice of Delta",
                                                           choices = list("Bounding Relative Magnitudes"=2,   
                                                                          "Smoothness restriction"=1),  
                                                           selected = 2)  
                                        ),
                                 column(4, offset = 0,
                                        # Select Sign restrictions
                                        radioButtons("owndata_sign","Sign restriction",
                                                     choices = list("None"=1,
                                                                    "Positive"=2,
                                                                    "Negative"=3),
                                                     selected = 1)
                                        ),
                                 column(4, offset = 0,
                                        # Select Monotonicity restrictions
                                        radioButtons("owndata_monotonicity","Monotonicity restriction",
                                                     choices = list("None"=1,
                                                                    "Increasing"=2,
                                                                    "Decreasing"=3),
                                                     selected = 1)
                                        )
                                 ),
                        # Step 3
                        tags$div(title="If 'Smoothness Restriction' is the only base Delta chosen at Step [2], then the default method here is 'Fixed Length Confidence Intervals'. \n \n If 'Bounding Relative Magnitudes' is included in the base Delta, then the default method here is 'Conditional Least Favorable Hybrid'. \n \n Fixed Length Confidence Intervals and Conditional FLCI Hybrid only work when smoothness restriction is the only base restriction at [ Step 2 ].",
                                 p(strong(span("[ Step 3 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        conditionalPanel(
                          condition = "input.owndata_delta == '1'", #JS expression
                          radioButtons("owndata_method_sd","Select a method to construct confidence interval",
                                       choices = list("Conditional"="Conditional",
                                                      "Conditional Least Favorable Hybrid"="C-LF",
                                                      "Fixed Length Confidence Intervals"="FLCI",
                                                      "Conditional FLCI Hybrid"="C-F"),
                                       selected = "FLCI")
                        ),
                        conditionalPanel(
                          condition = c("input.owndata_delta.includes('2') | input.owndata_delta == ''"), #JS expression
                          radioButtons("owndata_method_rm","Select a method to construct confidence interval",
                                       choices = list("Conditional"="Conditional",
                                                      "Conditional Least Favorable Hybrid"="C-LF",
                                                      "Fixed Length Confidence Intervals"="FLCI",
                                                      "Conditional FLCI Hybrid"="C-F"),
                                       selected = "C-LF")
                        ),
                        # Step 4
                        tags$div(title="If ONLY smoothness restriction is selected as the base Delta, the vector you enter below is a vector of M values, which is needed to define smoothness restriction. \n \n Otherwise, if bounding relative magnitudes is included in base Delta, then the vector you enter below is a vector of Mbar values, which is needed to define the restriction of bounding relative magitudes. \n \n There are two ways to enter the vector. You can either enter an arbitrary vector with numbers being seperated by commas (e.g., 0.25, 0.5, 0.75, 1, 1.25), or enter a sequence with lower, upper bounds and step (e.g., lower=0.25, upper=1.25, step=0.25). All numbers you enter must be non-negative. It is NOT allowed to enter a vector in BOTH ways simultaneously. \n \n As a special case, if you are unsure about how large M or Mbar could be and prefer to using default values, then it is HIGHLY suggested to just leave them blank. The default values of Mbars are already visible at [ Step 4 ]. For default values of M, please go to [ More ] > [ FAQ ] for details.",
                                 p(strong(span("[ Step 4 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        conditionalPanel(
                          condition = "input.owndata_delta == '1'", # JS expression
                          p("Enter a vector of M to define the base Delta you select at [ Step 2 ]"),
                          p("EITHER enter an arbitrary vector"),
                          textInput("owndata_m_textinput", NULL, 
                                    value = ""),
                          p("OR enter the vector by defining a sequence"),
                          fluidRow(
                            column(3,numericInput("owndata_m_lower", "Lower", value = "")),
                            column(3,numericInput("owndata_m_upper", "Upper", value = "")),
                            column(6,numericInput("owndata_m_step", "Step", value = ""))
                          )
                        ),
                        conditionalPanel(
                          condition = c("input.owndata_delta.includes('2') | input.owndata_delta == ''"), # JS expression
                          p("Enter a vector of Mbar to define the base Delta you select at [ Step 2 ]"),
                          p("EITHER enter an arbitrary vector"),
                          textInput("owndata_mbar_textinput", NULL,
                                    value = ""),
                          p("OR enter the vector by defining a sequence"),
                          fluidRow(
                            column(3,numericInput("owndata_mbar_lower", "Lower", value = 0.25)),
                            column(3,numericInput("owndata_mbar_upper", "Upper", value = 1.25)),
                            column(6,numericInput("owndata_mbar_step", "Step", value = 0.25))
                          )
                        ),
                        # Step 5
                        tags$div(title=" Examples: \n 1 indicates the first post-treatment period; \n 1,2 indicates the first two post-treatment periods; \n 1,2,3 means indicates the first three post-treatment periods; \n Leave it blank if you are interested in all post-treatment periods. \n \n This app will give the same weight to the period(s) that you enter when computing weight vector. It means that the average causal effect over those period(s) will be considered, as this is the most common parameter of interest for researchers. \n If you need to give different weights, please revise the produced R code accordingly and run it locally.",
                                 p(strong(span("[ Step 5 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        textInput("owndata_lvec", "Enter a vector of indices to indicate the post-treatment period(s) of your interest", 
                                  value = "1"),
                        # Step 6
                        p(strong(span("[ Step 6 ] : ", style="color:black"))),
                        sliderInput("owndata_alpha", "Select alpha, the level of significance", min = 0, 
                                    max = 1, value = 0.05),
                        # Step 7
                        tags$div(title="If Yes, the confidence intervals in parallel will be constructed. This uses the Foreach package and doParallel package. \n \n It is suggested to select No as is given by default and to increase computational efficiency.",
                                 p(strong(span("[ Step 7 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                        radioButtons("owndata_parallel","Indicate whether to construct robust confidence intervals in parallel",
                                     choices = list("Yes"=TRUE,
                                                    "No (Suggested)"=FALSE),
                                     selected = FALSE),
                        # Step 8
                        useShinyjs(),
                        tags$div(title="\n You are HIGHLY suggested to leave lower and upper bounds blank, so default values will be used. \n \n However, this step will be super helpful when the lower or upper bounds of your output confidence sets turn out to be exactly the same for different Mbar. Then, you need to adjust the bounds of grid for test inversion here in order to get correct confidence sets. Please go to [ More ] > [ FAQ ] for details. \n \n Also, you are HIGHLY suggested to leave number of grid points as is given (=1000), but it's okay to change it for computational reasons.",
                                 hidden(p(id="owndata_grid_hint",strong(span("[ Step 8 ] : ", style="color:black")), span(" [ Hover to see hints ] ")))),
                        hidden(p(id="owndata_grid_note","Enter the lower and upper bounds of grid as well as the number of grid points which will be used for underlying test inversion because the restriction of bounding relative magnitudes is included in your base Delta at [ Step 2 ].")),
                        fluidRow(
                          column(3,hidden(numericInput("owndata_grid_lb","Lower", value = ""))),
                          column(3,hidden(numericInput("owndata_grid_ub","Upper", value = ""))),
                          column(6,hidden(numericInput("owndata_grid_points","Number of grid points", value = 1000, min =1, step=1)))
                        ),
                        hr(),
                        
                        # Start and Clear Buttons
                        fluidRow(
                          column(6,actionButton("owndata_start", "Start")),
                          column(6,actionButton("owndata_reset", "Reset"))
                        )
                      ),
                      
                      # OwnData Main Panel
                      mainPanel(
                        fluidRow(
                          column(10,
                                 h4(textOutput("owndata_currentTime")),
                                 p("The clock will be frozen after 'Start' is clicked, but it will return to correct time after outputs pop out. The suspension of clock is a signal that code is running. Please be reminded that it may take more than 2 minutes for each sensitivity analysis to run, depending on the size of your data, the restrictions you choose, and your machine capability. The more M or Mbar you enter, the slower it will be. The process may take longer if you include bounding relative magnitudes in your restriction, so please be patient."),
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Event Study Plot", 
                                                      fluidRow(
                                                        br(),
                                                        column(12,
                                                               shinycssloaders::withSpinner(plotOutput("owndata_betaplot"))
                                                        )
                                                      )
                                             ),
                                             tabPanel("Sensitivity Plot", 
                                                      fluidRow(
                                                        br(),
                                                        column(12, 
                                                               shinycssloaders::withSpinner(plotOutput("owndata_ssplot"))
                                                               ),
                                                        br(),
                                                        column(12,
                                                               shinycssloaders::withSpinner(p(textOutput("owndata_note")))
                                                               ),
                                                        br(),
                                                        column(12,
                                                               shinycssloaders::withSpinner(p(textOutput("owndata_grid_warning")))
                                                               )
                                                      )
                                             ),
                                             tabPanel("Sensitivity Result", 
                                                      fluidRow(
                                                        br(),
                                                        column(12,
                                                               shinycssloaders::withSpinner(tableOutput("owndata_ssdata")),
                                                               style = "overflow-y: scroll;overflow-x: scroll;")
                                                      )
                                             ),
                                             tabPanel("R Code", 
                                                      shinycssloaders::withSpinner(verbatimTextOutput("owndata_code")),
                                                      tags$head(tags$style("#example_code{overflow-y:scroll;overflow-x:scroll;}"))
                                             )
                                 )
                          ),
                          column(2,
                                 p(strong(span("[ Downloads ]", style="color:black"))),
                                 # tags$style(".btn {width: 80%}"),
                                 downloadButton("owndata_betaplotdownload","Event Study Plot"),
                                 downloadButton("owndata_ssplotdownload","Sensitivity Plot"),
                                 downloadButton("owndata_ssdatadownload","Sensitivity Result"),
                                 br(),
                                 br(),
                                 p(strong(span("[ Reload ]", style="color:black"))),
                                 actionButton("owndata_disconnect", "Disconnect the App")
                          )
                        )
                      )
                    )
)


# Help Panel
Help <- navbarMenu("Quick Help", icon = icon("compass"),
                   # NonStagger DiD Tab
                   tabPanel("Non-Staggered DiD", fluid = TRUE,
                            # Nonstagger sidebar layout with an input and output definitions
                            sidebarLayout(
                              # Nonstagger Input Panel
                              sidebarPanel(
                                p(h4("DiD Estimation in Non-Staggered Treatment Timing using Two Way Fixed Effects")),
                                hr(),
                                p(h5("If you don't have estimated event-study coefficients or variance-covariance matrix, and your DiD setup is in non-staggered timing, you could then get your TWFE estimation quickly by completing following steps. Once finished, download 'Nonstaggered TWFE' file as the data file you need to upload for sensitivity analysis.")),
                                hr(),
                                # Step 1
                                tags$div(title=" File must be in xlsx format, and click [Example File] to check the exact format. \n \n id: individual id; \n time: time variable; \n D: binary variable that indicates if an individual at certain time is treated (=1) or not (=0); \n y: target variable \n \n The names of variables above mustn't be changed.",
                                         p(strong(span("[ Step 1 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                uiOutput("nonstagger_fileupload"),
                                # Step 2
                                tags$div(title="\n OLS is a special case of GLM with normal (gaussian) error terms. Hence, the result from GLM with gaussian family is the same as that from OLS. Moreover, estimation from MLE with gaussian is also the same as that from OLS. \n \n For GLM and MLE, there are many other options of family, which you can select to fit your data. \n \n Those who are familiar with 'family' of estimation should know 'link function', which can be further defined to the selected 'family'. But to avoid any confusion, 'link function' is not configurable on the left pane. Please revise the produced R code and run it locally if you are interested. For all available 'link function', please go to [ More ] > [ FAQ ].",
                                         p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                fluidRow(
                                  column(7,
                                         radioButtons("nonstagger_est_model", label = "Select an estimation model",
                                                      choices = list("Ordinary Least Squares (OLS)"=1,
                                                                     "Generalized Linear Model (GLM)"=2,
                                                                     "Maximum Likelihood Estimation (MLE)"=3),
                                                      selected = 1)),
                                  useShinyjs(),
                                  column(5,
                                         hidden(radioButtons("nonstagger_glm", label="Select a family for GLM",
                                                      choices = list("Binomial"=1,
                                                                     "Gaussian"=2,
                                                                     "Gamma"=3,
                                                                     "Inverse Gaussian"=4,
                                                                     "Poisson"=5),
                                                      selected = 2)),
                                         hidden(radioButtons("nonstagger_mlm", label="Select a family for MLE",
                                                      choices = list("Poisson"=1,
                                                                     "Negative Binomial"=2,
                                                                     "Logit"=3,
                                                                     "Gaussian"=4),
                                                      selected = 4))
                                         )
                                  ),
                                # Step 3
                                p(strong(span("[ Step 3 ] : ", style="color:black"))),
                                numericInput("nonstagger_ref", "Enter a numeric value that indicates the reference period", 
                                             value = 2013),
                                # Step 4
                                p(strong(span("[ Step 4 ] : ", style="color:black"))),
                                sliderInput("nonstagger_alpha", "Select alpha, the level of significance for estimating confidence intervals in event-study plot", min = 0, 
                                            max = 1, value = 0.05),
                                hr(),
                                
                                # Start and Clear Buttons
                                fluidRow(
                                  column(6,actionButton("nonstagger_start", "Start")),
                                  column(6,actionButton("nonstagger_reset", "Reset"))
                                  )
                                ),
                              
                              # NonStagger Main Panel
                              mainPanel(
                                fluidRow(
                                  column(10,
                                         h4(textOutput("nonstagger_currentTime")),
                                         p(strong("[ WARNINGS ]"),": If you have more specifications (e.g., control variables, nonlinear constraints, instrumental variables) on your model which cannot be set up on the left pane, please revise the produced R code accordingly and run it locally. Please select a suitable estimation model for your data. You may NOT receive a warning if you select an unsuitable but feasible model."),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Event Study Plot", 
                                                              fluidRow(
                                                                br(),
                                                                column(12,
                                                                       shinycssloaders::withSpinner(plotOutput("nonstagger_betaplot"))
                                                                )
                                                              )
                                                     ),
                                                     tabPanel("R Code", 
                                                              shinycssloaders::withSpinner(verbatimTextOutput("nonstagger_code")),
                                                              tags$head(tags$style("#nonstagger_code{overflow-y:scroll;overflow-x:scroll;}"))
                                                     )
                                         )
                                  ),
                                  column(2,
                                         p(strong(span("[ Downloads ]", style="color:black"))),
                                         # tags$style(".btn {width: 80%}"),
                                         downloadButton("nonstagger_betaplotdownload","Event Study Plot"),
                                         downloadButton("nonstagger_outputdownload", "Nonstaggered TWFE"),
                                         br(),
                                         br(),
                                         p(strong(span("[ Reload ]", style="color:black"))),
                                         actionButton("nonstagger_disconnect", "Disconnect the App")
                                  )
                                )
                              )
                            )
                   ),
                   
                   # Stagger DiD Tab
                   tabPanel("Staggered DiD", fluid = TRUE,
                            # stagger sidebar layout with an input and output definitions
                            sidebarLayout(
                              # Stagger Input Panel
                              sidebarPanel(
                                p(h4("DiD Estimation in Staggered Treatment Timing using Group-Time Average Effects")),
                                hr(),
                                p(h5("If you don't have estimated study-event coefficients and variance-covariance matrix, and your DiD setup is in staggered timing, you could then get your Group-Time ATT estimation quickly by completing following steps. Once finished, download 'Staggered ATT' file as the data file you need to upload for sensitivity analysis.")),
                                hr(),
                                # Step 1
                                tags$div(title=" File must be in xlsx format, and click [Example File] to check the exact format. \n \n id: individual id; \n time: time variable; \n y: target variable; \n g: a variable that contains the first period when a particular individual is treated;  \n \n The names of variables above mustn't be changed.",
                                         p(strong(span("[ Step 1 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                uiOutput("stagger_fileupload"),
                                # Step 2
                                tags$div(title="If unbalanced panel is NOT allowed, all units where data is not observed in all periods will be dropped. Computations will then be faster.",
                                         p(strong(span("[ Step 2 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                radioButtons("stagger_unbalance", "Choose if the panel with respect to time and id is allowed to be unbalanced.", 
                                             choices = list("Unbalanced Panel is Allowed" = "TRUE",
                                                            "Unbalanced Panel is NOT Allowed" = "FALSE"),
                                             selected = "FALSE"),
                                # Step 3
                                tags$div(title="'Never Treated': control group consists of units that never participate in the treatment. \n \n 'Not Yet Treated': control group consists of units that have not yet participated in the treatment in that specific time period.",
                                         p(strong(span("[ Step 3 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                radioButtons("stagger_control", "Choose how control group is defined",
                                             choices = list("Never Treated" = "nevertreated",
                                                            "Not Yet Treated (Suggested)" = "notyettreated"),
                                             selected = "notyettreated"),
                                # Step 4
                                p(strong(span("[ Step 4 ] : ", style="color:black"))),
                                radioButtons("stagger_method", "Choose the method to compute Group-Time ATT",
                                             choices = list("Doubly Robust Approach" = "dr",
                                                            "Inverse Probability Weighting" = "ipw",
                                                            "First Step Regression Estimators" = "reg"),
                                             selected = "dr"),
                                # Step 5
                                tags$div(title=" Smallest Event Time in relative sense: For event studies, this is the smallest event time to compute dynamic effects for. It must be smaller than Base Event Time (= -1) because 'universal' base period is chosen in estimation. Leave it blank if effects at all lengths of exposure are computed; \n \n Largest Event Time in relative sense: For event studies, this is the largest event time to compute dynamic effects for. It must be larger than Base Event Time (= -1) because 'universal' base period is chosen in estimation. Leave it blank if effects at all lengths of exposure are computed",
                                         p(strong(span("[ Step 5 ] : ", style="color:black")), span(" [ Hover to see hints ] "))),
                                fluidRow(
                                  column(6,
                                         numericInput("stagger_min_e", "Enter the smallest event time",
                                                      value = "")),
                                  column(6,
                                         numericInput("stagger_max_e", "Enter the largest event time",
                                                      value = ""))
                                  ),
                                # Step 6
                                p(strong(span("[ Step 6 ] : ", style="color:black"))),
                                sliderInput("stagger_alpha", "Select alpha, the level of significance", min = 0, 
                                            max = 1, value = 0.05),
                                hr(),
                                
                                # Start and Clear Buttons
                                fluidRow(
                                  column(6,actionButton("stagger_start", "Start")),
                                  column(6,actionButton("stagger_reset", "Reset"))
                                  )
                                ),
                              
                              # Stagger Main Panel
                              mainPanel(
                                fluidRow(
                                  column(10,
                                         h4(textOutput("stagger_currentTime")),
                                         p(strong("[ WARNINGS ]"),": If you have more specifications (e.g., control variables, nonlinear constraints, instrumental variables, inclusion of treatment anticipation) on your model which cannot be set up on the left pane, please revise the produced R code accordingly and run it locally."),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Event Study Plot", 
                                                              fluidRow(
                                                                br(),
                                                                column(12,
                                                                       shinycssloaders::withSpinner(plotOutput("stagger_betaplot"))
                                                                )
                                                              )
                                                     ),
                                                     tabPanel("R Code", 
                                                              shinycssloaders::withSpinner(verbatimTextOutput("stagger_code")),
                                                              tags$head(tags$style("#stagger_code{overflow-y:scroll;overflow-x:scroll;}"))
                                                     )
                                         )
                                  ),
                                  column(2,
                                         p(strong(span("[ Downloads ]", style="color:black"))),
                                         # tags$style(".btn {width: 80%}"),
                                         downloadButton("stagger_betaplotdownload","Event Study Plot"),
                                         downloadButton("stagger_outputdownload", "Staggered ATT"),
                                         br(),
                                         br(),
                                         p(strong(span("[ Reload ]", style="color:black"))),
                                         actionButton("stagger_disconnect", "Disconnect the App")
                                  )
                                )
                              )
                            )
                   )
)


# More Panel
More <- navbarMenu("More", icon = icon("list"),
                   # Resources Tab
                   tabPanel("Resources", fluid = TRUE,
                            p(h4("About Sensitivity Analysis")),
                            tags$ul(
                              tags$li(p(a("Rambachan and Roth (2022):", href="https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf"), "A More Credible Approach to Parallel Trends.", em("Review of Economic Studies."), em("Forthcoming."))),
                              tags$li(p(a("Video:", href="https://www.youtube.com/watch?v=F8C1xaPoRvM"), "Jonathan Roth, Testing and Sensitivity Analysis for Parallel Trends. May 7, 2021.")),
                              tags$li(p(a("Replication Package:", href="https://zenodo.org/record/7178661#.Y1riNOzMKJk"), "Code for simulation results in",a("Rambachan and Roth (2022)", href="https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf"), "is also available."))
                              ),
                            p(h4("Papers Listed in Examples")),
                            tags$ul(
                              tags$li(p(a("Bailey and Goodman-Bacon (2015):", href="https://www.aeaweb.org/articles?id=10.1257/aer.20120070"), "The War on Poverty' Experiment in Public Medicine: Community Health Centers and the Mortality of Older Americans.", em("American Economic Review"), "105 (3): 1067–104.")),
                              tags$li(p(a("Benzarti and Carloni (2019):", href="https://www.aeaweb.org/articles?id=10.1257/pol.20170504"), "Who Really Benefits from Consumption Tax Cuts? Evidence from a Large VAT Reform in France.", em("American Economic Journal: Economic Policy"), "11 (1): 38–63.")),
                              tags$li(p(a("Bosch and Campos-Vazquez (2014):", href="https://www.aeaweb.org/articles?id=10.1257/pol.6.4.71"), "The Trade-Offs of Welfare Policies in Labor Markets with Informal Jobs: The Case of the ‘Seguro Popular’ Program in Mexico.", em("American Economic Journal: Economic Policy"), "6 (4): 71–99.")),
                              tags$li(p(a("Deschenes et al. (2017):", href="https://www.aeaweb.org/articles?id=10.1257/aer.20131002"), "Defensive Investments and the Demand for Air Quality: Evidence from the NOx Budget Program.", em("American Economic Review"), "107 (10): 2958–89.")),
                              tags$li(p(a("Fitzpatrick and Lovenheim (2014):", href="https://www.aeaweb.org/articles?id=10.1257/pol.6.3.120"), "Early Retirement Incentives and Student Achievement.", em("American Economic Journal: Economic Policy"), "6 (3): 120–54.")),
                              tags$li(p(a("Gallagher (2014):", href="https://www.aeaweb.org/articles?id=10.1257/app.6.3.206"), "Learning about an Infrequent Event: Evidence from Flood Insurance Take-Up in the United States.", em("American Economic Journal: Applied Economics"), "6 (3): 206–33.")),
                              tags$li(p(a("He and Wang (2017):", href="https://www.aeaweb.org/articles?id=10.1257/app.20160079"), "Do College Graduates Serving as Village Officials Help Rural China?", em("American Economic Journal: Applied Economics"), "9 (4): 186–215.")),
                              tags$li(p(a("Kuziemko et al. (2018):", href="https://www.aeaweb.org/articles?id=10.1257/pol.20150262"), "Does Managed Care Widen Infant Health Disparities? Evidence from Texas Medicaid.", em("American Economic Journal: Economic Policy"), "10 (3): 255–83.")),
                              tags$li(p(a("Lafortune et al. (2018):", href="https://www.aeaweb.org/articles?id=10.1257/app.20160567"), "School Finance Reform and the Distribution of Student Achievement.", em("American Economic Journal: Applied Economics"), "10 (2): 12–6.")),
                              tags$li(p(a("Lovenheim and Willen (2019):", href="https://www.aeaweb.org/articles?id=10.1257/pol.20170570"), "The Long-Run Effects of Teacher Collective Bargaining.", em("American Economic Journal: Economic Policy"), "11 (3): 292–324.")),
                              tags$li(p(a("Markevich and Zhuravskaya (2018):", href="https://www.aeaweb.org/articles?id=10.1257/aer.20160144"), "The Economic Effects of the Abolition of Serfdom: Evidence from the Russian Empire.", em("American Economic Review"), "108 (4-5): 1074–117.")),
                              tags$li(p(a("Tewari (2014):", href="https://www.aeaweb.org/articles?id=10.1257/app.6.4.175"), "The Distributive Impacts of Financial Development: Evidence from Mortgage Markets during US Bank Branch Deregulation.", em("American Economic Journal: Applied Economics"), "6 (4): 175–96.")),
                              tags$li(p(a("Ujhelyi (2014):", href="https://www.aeaweb.org/articles?id=10.1257/pol.6.2.338"), "Civil Service Rules and Policy Choices: Evidence from US State Govern- ments.", em("American Economic Journal: Economic Policy"), "6 (2): 338–80.")),
                              ),
                            p(h4("Dataset")),
                            tags$ul(
                              tags$li(p("Dataset containing all information needed for sensitivity analysis for papers in [ Examples ] can be downloaded from", a("here.", href="https://raw.githubusercontent.com/ccfang2/HonestDiDSenAnlys/main/data/ResultsObjectList.rds")))
                              ),
                            p(h4("Packages")),
                            tags$ul(
                              tags$li(p("Sensitivity analysis in this shiny app is built on the R package: ", a("HonestDiD.", href="https://github.com/asheshrambachan/HonestDiD"), "Its Stata version is also available", a("here.", href="https://github.com/mcaceresb/stata-honestdid"))),
                              tags$li(p("Nonstaggered DiD is analyzed with the R package: ", a("fixest.",href="https://github.com/lrberge/fixest"))),
                              tags$li(p("Staggered DiD is analyzed with the R package: ", a("did.", href="https://github.com/bcallaway11/did#difference-in-differences-")))
                              )
                            ),
                   # FAQ Tab
                   tabPanel("FAQ", fluid = TRUE,
                            p(h4("Frequently Asked Questions")),
                            hr(),
                            p(HTML("<b>[ General Questions ]</b>"), 
                               style="text-align:center"),
                            tags$ol(
                              tags$li(p("Why does the clock only show UTC time rather than my own local time?"),
                                      p(span("Unfortunately, all timestamps in shinyapps.io are UTC in order to avoid issues like the change of daylight savings. See"), a("here", href="https://community.rstudio.com/t/change-the-default-timezone-of-shinyapps-io-to-ist/2240/4"), span("for a short discussion. Alternatively, you could run this Shiny app locally on your "),a("RStudio", href="https://posit.co"), span("by following commands:"),code("library(shiny); shiny::runGitHub('HonestDiDSenAnlys', 'ccfang2')"),span(". Then, the clock will hopefully show your local time instead of UTC time."), style="font-weight: normal")),
                              tags$li(p("Why does the clock pause when the code is running?"),
                                      p(span("Don't worry. It pauses because the clock is wrapped in the same session as sensitivity analysis in the source code of shiny app. Whenever analysis is being processed, clock is suspended. However, it will tick back to the correct time when analysis is done, so you may consider the suspension of clock as a signal that the analysis is running.
                                             And obviously, the other signal of running code is a revolving spinner."), style="font-weight: normal")),
                              tags$li(p("What is the difference between buttons 'Reset' and 'Disconnect the App'?"),
                                      p(span("If you'd like to clear the outputs from an old analysis and reset all settings before starting a new one, then click 'Reset'. However, if you'd like to terminate a running analysis, then click 'Disconnect the App'."), style="font-weight: normal")),
                              tags$li(p("Could I save my plot or data file in formats other than png or xlsx?"),
                                      p(span("Unfortunately, no other format is available in order to keep the design of app neat. But, you are able to change the formats easily by using many tools available in your machine."), style="font-weight: normal")),
                              tags$li(p("Could I download plots or data files generated from old analyses?"),
                                      p(span("Unfortunately, only results from latest analysis on each tab are downloadable. Please save the results timely if needed."), style="font-weight: normal")),
                              tags$li(p("What should I do if errors occur when I use the produced R code to draw event study plot and sensitivity plot in my own R console, while all other analysis results turn out to be fine?"),
                                      p(span("Don't worry. This is a tricky error that has happened on my R console as well. To avoid this error, print out the plot directly instead of assigning it first to an object."), style="font-weight: normal")),
                              tags$li(p("Could the names of variables and titles of sheets in my data file be different from those in Example file?"),
                                      p(span("No, you are not supposed to change the names of variables or titles of sheets, since they are needed as identifiers in source code. But you are free to swap the positions of variables or sheets. Please download the Example File in each section for reference."), style="font-weight: normal")),
                              tags$li(p("Where can I access the instruction video if that one on Home page doesn't work for me?"),
                                      p(span("Please be aware that you need to stay online to watch the video on Home page. If it still doesn't work, then you can watch it on"), a("YouTube", href="https://www.youtube.com/watch?v=PyY-2ptiLTU"), "directly.", style="font-weight: normal")),
                              tags$li(p("Where can I find source code of this app?"),
                                      p(span("The source code is available from"), a("here.", href="https://github.com/ccfang2/HonestDiDSenAnlys"),span("You are also encouraged to report any issues"),a("here.", href="https://github.com/ccfang2/HonestDiDSenAnlys/issues"), style="font-weight: normal")),
                              style="font-weight: bold"
                              ),
                            br(),
                            p(HTML("<b>[ Examples ] and [ Analyze Your Own Data ]</b>"), 
                              style="text-align:center"),
                            tags$ol(
                              tags$li(p("Why does it usually take more than 2 minutes to run a sensitivity analysis for each case?"),
                                      p(span("With a careful investigation into"),a("Rambachan and Roth (2022)", href="https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf"), span("and source code of"), a("HonestDiD", href="https://github.com/asheshrambachan/HonestDiD"), span("package, you may find all inference methods including conditional approach and FLCI involve the finding of optimality in linear programming problems, 
                                             thus making the process relatively slow. The speed of estimation also depends on the size of data, chosen restrictions, your machine capability and so on. Practically speaking, analysis with only smoothness restriction works faster than the others. The more restrictions you select or the more M or Mbar you enter, the slower it will be."), style="font-weight: normal")), 
                              tags$li(p("Could I change the setup of my sensitivity analysis and click 'Start' again when analysis using old setup is still running?"),
                                      p(span("Yes, you can, and the outputs will be computed from the newest setup. But you are not suggested to do so. It would be better to click 'Disconnect the App' to terminate a running session before starting a new one."), style="font-weight: normal")),
                              tags$li(p("What is the format of indice vector that indicates the post-treatment period(s) of my interest?"),
                                      p(span("This vector of indices is used to compute the", em("l"),"vector discussed in the original paper", a("Rambachan and Roth (2022).", href="https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf"),"Here, if you are interested in a single post-treatment period, then enter the relative number of that period. For example, 1 means the first post-treatment
                                             period. If you are interested in multiple post-treatment periods, then enter the numbers with a comma in between. For example, 1,2,3 means the first three post-treatment periods. Then, the same weight will be given to the period(s) that you enter to compute", em("l"), "vector. It means that the average causal effect over those period(s) will be considered, as this is the most common parameter of interest for researchers. 
                                             If you need to give different weights to",em("l"),"vector, please revise the produced R code accordingly and run it locally. As a special case, if you are interested in average effect over all post-treatment periods, then just leave the box blank."), style="font-weight: normal")),
                              tags$li(p("What are the default values of M and Mbar at [ Step 4 ] of [ Examples ] or [ Analyze Your Own Data ]?"),
                                      p(span("By default, vector of M is a grid of length 10 that starts at M = 0 and ends at M equal to the upper bound constructed from the pre-periods using the function", code("DeltaSD_upperBound_Mpre") ,"which can be found in" ,a("HonestDiD", href="https://github.com/asheshrambachan/HonestDiD"), "package if number of pre-periods > 1 or the standard deviation of the first pre-period coefficient if number of pre-periods = 1."), style="font-weight: normal"),
                                      p(span("Whereas, by default, vector of Mbar is a grid of length 5 that starts at Mbar = 0.25 and ends at Mbar = 1.25. Please be aware that the default values of Mbar in"), a("HonestDiD", href="https://github.com/asheshrambachan/HonestDiD"), span("package is different, which has a grid of length 10 that starts at Mbar = 0 and ends at Mbar = 2."), style="font-weight: normal")),
                              tags$li(p("What should I do if the lower (upper) bounds of confidence set turn out to be exactly the same for different Mbars?"),
                                      p(span("This is due to the fact that the lower (upper) bound of grid used for underlying test inversion is too large (small) in your setup. You need to decrease (increase) it to get correct confidence sets. By default, the upper bound of grid used for underlying test inversion is equal to twenty times the standard deviation of the estimated target parameter (i.e., weight vector", code("%*%") ,"post-treatment event-study coefficient vector). Whereas, lower bound of grid is equal to negative twenty times the standard deviation of the estimated target parameter."), style="font-weight: normal")),
                              tags$li(p("What does [Step 7] of [ Examples ] or [ Analyze Your Own Data ] indicate?"),
                                      p(span("This is a logical value to indicate whether you would like to construct the robust confidence intervals in parallel. This uses the", a("Foreach",href="https://github.com/cran/foreach"), "package and", a("doParallel",href="https://github.com/cran/doParallel"), "package. You are highly suggested to select 'No' as given by default to enhance computational efficiency."), style="font-weight: normal")),
                              tags$li(p("Where can I download the data file specifically for the paper I select at section of [ Examples ]?"),
                                      p(span("You can download the data for your selected paper by clicking the button of 'Data File' once the analysis is done. But you can always download data files for all papers in [ Examples ] from", a("here.", href="https://raw.githubusercontent.com/ccfang2/HonestDiDSenAnlys/main/data/ResultsObjectList.rds")), style="font-weight: normal")),
                              style="font-weight: bold"
                              ),
                            br(),
                            p(HTML("<b>[ Quick Help ]</b>"), 
                              style="text-align:center"),
                            tags$ol(
                              tags$li(p("What does 'family' at [ Step 2 ] of [Non-Staggered DiD] mean?"),
                                      p(span("Generally, it indicates the distribution of error term. GLM or MLE with gaussian family gives out the same estimation as OLS. You need to select a suitable estimation model and family to fit your own data."), style="font-weight: normal")),
                              tags$li(p("Could I define 'link function' for 'family' I select at [ Step 2 ] of [Non-Staggered DiD]?"),
                                      p(span("Unfortunately, selection of 'link function' is not available to make the design of this app neat and clear. However, you can always copy and paste the produced R code, revise it accordingly and run it locally. This app uses default link to each family. For GLM, by default, link function of binomial family is logit; gaussian, identity link; gamma, inverse link; inverse gaussian, 1/mu^2 link; poisson, log link. For MLE, by default, link function of poisson family is log; negative binomial, log link; logit, log link; gaussian, identity link."), style="font-weight: normal")),
                                      # p(span("For GLM, by default, link function of binomial family is logit; gaussian, identity link; gamma, inverse link; inverse gaussian, 1/mu^2 link; poisson, log link."), style="font-weight: normal"),
                                      # p(span("For MLE, by default, link function of poisson family is log; negative binomial, log link; logit, log link; gaussian, identity link."), style="font-weight: normal")),
                              style="font-weight: bold"
                              ),
                            hr()
                            )
)
