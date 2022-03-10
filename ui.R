library(shiny)
library(tidyverse)

rebalance_ui <- 
  navbarPage("Schwab Portfolio Rebalancer",
             tabPanel(
               "1. Instructions",
               fluidRow(column(
                 br(),
                 p("This application visualizes planned changes in a Charles Schwab portfolio and assists in the arithmatic needed to manually rebalance assets.",style="text-align:justify;color:black;background-color:#bbdbeb;padding:15px;border-radius:10px"),
                 # br(),
                 
                 p("To export the data necessary to use this tool, navigate to the ",
                   a("Accounts > Positions", href="https://client.schwab.com/Areas/Accounts/Positions"), 
                   " screen on the Charles Schwab client site and select the Export option located in the upper-right hand corner of the page. The image to the right is a screenshot of what should subsequently display. The downloaded file can then be uploaded to the application using the Import tab.",
                   style="text-align:justify;color:black;background-color:#bbdbeb;padding:15px;border-radius:10px"),
                 width=5),
                 column(
                   imageOutput("image", width = "10%"),
                   width=5))
             ),
             tabPanel("2. Import",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          tags$hr(),
                          numericInput(
                            "nskip",
                            "Skip rows",
                            1,
                            min = 0,
                            max = 10,
                            step = 1
                          ),
                          radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          checkboxInput("view_og", 'View original file', F)
                        ),
                        mainPanel(
                          tableOutput("contents")
                        )
                      )
             ),
             tabPanel("3. Allocate",
                      sidebarLayout(
                        sidebarPanel(
                          textInput('input_ticker','Add additional asset ticker',''),
                          actionButton('add_ticker','Update ticker list'),
                          br(),br(),
                          uiOutput("sliders")
                        ),
                        mainPanel(
                          br(),
                          plotOutput("plot_comparison")
                        )
                      )
             ),
             tabPanel("4. Trades",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput(
                            "threshold_option",
                            "Avoid small trades?",
                            T
                          ),
                          numericInput(
                            "threshold_dollar",
                            "Avoid trades below $",
                            25,
                            min = 0,
                            max = 125,
                            step = 5
                          ),
                          br(),br(),
                          tableOutput("report_allocation")
                        ),
                        mainPanel(
                          tableOutput("trades")#,
                          # "Data current as of ",textOutput("datetime")
                        )
                      )
             )
  )