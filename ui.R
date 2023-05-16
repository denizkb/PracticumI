#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(plotly)

library(conflicted)
conflict_prefer("box", "shinydashboard", "graphics")
conflict_prefer("dataTableOutput", "DT", "shiny")
conflict_prefer("renderDataTable", "DT", "shiny")
conflict_prefer("last_plot", "plotly", "ggplot2")
conflict_prefer("filter", "plotly", "stats")
conflict_prefer("layout", "plotly", "graphics")

dashboardPage(

  dashboardHeader(disable=T),
  
  dashboardSidebar(disable = T),
  
  
  
  dashboardBody(
    navbarPage( title = "GSS 72-21 DATA",
                tabPanel("View Data",icon = icon("table"),
                      fluidRow(
                        shinydashboard::box(width = 3,
                            title = "Make Selections",
                            status = "primary",
                            solidHeader = T,
                            br(),
                            uiOutput("picker_year"),
                            br(),
                            uiOutput("picker_variable"),
                            br(),
                            actionButton("view", "View Selection",
                                             style = "
                                              color: #fff;
                                               background-color: #337ab7;
                                               border-color: #fff;
                                               padding: 5px 14px 5px 14px;
                                               margin: 10px 10px 10px 85px; "
                                         ),
                            br(),
                            downloadButton("downloadData", "Download Filtered Data",
                                               style = "
                                               color: #fff;
                                               background-color: #337ab7;
                                               border-color: #fff;
                                               padding: 5px 14px 5px 14px;
                                               margin: 10px 10px 10px 50px; "
                                           )
                            ),
                        shinydashboard::box(width = 9,
                            title = "Filtered Data",
                            status = "primary",
                            solidHeader = T,
                            DT::dataTableOutput("dt")
                            )
                      )
                ),
                
                tabPanel("Summary of a Single Variable", icon = icon("list-alt"),
                         fluidPage(
                           titlePanel("Descriptive Statistics"),
                           fluidRow(
                             shinydashboard::box(width = 6,
                                 title = "Select a single variable: ",
                                 status = "primary",
                                 solidHeader = T,
                                 height = 130,
                                 uiOutput("picker_variable2")),
                             shinydashboard::box(width = 6,
                                 title = "Description of Selected Variable:",
                                 status = "success",
                                 solidHeader = T,
                                 height = 130,
                                 br(),
                                 textOutput("info_box")
                             )),
                           fluidRow(
                             shinydashboard::box(width = 6,
                                                 title = "Values and Labels:",
                                                 status = "warning",
                                                 solidHeader = T,
                                                 height = 400,
                                                 verbatimTextOutput("info_box2")),
                             shinydashboard::box(width = 6,
                                                 title = "Summary:",
                                                 status = "danger",
                                                 solidHeader = T,
                                                 height = 400,
                                                 uiOutput("summary"),
                                                 br(),
                                                 textOutput("NAs")
                                              
                             )
                             
       
                         )
                         ),
                         fluidPage(
                           titlePanel("Visualization"),
                           fluidRow(
                             column(width = 4,
                                    verticalLayout(
                                  box(title = "Chart Options: ",
                                      width = 12,
                                      status = "primary",
                                      solidHeader = T,
                                      uiOutput("chart_type")
                                      )
                                  # ,
                                  # box(title = "General Chart Options: ",
                                  #     status = "primary",
                                  #     width = 12,
                                  #     solidHeader = T,
                                  #     checkboxInput("show_percentage", "Show percentages", FALSE),
                                  #     radioButtons(
                                  #       inputId = "colored",
                                  #       label = "Color", 
                                  #       choices = c("Color", "Grayscale"),
                                  #       selected = "Color"
                                  #     )
                                  #     ),
                                  # conditionalPanel(
                                  #   condition = "input.chart_type == 'BarPlot'",
                                  #   box(title = "Bar Chart Options: ",
                                  #       status = "warning",
                                  #       width = 12,
                                  #       solidHeader = T,
                                  #       radioButtons(
                                  #         inputId = "orientation",
                                  #         label = "Orientation", 
                                  #         choices = c("Vertical", "Horizontal"),
                                  #         selected = character(0)
                                  #       ))
                                  # )
                                  )),
                             column(width = 8,
                                    box(title = "Visualization: ",
                                        status = "info",
                                        width = 12,
                                        solidHeader = T,
                                        plotlyOutput("visualization"),
                                        p("*Plots exclude missing data.",
                                          style = "font-size:16px"
                                        )
                                        ))
                         
                )
                
                )
                      
                         
                         ),
                # tabPanel("Summary of Two Variables", icon = icon("list-alt"),
                #          fluidRow(
                #            tags$head(
                #              tags$style(HTML(" #comparison {
                #              display: flex;
                #              justify-content: center;
                #                              }"
                #              ))
                #            ),
                #            box(title = "Make Selection: ",
                #                status = "primary",
                #                width = 12,
                #                solidHeader = T,
                #                prettyRadioButtons(
                #                  inputId = "comparison",
                #                  label = "",
                #                  inline = TRUE,
                #                  choices = c("Relationship between Two Categorical Variables",
                #                              "Relationship between Two Numeric Variables",
                #                              "Relationship between Numeric and Categorical Variables"),
                #                  selected = character(0),
                #               fill = TRUE
                #                )
                #            )
                #          ),
                #          fluidRow(
                #            shinydashboard::box(width = 6,
                #                                title = "Select a variable for row:",
                #                                status = "danger",
                #                                solidHeader = T,
                #                                #height = 400,
                #                                uiOutput("picker_variable3")
                #                                ),
                #            shinydashboard::box(width = 6,
                #                                title = "Select a variable for column:",
                #                                status = "warning",
                #                                solidHeader = T,
                #                                #height = 400,
                #                                uiOutput("picker_variable4")
                #                                )
                #            ),
                #          fluidRow(
                #            
                #          )
                #          ),
                tabPanel("About", 
                         icon = icon("info"),
                         fluidRow(
                            
                                  p("This project was created by Deniz Kardes Birinci under the supervision of Prof. Bernard Klingenberg.",
                                    style = "font-size:25px"
                                  ),
                                  p("e-mail:", style = "font-size:20px"),
                                  p("bklingenberg@ncf.edu", style = "font-size:20px"),
                                  p("deniz.kardesbirinc23@ncf.edu",
                                    style = "font-size:20px"
                                  )
                           
                         )
                         )
                
                
    )
  )

 )



