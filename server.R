#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Required Libraries 
require(shiny)
library(shiny)
library(shinydashboard)

library(haven)
library(labelled)

library(dplyr)
library(readr)
library(plotly)

library(DT)

library(shinyWidgets)


#Solving masking issues

library(conflicted)
conflict_prefer("box", "shinydashboard", "graphics")
conflict_prefer("dataTableOutput", "DT", "shiny")
conflict_prefer("renderDataTable", "DT", "shiny")
conflict_prefer("last_plot", "plotly", "ggplot2")
conflict_prefer("filter", "plotly", "stats")
conflict_prefer("layout", "plotly", "graphics")

#I use two different datasets:
#One of them takes only labels for DT and some outputs in summary.
#Other one (dta) is for some outputs in summary.

#Factorized data
df <- read.csv("finaldata.csv")

data <- reactive(
  {df}
)

#Original data is being used in summary
df_dta <- read_dta("finaldata.dta")

data_dta <- reactive(
  {df_dta}
)

#I needed to do this here for some reason.

#year assigned as factor

#For categorical variables
catCols <- c(1, 3, 5, 6, 7, 8, 9, 10, 11, 13, 14)

for (i in catCols) {
  df[, i] <- as.factor(df[, i])
}

#For numeric Variables
numCols <- c(2, 4, 12, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26)

for (i in numCols) {
  df[, i] <- as.numeric(df[, i])
}


shinyServer(
  function(input, output, session) {
    
    #VIEW DATA page
    
    #Filter by year
    output$picker_year <- renderUI({
      
      selectizeInput(inputId = 'pick_year', 
                     label = 'Filter years:', 
                     choices = data()$year %>% unique() %>% sort(),
                     options = list(placeholder = 'All years'
                                   # ,onChange = event("ev_click")
                                    ),
                     multiple = TRUE)
      
      
    })
    
    #Select variable(s)
    output$picker_variable <- renderUI({
      
      selectizeInput(inputId = 'pick_variable', 
                  label = 'Select variables:', 
                  choices = colnames(data()),
                  options = list(placeholder = 'Search varibles, No selection yet!'
                                # ,onChange = event("ev_click")
                                 ),
                  multiple = TRUE)
      
      
    })
    
    
    datasetInput <- eventReactive(input$view,{
      
      data () %>%
        #If year is not selected, this will provide all years
        dplyr::filter( if(length(input$pick_year)==0) T else year %in% input$pick_year) %>%
        select(input$pick_variable)
      
    })
    
    #Data Table
    output$dt <- DT::renderDataTable({
      datasetInput() %>%
        datatable(
          filter = "top",
          rownames = TRUE,
          #selection = "none",
          selection = list(which = 'column'),
          options = list(pageLength = 5, scrollX = TRUE),
          callback=JS("table.on( 'order.dt search.dt', function () {
                                table.column(0, {search:'applied', order:'applied'}).nodes().each( function (cell, i) {
                                      cell.innerHTML = i+1;});}).draw();")
        )
    })
    
    
    # Downloadable csv of selected columns and filtered rows (built-in) dataset 
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("filtered_data.csv", sep="")
      },
      content = function(file) {
        write.csv(datasetInput()[input[["dt_rows_all"]], ], file, row.names = TRUE
                  )
      }
    )
    
    
    # TAB: Summary of a single variable
    
    #Select a variable
    output$picker_variable2 <- renderUI({
      
      selectizeInput(inputId = 'pick_variable2', 
                     label = '', 
                     choices = colnames(data()),
                     options = list( placeholder = 'No selection yet!'
                                    , maxItems = 1
                                    # ,onChange = event("ev_click")
                     ),
                     multiple = TRUE)
      
      
    })
  
    
    
    #Description of Selected Variable
    output$info_box <- renderText({
      
      if (length(input$pick_variable2) == 0) {
        return(NULL) #"Variable not selected yet!"
      } else {
      unlist(var_label(data_dta()[,input$pick_variable2])
              )
      }
    })
    
    #Values and Labels
    output$info_box2 <- renderText({
      if (length(input$pick_variable2) == 0) {
        "Variable not selected yet!"
      } else {
        unlist(val_labels(data_dta()[,input$pick_variable2]))
      }
    })
    
    #Summary
    output$summary <- renderUI({
      selected_variable <- data()[,input$pick_variable2]
      
      if (length(input$pick_variable2) == 0) {
        df_sum = NULL
      } else if (sum(!is.na(selected_variable)) == 0) {
        df_sum = NULL
        # numeric variable
      } else if (!is.factor(selected_variable[[1]])) {
        Metric <- as.matrix(attributes(summary(selected_variable))$names)
        Value <- as.matrix(summary(selected_variable))
        df_sum <- data.frame(Metric, Value)
        #categorical variable
      } else {
        levels <- levels(selected_variable)
        count <- table(selected_variable) # Create contingency table
        proportion <- round(prop.table(count),2) # Create proportion table
        percentage <- round(proportion * 100,2) # Create percentage table
        
        df_sum <- data_frame(Categories = levels, 
                             Count = count, 
                             Proportion = proportion, 
                             Percentage = percentage)
  
      }
      
      renderTable({
        df_sum
      })
    })
    
    output$NAs <- renderText({
      selected_variable <- data()[,input$pick_variable2]
      
      sum(is.na(selected_variable))
      if (length(input$pick_variable2) == 0) {
        return(NULL)
      } else if (!is.factor(selected_variable[[1]])) {
        return(NULL)
      } else {
        paste0("Number of NAs: ", sum(is.na(selected_variable)))
      }
    })
      
    
    #Visualization
    
    #Select Chart Type
    
    #Select a variable
    output$chart_type <- renderUI({

      selectizeInput(inputId = 'chart_type', 
                     label = "Plot Type:", 
                     choices = list(BarPlot = "BarPlot",
                                    PieChart = "PieChart"
                     ),
                     options = list( placeholder = 'No selection yet!'
                                     , maxItems = 1
                                      ),
                     multiple = TRUE)
      
      
    })
    
    
    
    
    output$visualization <- renderPlotly({
      
      if (req(input$pick_variable2) == '') {
        return(NULL)
      } 
      
      if (req(input$chart_type) == '') {
        return(NULL)
      } 
      
      
      #Barplot, count, colored, vertical
      b1 <- plot_ly(data(),
                    x = ~get(input$pick_variable2),
                    color = ~get(input$pick_variable2),
                    opacity = 0.95
      ) %>%
        add_histogram() %>%
        plotly::layout(
          #title = list(text = "Counts by Levels", xref = "paper"),
          xaxis = list(title = ~get(input$pick_variable2) ),
          yaxis = list(title = "Count"),
          legend = list(title = list(text = "Levels")),
          showlegend = TRUE)
      
      df_pie <- as.data.frame(table(data()[,input$pick_variable2]))
      colnames(df_pie)[1] <- "level"
      colnames(df_pie)[2] <- "count"
      
      #Pie, percentage, color
      p1 <- plot_ly(df_pie, labels = ~level, values = ~count, type = 'pie') %>%
        layout(title = 'Percentage',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      
      if (length(input$pick_variable2) == 0) {
      return(NULL)
    } else if (
      input$chart_type =="BarPlot" 
      # &
      # input$show_percentage == FALSE &
      # input$colored == "Color" &
      # input$orientation == "Vertical"
    ) {
      b1
    } else if (
      input$chart_type == "PieChart" 
      # &
      # input$show_percentage == TRUE &
      # input$colored == "Color"
    ) {
      p1
    } else {
      #?
    }
      
    })
    
    
    #Select a variable
    # output$picker_variable3 <- renderUI({
    #   
    #   selectizeInput(inputId = 'pick_variable3', 
    #                  label = '', 
    #                  choices = colnames(data()),
    #                  options = list( placeholder = 'No selection yet!'
    #                                  , maxItems = 1
    #                                  # ,onChange = event("ev_click")
    #                  ),
    #                  multiple = TRUE)
    #   
    #   
    # })
    
    # #Select a variable
    # output$picker_variable4 <- renderUI({
    #   
    #   selectizeInput(inputId = 'pick_variable4', 
    #                  label = '', 
    #                  choices = colnames(data()),
    #                  options = list( placeholder = 'No selection yet!'
    #                                  , maxItems = 1
    #                                  # ,onChange = event("ev_click")
    #                  ),
    #                  multiple = TRUE)
    #   
    #   
    # })
    
  }
)

