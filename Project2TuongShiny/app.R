#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#install.packages("shiny")
#install.packages("shinyalert")
#install.packages("shinydashboard")
#install.packages("tidyverse")
#install.packages("bslib")
#install.packages("ggplot2")
#install.packages("stringr")
#install.packages("DT")

# library(shiny)
library(shinyalert)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(ggplot2)
library(stringr)
library(DT)

source("Project2Tuong.qmd")

# Time for application creation. I will be using the base app.R template and a bit of the template from my first Shiny App project to reference.

# Define UI for application that allows for subsetting 
ui <- fluidPage(

    # Application title
    titlePanel("Mobile Device Exploration"),

    # Created side bar
    sidebarLayout(
        sidebarPanel(
          
          # Created header for selecting categorical variables
          h3("Select Categorical Variables \n to Subset the Data:"),
            
            # Created selecticized inputs here w/ internal IDs for categorical variables
            selectizeInput(inputId = "operating_system",
                           label = "Select Operating System:",
                           choices = c("All", mobdevdata$Operating.System),
                           selected = "All",
                           options = NULL,
                           width = NULL),
            selectizeInput(inputId = "gend",
                           label = "Select Gender:",
                           choices = c("All", mobdevdata$Gender),
                           selected = "All",
                           options = NULL,
                           width = NULL),
          
          # Created header for selecting the first numeric variable
          h3("Select Your Numeric Variables"),  
            
            # Created selecticized inputs w/ internal IDs for numeric variables - use corr_n method to reference the index*
            selectizeInput(inputId = "num_var1",
                           label = "Select your first numeric variable (x-axis):",
                           choices = numeric_vars,
                           options = NULL,
                           width = NULL),
            
            sliderInput(inputId = "value_range1",
                        label = NULL,
                        min = 0,
                        max = 3000,
                        value = c(0, 3000)),
          
          # Created selecticized inputs w/ internal IDs for numeric variables and their sliders
          selectizeInput(inputId = "num_var2",
                         label = "Select your second numeric variable (y-axis):",
                         choices = numeric_vars,
                         options = NULL,
                         width = NULL),
          
          sliderInput(inputId = "value_range2",
                      label = NULL,
                      min = 0,
                      max = 3000,
                      value = c(0, 3000)),
          
          # Created action button to update the analysis based on subset
          actionButton(inputId = "update",
                       label = "Update Analysis")
        ),

        # Created main panel and other panels
        mainPanel(
          tabsetPanel(
            tabPanel("About",
                     h2("Mobile Device Usage Analyzation"),
                     p("The purpose of this application is to observe mobile device usage across different variables to analyze mobile device behavior."),
                     h3("Available Data to Choose from:"),
                     p("- You are able to subset information about mobile device usage including device information, usage metrics, user demographics, and user behavioral classifications."),
                     p("- You can find the following mobile device usage data set by coping and pasting this link in your: https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset?resource=download."),
                     h3("Notes:"),
                     p("Data Download: this tab at the top allows you view the subsetted data and download it as a .csv file."),
                     p("Data Exploration: this tab at the top  allows you to observe numerical and graphical summaries of your subsetted data."),
                     p("Reminder: Update Analysis must be clicked for every new sidebar subset.")
                     ),
            tabPanel("Data Download",
                     DT::dataTableOutput("data_table"),
                     downloadButton(outputId = "download_data",
                                    label = "Download Data")),
            tabPanel("Data Exploration", 
                     h3("Explore your subsetted data!"),
                     tabsetPanel(
                       tabPanel("One-Way Contingency Table",  
                                p("Reminder: Update Analysis must be clicked for every new sidebar subset."),
                                # Select input to choose categorical variable for one-way table
                                selectizeInput(inputId = "cat_var_one_way",  
                                               label = "Select Categorical Variable for One-Way Table:",
                                               choices = names(category_vars),  
                                               options = NULL,
                                               width = NULL),
                                tableOutput("one_way_table")), 
                       tabPanel("Two-Way Contingency Table",
                                p("Reminder: Update Analysis must be clicked for every new sidebar subset."),
                                tableOutput("two_way_table")),
                       tabPanel("Numerical Summary by Category(s)",
                                p("Reminder: Update Analysis must be clicked for every new sidebar subset."),
                                tableOutput("num_sum_by_cat")),
                       tabPanel("Bar Graph Plot",
                                p("Reminder: Update Analysis must be clicked for every new sidebar subset."),
                                tableOutput("bar_plot"))
                       )
                                
                     )
              )
          )
        )
    )

    

# Define the server for inputs and outputs
# Steps: Inputs to subset and filter (reactive data table), Outputs (rendering data table, download .csv, )
server <- function(input, output, session) {
  
# Inputs  

    # Updating input boxes so the same numeric variable cannot be chosen
    observeEvent(c(input$num_var1, input$num_var2), {
      num_var1 <- input$num_var1
      num_var2 <- input$num_var2
      choices <- numeric_vars
      if (num_var1 == num_var2) {
        choices <- choices[-which(choices == num_var1)]
        updateSelectizeInput(session,
                             "num_var2",
                             choices = choices)
      }
    })
  
    # Creating subsetted/filtered data for numeric variable selection
    # Using a eventReactive() to ensure that action must be clicked for each new subset
    filtered_data <- eventReactive(input$update, {
      subsetted_data <- mobdevdata 
      if (input$operating_system != "All") {
        subsetted_data <- subsetted_data[subsetted_data$Operating.System == input$operating_system, ]
      }
      if (input$gend != "All") {
        subsetted_data <- subsetted_data[subsetted_data$Gender == input$gend, ]
      }
      if (input$num_var1 %in% colnames(subsetted_data)) {
        subsetted_data <- subsetted_data[subsetted_data[input$num_var1] >= input$value_range1[1] & subsetted_data[input$num_var1] <= input$value_range1[2], ]
      }
      if (input$num_var2 %in% colnames(subsetted_data)) {
        subsetted_data <- subsetted_data[subsetted_data[input$num_var2] >= input$value_range1[1] & subsetted_data[input$num_var2] <= input$value_range2[2], ]
      }      
      subsetted_data
    })
    
    # Created a filter for contingency tables to only be able to be subsetted by categorical variables
    filtered_categorical_data <- eventReactive(input$update, {
      subsetted_data <- mobdevdata 
      if (input$operating_system != "All") {
        subsetted_data <- subsetted_data[subsetted_data$Operating.System == input$operating_system, ]
      }
      if (input$gend != "All") {
        subsetted_data <- subsetted_data[subsetted_data$Gender == input$gend, ]
      }
      subsetted_data
    })

# OutPuts
        
    # Rendering the data table for tab 2 from filtered_data subset above
    output$data_table <- DT::renderDataTable({
      filtered_data()
    })
  
    # Download handler for the filtered data as a csv. file. for button in tab 2
    output$download_data <- downloadHandler(
      filename = function() {
        paste('filtered_data', Sys.Date(), '.csv', sep ='')
      },
      content = function(file) {
        write.csv(filtered_data(), file)
      }
    )
    

    # One way table
    output$one_way_table <- renderTable({
      
      # Ensuring that Update must be clicked before obtaining a one way table
      validate(
        need(input$update, "Please click Update Analysis in order obtain a one way table.")
      )
      one_way_table(filtered_categorical_data(), category_vars[input$cat_var_one_way])
    })
    
    # Two way table  
    output$two_way_table <- renderTable({
      
      validate(
        need(input$update, "Please click Update Analysis in order obtain a two way table.")
      )
      
      # Call the two_way_table function defined in the qmd
      two_way_table(filtered_categorical_data(), category_vars[["Operating System"]], category_vars[["Genders"]])
    })
    
    # Numerical Summary by Category
    output$num_sum_by_cat <- renderTable({
      
      validate(
        need(input$update, "Please click Update Analysis in order obtain your numerical summary.")
      )
      
      num_sum_by_cat(filtered_data(), c(input$num_var1, input$num_var2), c(category_vars["Operating System"], category_vars["Genders"]))
    })
    
    # Bar Plot Output
    output$bar_plot <- renderPlot({
      bar_plot(filtered_data(), combined_var[input$combined_vars], combined_var[input$combined_vars])
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)


 