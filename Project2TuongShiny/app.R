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
# <- install.packages("DT")

library(shiny)
library(shinyalert)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(ggplot2)
library(stringr)
library(DT)

# Time for application creation. I will be using the base app.R template and a bit of the template from my first Shiny App project to reference.

# Define UI for application that allows for subsetting
ui <- fluidPage(

    # Application title
    titlePanel("Mobile Device Exploration"),

    # Created side bar
    sidebarLayout(
        sidebarPanel(
          
          # Created header for selecting categorical variables
          h2("Select Categorical Variables \n to Subset the Data:"),
            
            # Created selecticized inputs here w/ internal IDs for categorical variables
            selectizeInput(inputId = "device_model",
                           label = "Select Device Model:",
                           choices = c("All", mobdevdata$Device.Model),
                           options = NULL,
                           width = NULL),
            selectizeInput(inputId = "operating_system",
                           label = "Select Operating System:",
                           choices = c("All", mobdevdata$Operating.System),
                           options = NULL,
                           width = NULL),
            selectizeInput(inputId = "gend",
                           label = "Select Gender:",
                           choices = c("All", mobdevdata$Gender),
                           options = NULL,
                           width = NULL),
          
          # Created header for selecting the first numeric variable
          h2("Select Your First Numeric Variable"),  
            
            # Created selecticized inputs w/ internal IDs for numeric variables - use corr_n method to reference the index*
            selectizeInput(inputId = "num_var1",
                           label = "Select your first numeric variable:",
                           choices = numeric_vars,
                           options = NULL,
                           width = NULL),
            
            sliderInput(inputId = "value_range1",
                        label = NULL,
                        min = 0,
                        max = 3000,
                        value = 30),
          
          # Created header for selecting the second numeric variable
          h2("Select Your Second Numeric Variable"),
          
          # Created selecticized inputs w/ internal IDs for numeric variables - use corr_n method to reference the index*
          selectizeInput(inputId = "num_var2",
                         label = "Select your first numeric variable:",
                         choices = numeric_vars,
                         options = NULL,
                         width = NULL),
          
          sliderInput(inputId = "value_range2",
                      label = NULL,
                      min = 0,
                      max = 3000,
                      value = 30),
          
          # Created action button to update the analysis based on subset
          actionButton(inputId = "update",
                       label = "Update Analysis")
        ),

        # Created main panel
        mainPanel(
          tabsetPanel(
            tabPanel("About",
                     h2("Mobile Device Usage Analyzation"),
                     p("The purpose of this application is to observe mobile device usage across different variables to analyze mobile device behavior."),
                     h3("Available Data to Choose from:"),
                     p("- You are able to subset information about mobile device usage including device information, usage metrics, user demographics, and user behavioral classifications."),
                     p("- You can find the following mobile device usage data set by coping and pasting this link in your: https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset?resource=download."),
                     h3("Notes:"),
                     p("Data Download: this tab at the top allows you view the subsetted data and download it asf a .csv file."),
                     p("Data Exploration: this tab at the top allows you to observe numerical and graphical summaries of your subsetted data.")
                     ),
            tabPanel("Data Download",
                     DT::dataTableOutput("data_table"),
                     downloadButton(outputId = "download_data",
                                    label = "Download Data")),
            tabPanel("Data Exploration", 
                     h3("Explore your subsetted data!"),
                     plotOutput("scatterplot"))
          )
        )
    )
)

# Define the server for inputs and outputs
# Steps: Inputs to subset and filter (reactive data table), Outputs (rendering data table, download .csv, )
server <- function(input, output, session) {
  
    # Input - Creating subsetted/filtered data for numeric variable selection


  
    # OutPut - Download handler for the filtered data as a csv. file. for button in tab 2
    output$download_data <- downloadHandler(
      filename = function() {
        paste('data', Sys.Date(), '.csv', sep ='')
      },
      content = function(con) {
        write.csv(data, con)
      }
    )
    
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
