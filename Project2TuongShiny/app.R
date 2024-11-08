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

library(shiny)
library(shinyalert)
library(shinydashboard)
library(tidyverse)
library(bslib)
library(ggplot2)
library(stringr)

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
            selectizeInput(inputId = "user_behavior_class",
                           label = "Select User Behavior Classification:",
                           choices = c("All", mobdevdata$User.Behavior.Class),
                           options = NULL,
                           width = NULL),
          
          # Created header for selecting the first numeric variable
          h2("Select Your First Numeric Variable"),  
            
            # Created selecticized inputs w/ internal IDs for numeric variables - use corr_n method to reference the index*
            selectizeInput(inputId = "num_var1",
                           label = "Select your first numeric variable:",
                           choices = c("Screen.on.Time..hours.day.", "Battery.Drain..mAh.day.", "Data.Usage..MB.day."),
                           options = NULL,
                           width = NULL),
            
            sliderInput(inputId = "screen_time",
                        label = c("Choose amount of Battery Drain \n (mAh per day):"),
                        min = 300,
                        max = 3000,
                        value = 30),
          
          # Created header for selecting the second numeric variable
          h2("Select Your Second Numeric Variable"),
          
          # Created selecticized inputs w/ internal IDs for numeric variables - use corr_n method to reference the index*
          selectizeInput(inputId = "num_var1",
                         label = "Select your first numeric variable:",
                         choices = c("Screen.on.Time..hours.day.", "Battery.Drain..mAh.day.", "Data.Usage..MB.day."),
                         options = NULL,
                         width = NULL),
          
          sliderInput(inputId = "screen_time",
                      label = c("Choose amount of Battery Drain \n (mAh per day):"),
                      min = 300,
                      max = 3000,
                      value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("About",
                     h3("Mobile Device Usage Analyzation"),
                     p()),
            tabPanel("Data Download", "placeholder"),
            tabPanel("Data Exploration", "placeholder")
          ), 
          plotOutput("distPlot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

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
