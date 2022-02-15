#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(googlesheets4)

gs4_auth(email = "")

mil = range_read("1m45w0hQOkvUFvNpc5MwQcN4snGs0Lkrb6kJfOUJf6tw"
                 ,sheet = 'master_item_list'
                 ,col_types = 'cccccccncD')

mil_max = mil %>% 
  summarise(max = max(`Snapshot Date`)) %>% 
  pull

milc = mil %>% filter(`Snapshot Date` == mil_max) %>% filter(Category %in% c('Produce - Vegetables','Produce - Fruits')) %>% arrange(Category)

item_list = c('carrots','potatoes - sweet','radishes - purple heart')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Fair Shares - Farmer Produce Availability - Master"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("item","Item*",milc$Item),  #item_list
            actionButton("update", "Update Table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("itemtable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    values <- reactiveValues()
    values$df <- data.frame(Column1 = NA, Column2 = NA)
    newEntry <- observe({
      if(input$update > 0) {
        newLine <- isolate(c(input$item, NA))
        isolate(values$df <- rbind(values$df, newLine) %>% filter(!is.na(Column1)))
      }
    })
    output$itemtable <- renderTable({values$df})
}

# Run the application 
shinyApp(ui = ui, server = server)
